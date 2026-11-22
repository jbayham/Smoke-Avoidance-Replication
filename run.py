#!/usr/bin/env python3
"""Portable entry point for Smoke Avoidance. Python >=3.10; standard library only."""
import argparse
import csv
import datetime as dt
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parent

def read_json(path):
    return json.loads(Path(path).read_text(encoding='utf-8'))

def write_json(path, value):
    path=Path(path);path.parent.mkdir(parents=True,exist_ok=True)
    tmp=path.with_suffix(path.suffix+'.tmp')
    tmp.write_text(json.dumps(value,indent=2)+'\n',encoding='utf-8');os.replace(tmp,path)

def now():return dt.datetime.now(dt.timezone.utc).isoformat(timespec='seconds')

def settings(root=ROOT):
    c={'data_dir':'data','output_dir':'output','rscript':'Rscript','threads':4}
    f=root/'config/local.json'
    if f.exists():c.update(read_json(f))
    for k in ['data_dir','output_dir']:
        p=Path(c[k]).expanduser();c[k]=(root/p).resolve() if not p.is_absolute() else p.resolve()
    if c['data_dir']==c['output_dir'] or c['data_dir'] in c['output_dir'].parents or c['output_dir'] in c['data_dir'].parents:
        raise ValueError('data_dir and output_dir must be separate, non-nested directories')
    if not isinstance(c['threads'],int) or c['threads']<1:raise ValueError('threads must be a positive integer')
    return c

def resolve(p,c,root=ROOT):
    if p.startswith('data/'):return c['data_dir']/p[5:]
    if p.startswith('output/'):return c['output_dir']/p[7:]
    return root/p

def digest(path,cache=None):
    path=Path(path)
    if not path.exists():raise FileNotFoundError(path)
    files=sorted(p for p in path.rglob('*') if p.is_file()) if path.is_dir() else [path]
    if not files:raise ValueError(f'Empty input/output: {path}')
    h=hashlib.sha256()
    for f in files:
        stat=f.stat();key=(str(f),stat.st_size,stat.st_mtime_ns)
        value=cache.get(key) if cache is not None else None
        if value is None:
            fh=hashlib.sha256()
            with f.open('rb') as stream:
                for b in iter(lambda:stream.read(8*1024*1024),b''):fh.update(b)
            value=fh.hexdigest()
            if cache is not None:cache[key]=value
        h.update((str(f.relative_to(path)) if path.is_dir() else f.name).encode());h.update(value.encode())
    return h.hexdigest()

def ordered(stages):
    index={s['name']:s for s in stages}
    if len(index)!=len(stages):raise ValueError('Duplicate stage names')
    seen=set();visiting=set();result=[]
    def visit(name):
        if name in visiting:raise ValueError(f'Cyclic dependency: {name}')
        if name not in index:raise ValueError(f'Unknown dependency: {name}')
        if name in seen:return
        visiting.add(name)
        for d in index[name]['deps']:visit(d)
        visiting.remove(name);seen.add(name);result.append(index[name])
    for name in index:visit(name)
    return result

def selected(stages,command,stage=None):
    stages=ordered(stages)
    if stage:
        index={s['name']:s for s in stages};needed=set()
        def include(n):
            if n not in index:raise ValueError(f'Unknown stage: {n}')
            if n in needed:return
            needed.add(n)
            for d in index[n]['deps']:include(d)
        include(stage);return [s for s in stages if s['name'] in needed]
    if command=='build':return [s for s in stages if s['group']=='build']
    if command=='analysis':return [s for s in stages if s['group']=='analysis']
    return stages

def input_errors(c,root=ROOT):
    errors=[]
    for row in read_json(root/'config/inputs.json'):
        p=resolve(row['path'],c,root)
        if row['kind']=='glob':
            # pathlib absolute globs are unavailable; use glob with recursive=True.
            import glob
            found=[Path(x) for x in glob.glob(str(p),recursive=True) if Path(x).is_file() and Path(x).stat().st_size]
        else:found=[p] if p.is_file() and p.stat().st_size else []
        if len(found)<row['minimum_files']:errors.append({'id':row['id'],'path':row['path'],'problem':f'Need >= {row["minimum_files"]} nonempty file(s); found {len(found)}','source':row['source']})
    return errors

def remove_generated(p,c):
    allowed=[c['data_dir']/'intermediate',c['data_dir']/'processed',c['output_dir']]
    # Never follow a generated-directory symlink into an external input snapshot.
    for base in allowed:
        try:p.absolute().relative_to(base.absolute());break
        except ValueError:pass
    else:raise ValueError(f'Refusing to remove non-generated path: {p}')
    for node in [p,*p.parents]:
        if node.is_symlink():raise ValueError(f'Refusing to overwrite through symlinked generated output: {node}')
        if node==base:break
    if p.is_dir():shutil.rmtree(p)
    elif p.exists():p.unlink()

class RunLock:
    def __init__(self,path):self.path=Path(path)
    def __enter__(self):
        self.path.parent.mkdir(parents=True,exist_ok=True)
        try:self.path.mkdir()
        except FileExistsError:raise RuntimeError(f'Run lock exists: {self.path}. Check its owner.json; remove this directory manually only after confirming that the prior process has stopped.')
        write_json(self.path/'owner.json',{'pid':os.getpid(),'host':__import__('platform').node(),'started':now()});return self
    def __exit__(self,*args):shutil.rmtree(self.path)

def environment(c):
    env=os.environ.copy()
    # Do not inherit another research run's dataset overrides or month restrictions.
    for k in list(env):
        if k.startswith('REPRO_') or k in ['DEWEY_FIRST_MONTH','DEWEY_LAST_MONTH','DEWEY_PLACE_DIR','PLACE_DIR','DEWEY_SPEND_FILES_DIR','DEWEY_MONTHLY_PATTERNS_FILES_DIR','SMOKE_AVOIDANCE_PROJECT_DIR','SMOKE_AVOIDANCE_DATA_DIR']:env.pop(k,None)
    env.update(SMOKE_AVOIDANCE_PROJECT_DIR=str(ROOT),SMOKE_AVOIDANCE_DATA_DIR=str(c['data_dir']),SMOKE_AVOIDANCE_OUTPUT_DIR=str(c['output_dir']),SMOKE_THREADS=str(c['threads']),TZ='UTC',OMP_NUM_THREADS=str(c['threads']),OPENBLAS_NUM_THREADS='1',ARROW_NUM_THREADS=str(c['threads']))
    return env

def execute(stages,c,resume=False):
    out=c['output_dir'];out.mkdir(parents=True,exist_ok=True)
    with RunLock(out/'.run-lock'):
        state_path=out/'run_status.json';state=read_json(state_path) if state_path.exists() else {'stages':{}}
        state.update(state='running',requested_stages=[s['name'] for s in stages],started=now(),platform=sys.platform,python=sys.version,threads=c['threads']);write_json(state_path,state)
        cache={};common=[ROOT/'project_helpers_and_constants.R',ROOT/'scripts/launch.R',ROOT/'config/paths.json',ROOT/'renv.lock']
        helpers=list((ROOT/'2_analysis').rglob('*helper*.R'))+list((ROOT/'1_build').rglob('*helpers.R'))+[ROOT/'2_analysis/lpdid_build_dataset.R',ROOT/'scripts/bootstrap.R']
        for s in stages:
            name=s['name'];print(f'{now()} {name}: checking prerequisites',flush=True)
            log=out/'logs'/f'{name}.log';log.parent.mkdir(parents=True,exist_ok=True)
            outputs=[resolve(p,c) for p in s['outputs']]
            started=time.monotonic()
            state.update(current_stage=name,phase='fingerprinting');write_json(state_path,state)
            try:
                signature={'stage':s,'code':{str(f.relative_to(ROOT)):digest(f,cache) for f in common+helpers+[ROOT/s['script']]},'inputs':{i:digest(resolve(i,c),cache) for i in s['inputs']},'threads':c['threads']}
                sig=hashlib.sha256(json.dumps(signature,sort_keys=True).encode()).hexdigest()
                prev=state['stages'].get(name,{})
                if resume and prev.get('signature')==sig and prev.get('status')=='complete' and outputs and all(p.exists() and digest(p,cache)==prev.get('outputs',{}).get(o) for o,p in zip(s['outputs'],outputs)):
                    print(f'{name}: verified, reused',flush=True);continue
                for p in outputs:
                    if p.exists():remove_generated(p,c)
                    p.parent.mkdir(parents=True,exist_ok=True)
                state['stages'][name]={'status':'running','started':now(),'signature':sig,'log':str(log.relative_to(out))};write_json(state_path,state)
                state['phase']='executing';write_json(state_path,state)
                env=environment(c);env.update(REPRO_STAGE_SCRIPT=s['script'],REPRO_STAGE_CHECKS=str(out/'checks'/name))
                with log.open('w',encoding='utf-8') as stream:
                    result=subprocess.run([c['rscript'],'--vanilla',str(ROOT/'scripts/launch.R'),s['script']],cwd=ROOT,env=env,stdout=stream,stderr=subprocess.STDOUT)
                if result.returncode:raise RuntimeError(f'R stage exited {result.returncode}; see {log}')
                hashes={o:digest(p,cache) for o,p in zip(s['outputs'],outputs)}
                state['stages'][name].update(status='complete',finished=now(),elapsed_seconds=round(time.monotonic()-started,2),outputs=hashes,input_fingerprints=signature['inputs']);write_json(state_path,state)
                print(f'{now()} {name}: complete',flush=True)
            except BaseException as e:
                state['stages'].setdefault(name,{}).update(status='failed',error=str(e),finished=now());state['state']='failed';write_json(state_path,state);raise
        state.update(state='complete',finished=now());write_json(state_path,state)

def _validate(c):
    problems=[];exhibits=read_json(ROOT/'config/exhibits.json')
    for ex in exhibits:
        for a in ex['assets']:
            f=resolve(a['path'],c)
            if not f.is_file() or f.stat().st_size==0:problems.append(f"Missing {ex['exhibit']}: {a['path']}")
    if problems:
        print('\n'.join(problems));return 1
    result=subprocess.run([c['rscript'],'--vanilla',str(ROOT/'scripts/validate_results.R')],cwd=ROOT,env=environment(c))
    if result.returncode:return result.returncode
    print(f'Validated {len(exhibits)} exhibits and numerical benchmarks. See output/validation/.');return 0

def validate(c):
    status=1
    try:
        status=_validate(c)
        return status
    finally:
        report={'status':'passed' if status==0 else 'failed','finished':now()}
        write_json(c['output_dir']/'validation/status.json',report)
        state_path=c['output_dir']/'run_status.json'
        if state_path.exists():
            state=read_json(state_path);state['validation']=report
            if status:state['state']='failed_validation'
            write_json(state_path,state)

def main(argv=None):
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('command',choices=['plan','check-inputs','doctor','build','analysis','all','validate','test'])
    parser.add_argument('--resume',action='store_true');parser.add_argument('--stage',help='Run this stage and its prerequisites')
    args=parser.parse_args(argv);c=settings();stages=selected(read_json(ROOT/'config/pipeline.json'),args.command,args.stage)
    if args.command=='plan':
        for s in stages:print(s['name']+' <- '+', '.join(s['deps']))
        return 0
    if args.command=='doctor':
        print('Python:',sys.version);print('Rscript:',shutil.which(c['rscript']) or c['rscript'])
        return subprocess.call([c['rscript'],'--vanilla',str(ROOT/'scripts/doctor.R')],cwd=ROOT,env=environment(c))
    if args.command=='test':
        status=subprocess.call([sys.executable,'-m','unittest','discover','-s','tests','-p','test_runner.py'],cwd=ROOT)
        if status:return status
        return subprocess.call([c['rscript'],'--vanilla',str(ROOT/'scripts/test.R')],cwd=ROOT,env=environment(c))
    if args.command in ['all','build','check-inputs'] and not args.stage:
        errors=input_errors(c)
        if errors:
            for e in errors:print(f"{e['id']}: {e['path']} — {e['problem']}")
            print(f'{len(errors)} missing/incomplete inputs. See data/README.md. Nothing has been estimated.');return 2
        status=subprocess.call([c['rscript'],'--vanilla',str(ROOT/'scripts/check_inputs.R')],cwd=ROOT,env=environment(c))
        if status or args.command=='check-inputs':return status
    if args.command=='validate':return validate(c)
    status=subprocess.call([c['rscript'],'--vanilla',str(ROOT/'scripts/doctor.R')],cwd=ROOT,env=environment(c))
    if status:return status
    execute(stages,c,args.resume)
    return validate(c) if args.command in ['all','analysis'] and not args.stage else 0

if __name__=='__main__':
    try:sys.exit(main())
    except (ValueError,RuntimeError,FileNotFoundError) as e:print(f'ERROR: {e}',file=sys.stderr);sys.exit(1)
