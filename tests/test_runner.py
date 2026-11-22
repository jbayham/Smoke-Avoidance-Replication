import importlib.util,json,tempfile,unittest
from pathlib import Path
from unittest.mock import patch
from types import SimpleNamespace
spec=importlib.util.spec_from_file_location('runner',Path(__file__).resolve().parents[1]/'run.py');r=importlib.util.module_from_spec(spec);spec.loader.exec_module(r)
class RunnerTests(unittest.TestCase):
 def test_actual_graph_and_exhibits(self):
  stages=r.ordered(r.read_json(r.ROOT/'config/pipeline.json'));order={s['name']:i for i,s in enumerate(stages)}
  for s in stages:
   self.assertTrue((r.ROOT/s['script']).is_file())
   for d in s['deps']:self.assertLess(order[d],order[s['name']])
  outputs={o for s in stages for o in s['outputs']}
  for ex in r.read_json(r.ROOT/'config/exhibits.json'):
   for a in ex['assets']:self.assertIn(a['path'],outputs)
 def test_cycle_fails(self):
  with self.assertRaises(ValueError):r.ordered([{'name':'a','deps':['b']},{'name':'b','deps':['a']}])
 def test_content_hash_detects_change_and_paths_with_spaces(self):
  with tempfile.TemporaryDirectory(prefix='smoke test ') as d:
   p=Path(d)/'file name.csv';p.write_text('a');before=r.digest(p);p.write_text('b');self.assertNotEqual(before,r.digest(p))
 def test_lock_excludes_second_process_and_releases(self):
  with tempfile.TemporaryDirectory() as d:
   p=Path(d)/'lock'
   with r.RunLock(p):
    with self.assertRaises(RuntimeError):
     with r.RunLock(p):pass
   self.assertFalse(p.exists())
 def test_raw_input_cannot_be_removed(self):
  with tempfile.TemporaryDirectory() as d:
   root=Path(d);c={'data_dir':root/'data','output_dir':root/'output'};p=root/'data/raw/x';p.parent.mkdir(parents=True);p.write_text('keep')
   with self.assertRaises(ValueError):r.remove_generated(p,c)
   self.assertEqual(p.read_text(),'keep')
 def test_empty_checkout_reports_all_inputs(self):
  with tempfile.TemporaryDirectory() as d:
   c={'data_dir':Path(d),'output_dir':Path(d)/'unused'}
   self.assertEqual(len(r.input_errors(c)),len(r.read_json(r.ROOT/'config/inputs.json')))
 def test_analysis_requires_no_build_execution(self):
  stages=r.selected(r.read_json(r.ROOT/'config/pipeline.json'),'analysis')
  self.assertTrue(stages);self.assertTrue(all(s['group']=='analysis' for s in stages))
 def test_resume_and_downstream_invalidation(self):
  with tempfile.TemporaryDirectory(prefix='smoke test ') as d:
   root=Path(d);c={'data_dir':root/'data','output_dir':root/'output','threads':1,'rscript':'fake-R'}
   for rel in ['project_helpers_and_constants.R','scripts/launch.R','scripts/bootstrap.R','config/paths.json','config/pipeline.json','renv.lock','2_analysis/lpdid_build_dataset.R','first.R','second.R']:
    f=root/rel;f.parent.mkdir(parents=True,exist_ok=True);f.write_text('code')
   (root/'1_build').mkdir()
   raw=root/'data/raw/input.csv';raw.parent.mkdir(parents=True);raw.write_text('a')
   stages=[{'name':'first','script':'first.R','inputs':['data/raw/input.csv'],'outputs':['data/intermediate/one.csv']},
           {'name':'second','script':'second.R','inputs':['data/intermediate/one.csv'],'outputs':['output/result.csv']}]
   calls=[]
   def fit(args,**kwargs):
    calls.append(args[-1]);stage=stages[0 if args[-1]=='first.R' else 1]
    src=r.resolve(stage['inputs'][0],c);dest=r.resolve(stage['outputs'][0],c);dest.write_text(src.read_text()+' fitted')
    return SimpleNamespace(returncode=0)
   with patch.object(r,'ROOT',root),patch.object(r.subprocess,'run',side_effect=fit):
    r.execute(stages,c,True);self.assertEqual(len(calls),2)
    r.execute(stages,c,True);self.assertEqual(len(calls),2)
    raw.write_text('b');r.execute(stages,c,True);self.assertEqual(len(calls),4)
    (root/'output/result.csv').write_text('corrupt');r.execute(stages,c,True);self.assertEqual(len(calls),5)
   self.assertEqual(raw.read_text(),'b')
 def test_failed_stage_is_not_complete(self):
  with tempfile.TemporaryDirectory() as d:
   root=Path(d);c={'data_dir':root/'data','output_dir':root/'output','threads':1,'rscript':'fake-R'}
   for rel in ['project_helpers_and_constants.R','scripts/launch.R','scripts/bootstrap.R','config/paths.json','config/pipeline.json','renv.lock','2_analysis/lpdid_build_dataset.R','first.R']:
    f=root/rel;f.parent.mkdir(parents=True,exist_ok=True);f.write_text('code')
   (root/'1_build').mkdir()
   stages=[{'name':'failure','script':'first.R','inputs':[],'outputs':['output/result.csv']}]
   with patch.object(r,'ROOT',root),patch.object(r.subprocess,'run',return_value=SimpleNamespace(returncode=1)):
    with self.assertRaises(RuntimeError):r.execute(stages,c)
   state=r.read_json(root/'output/run_status.json');self.assertEqual(state['state'],'failed');self.assertEqual(state['stages']['failure']['status'],'failed')
   self.assertFalse((root/'output/.run-lock').exists())
 def test_validation_failure_is_recorded(self):
  with tempfile.TemporaryDirectory() as d:
   c={'output_dir':Path(d)};r.write_json(Path(d)/'run_status.json',{'state':'complete'})
   with patch.object(r,'_validate',return_value=1):self.assertEqual(r.validate(c),1)
   self.assertEqual(r.read_json(Path(d)/'run_status.json')['state'],'failed_validation')
   self.assertEqual(r.read_json(Path(d)/'validation/status.json')['status'],'failed')
if __name__=='__main__':unittest.main()
