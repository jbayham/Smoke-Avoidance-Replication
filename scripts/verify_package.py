"""Verify distributed source files against MANIFEST.sha256 (Python standard library)."""
from pathlib import Path
import hashlib
import sys
root=Path(__file__).resolve().parents[1]
errors=[];count=0
for line in (root/'MANIFEST.sha256').read_text(encoding='utf-8').splitlines():
 expected,relative=line.split('  ',1);p=Path(relative)
 if p.is_absolute() or '..' in p.parts:raise ValueError('Unsafe manifest path')
 f=root/p;count+=1
 if not f.is_file():errors.append('Missing: '+relative);continue
 if hashlib.sha256(f.read_bytes()).hexdigest()!=expected:errors.append('Changed: '+relative)
if errors:print('\n'.join(errors));sys.exit(1)
print(f'PASS: {count} distributed files match their SHA-256 hashes.')
