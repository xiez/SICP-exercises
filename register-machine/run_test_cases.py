import os
import subprocess

files = [f for f in os.listdir('.') if os.path.isfile(f) and f.startswith('test_') and f.endswith('.py')]
for f in files:
    print(f'testing {f}...')
    subprocess.run(f'python3 {f}', shell=True)
