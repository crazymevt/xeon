#!/usr/bin/env python3
"""Build the vendored terminal bridge with CMake (no dependency downloads)."""
import argparse
from pathlib import Path
import subprocess
import sys

root = Path(__file__).resolve().parent.parent
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--config', default='Release')
args = parser.parse_args()
build = root / 'build' / 'terminal'
for command in ([ 'cmake', '-S', str(root / 'native' / 'terminal'), '-B', str(build),
                  '-DCMAKE_BUILD_TYPE=' + args.config ],
                ['cmake', '--build', str(build), '--config', args.config]):
    result = subprocess.run(command, cwd=root)
    if result.returncode:
        sys.exit(result.returncode)
