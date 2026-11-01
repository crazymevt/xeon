"""Capture a CI subprocess log and preserve its exit status on every supported OS."""

import argparse
import pathlib
import subprocess
import sys

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--log", required=True)
parser.add_argument("command", nargs=argparse.REMAINDER)
args = parser.parse_args()
command = args.command
if command[:1] == ["--"]:
    command = command[1:]
if not command:
    parser.error("a command is required")
log = pathlib.Path(args.log)
log.parent.mkdir(parents=True, exist_ok=True)
with log.open("wb") as output:
    try:
        result = subprocess.run(command, stdout=output, stderr=subprocess.STDOUT)
        status = result.returncode
    except OSError as error:
        output.write(str(error).encode("utf-8"))
        status = 127
print(f"{'OK' if status == 0 else 'FAILED'} (exit {status}): {log.resolve()}")
lines = log.read_text(encoding="utf-8", errors="replace").splitlines()
if status:
    print("\n".join(lines[-60:]))
else:
    for line in lines:
        if "backend checks passed" in line:
            print(line)
sys.exit(status if status >= 0 else 128 - status)
