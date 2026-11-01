"""Fetch pinned Lisp sources for HTTP CI, without Quicklisp network bootstrapping."""
from pathlib import Path
import runpy

shared = runpy.run_path(str(Path(__file__).with_name("build-http-bundle.py")))
for name, (url, revision) in shared["LOCK"]["lisp"].items():
    shared["checkout"](url, revision, shared["ROOT"] / "build/http-test-deps" / name)
