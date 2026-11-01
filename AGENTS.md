# Xeon

- Common Lisp, targeting SBCL on macOS, Linux and Windows.
- This repository is currently backend only. The GUI will use LGT (`../lgt`).
  Keep UI packages and ASDF systems separate from the backend.
- Do not add application drawing callbacks or render methods. Report missing LGT
  capabilities so they can be addressed in the toolkit.
- Treat professional visual design and interaction polish as requirements.
- Keep UI updates event-driven. Never rebuild trees, assign styles, refresh
  captions or invalidate widgets on every frame. Test native cache reuse and
  measure idle CPU as the GUI grows; report toolkit costs separately.
- Keep editor state, commands, documents and language behavior headless.
- Use range/chunk APIs, shared text storage and bounded undo for large files.
- Preserve vendor licenses and provenance.
- Run backend checks with `make test`; run live SBCL integration with `make repl-test`.
- Always run noisy non-interactive shell commands through
  `/Users/jessie/.local/bin/quiet-run.sh -- <command> [args...]`.
  Wrap entire pipelines/compound operations with the wrapper and `bash -lc`.
  Use its status and log path, inspecting only relevant log excerpts.
  Short targeted reads and interactive commands may run directly.
  If unavailable, report that and use bounded output.
