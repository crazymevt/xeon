# Xeon

Headless Common Lisp editor backend: documents, editing, search, syntax,
projects, REPLs, terminals, HTTP, SQLite and AI chat sessions. SBCL is the
supported implementation.

The GUI is built with [LGT](../lgt), the Lisp GUI Toolkit, using native macOS
controls. The backend was copied from Xeon's LUT-based tree; see
[PROVENANCE.md](PROVENANCE.md).

## Run the editor

LGT must be visible to Quicklisp (for example linked into
`~/quicklisp/local-projects/`).

```sh
make run                         # opens the current folder
make run FILE=src/ui/editor.lisp
make run FOLDER=~/projects/lisp/lgt
make ui-test                     # headless UI tests
```

The window has an activity rail, a folder explorer, editor tabs with syntax
highlighting and line numbers, runtime tools (REPL, Problems, Debugger,
Inspector) and a status bar with a language picker. The REPL connects to, or
starts, SBCL (Swank), Clojure (nREPL) or Racket (Cadre). The Lisp menu evaluates
or compiles the current form (Command+Return, Command+K) or the whole document
(add Shift). Markdown documents get a live preview beside the editor: the Preview button at the
end of the tab bar, or Shift+Command+V. Command+B and Command+J toggle the explorer and tools. Xeon remembers the window's size and position, whether it
was maximized, the explorer and tools widths and which of them are shown. Settings
(the gear, or Xeon › Settings…) choose the theme, editor font size, tab width and
whitespace trimming. `tools/screenshot.lisp` captures the window for review.

## Load and test

Make CL-PPCRE 2.1.2 available to ASDF (for example through Quicklisp), or place
its sources at `build/deps/cl-ppcre/`. The development runner also accepts
`XEON_CL_PPCRE_ASD` pointing to its `.asd` file.

```lisp
(require :asdf)
(asdf:load-asd #P"/absolute/path/to/xeon/xeon.asd")
(asdf:load-system "xeon")
(asdf:test-system "xeon")
```

```sh
make test
make repl-test
```

Development runners put compiled files in `build/cache/`. `make` uses
`/Users/jessie/.local/bin/quiet-run.sh`; override `QUIET_RUN` with another log
wrapper on other machines. CI uses the portable `tools/ci-run.py` wrapper.

## Systems

| System | Responsibility |
| --- | --- |
| `xeon` | Core, search, themes, folders and syntax |
| `xeon/core` | Shared text storage, bounded undo, cursors, editing, files, buffer manager and documentation |
| `xeon/search` | Literal and regular expression search/replace |
| `xeon/themes` | Theme palettes, configuration and history |
| `xeon/folders` | Lazy project trees, Git status, file mutations, workers and workspace search |
| `xeon/syntax` | Highlighting, completion, structural editing, indentation, outline and formatting |
| `xeon/repl` | Swank, nREPL and Racket sessions, problems, inspection and help |
| `xeon/assistants` | Codex/Claude process protocols, sessions, approvals and history |
| `xeon/chat-render` | Escaped Markdown and optional HTML presentation |
| `xeon/http` | HTTP collections, request builder and native libcurl jobs |
| `xeon/databases` | Native SQLite adapter |
| `xeon/terminal` | libvterm with PTY/ConPTY transport |
| `xeon/docset` | Vendored local documentation reader |

The optional native adapters are loaded explicitly. HTTP and terminal require
CFFI and Babel; SQLite requires CFFI. SQLite needs a platform SQLite library.
Build the terminal bridge with `make terminal-build`; `make terminal-test`
checks the native emulator and shell transport. `make http-bundle` builds a
private libcurl bundle and requires network access and native build tools.
Run `make http-test` for loopback HTTP/TLS checks, using the native bundle if
present or the OS libcurl for source development. The HTTP runner discovers
CFFI/Babel from `build/http-test-deps/` or a local Quicklisp installation.
The native bridge retains its `krypton_terminal` library name and `kte_*` ABI.

## UI integration boundary

The `xeon/ui` system depends on LGT and the needed backend systems.
Keep widgets, layout and input translation there. Use
`xeon.core:buffer-manager` for documents, `buffer-cursor` for view selections,
range APIs for text access, and change listeners for invalidation. Keep editing
rules, file operations and language behavior in the backend. Loading `xeon`
must remain independent of LGT and any other GUI toolkit; `make test` checks this.

HTML documentation exposes `data-entry` and `data-action` attributes for an
adapter to handle. Chat HTML emits `xeon-message` DOM events with the payload in
`event.detail` and exposes `window.xeonChat` and `window.xeonInput`; a web host
must supply its own bridge. Native views can consume backend snapshots directly.

See [PROVENANCE.md](PROVENANCE.md) for import details and retained notices.
