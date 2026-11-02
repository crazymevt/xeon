# Backend import

Imported on 2026-11-01 from the local `../xeon-lut` working tree, whose HEAD
was `ae364839` ("Integrate LUT event scheduling and simplify native setup").
That tree had uncommitted changes to backend files (`src/databases`, `src/http`,
`src/repl`, `src/syntax`, `tests/http.lisp`, `tests/outline.lisp`,
`tools/check-repl.lisp`); this is a snapshot of its working files, not just HEAD.
The source project was not modified.

Included: headless Common Lisp systems, their regression tests and fixtures,
REPL runtime vendors, local docsets, libvterm, terminal bridge source, native
build helpers and backend CI. Excluded: the LUT UI (`src/ui`, `xeon/ui`,
`xeon/gui`), UI assets (chat CSS/JS), UI tests, GUI launchers
and observers, GUI/chat/idle-CPU docs, the LUT revision pin and build output.
Optional library documentation discovery now looks for LGT instead of LUT.
Material Icons (`assets/fonts/material-icons/`, Apache 2.0, with its license and
provenance) was later copied from the same tree for the LGT UI.

## Earlier history

xeon-lut's backend was itself imported on 2026-10-31 from `../kryptonedit`
(HEAD `eb8323783d9c9b1ae84a815850896662222ba379`, plus working-tree changes).
Application systems, packages, environment variables and configuration
directories were renamed from KryptonEdit to Xeon. Chat rendering moved out of
`src/ui` and uses a neutral DOM event interface. Documentation rendering no
longer embeds a wxLambda bridge. The nREPL value helper namespace was adjusted
to match Xeon's client code. Native terminal ABI names and vendor protocol
keywords remain compatible.

Original notices remain alongside the code, including `src/repl/CADRE-LICENSE`,
`src/assistants/CADRE-LICENSE` and the licenses/provenance in `vendor/` and
`data/docsets/`. No new license is asserted for imported application code.
