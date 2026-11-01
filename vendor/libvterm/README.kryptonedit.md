# Vendored libvterm

libvterm 0.3.3, from https://github.com/neovim/libvterm,
tag `v0.3.3`, commit `9d6d2112335080312ef8c36667fa717ded4f7daf`.
MIT license; see LICENSE (and the Unicode table's attribution in src/unicode.c).

Only include/ and src/ are vendored. Encoding `.inc` files were generated from
the unchanged `.tbl` files with upstream `perl -CSD tbl2inc_c.pl <table>`.
No network fetch or Perl installation is needed to build KryptonEdit.
All application-specific adaptation lives in native/terminal/bridge.cpp.
Changes to this vendor snapshot must record the source revision and rerun the
native emulator and PTY/ConPTY suite on all three platforms.
