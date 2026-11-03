@echo off
setlocal
set "SBCL_HOME=%~dp0runtime\lib"
set "PATH=%~dp0runtime\bin;%PATH%"
"%~dp0runtime\bin\sbcl.exe" --noinform --no-sysinit --no-userinit --script "%~dp0packaging\launch.lisp" %*
