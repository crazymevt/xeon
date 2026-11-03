@echo off
rem Launches Xeon on Windows, with the LGT checkout beside this repository.
rem   tools\run.cmd                  opens Xeon on this repository
rem   tools\run.cmd C:\path\folder   opens Xeon on another folder
rem Needs SBCL on PATH and Quicklisp in the SBCL init file.
setlocal
set "ROOT=%~dp0.."
for %%I in ("%ROOT%") do set "ROOT=%%~fI"
for %%I in ("%ROOT%\..\lgt") do set "LGT=%%~fI"
set "LGT=%LGT:\=/%/"
set "CL_SOURCE_REGISTRY=(:source-registry (:directory "%LGT%") :inherit-configuration)"
if "%~1"=="" (set "XEON_OPEN_FOLDER=%ROOT%") else (set "XEON_OPEN_FOLDER=%~f1")
sbcl --load "%~dp0run.lisp"
