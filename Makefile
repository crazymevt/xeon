.DEFAULT_GOAL := test
SBCL ?= sbcl
PYTHON ?= python3
QUIET_RUN ?= $(wildcard /Users/jessie/.local/bin/quiet-run.sh)
QUIET_PREFIX = $(if $(QUIET_RUN),"$(QUIET_RUN)" --)
LISP_FLAGS = --noinform --no-sysinit --no-userinit --non-interactive

.PHONY: test repl-test terminal-build terminal-test http-bundle http-test database-test
test:
	@$(QUIET_PREFIX) "$(SBCL)" $(LISP_FLAGS) --load tests/run.lisp

repl-test:
	@$(QUIET_PREFIX) "$(SBCL)" $(LISP_FLAGS) --load tools/check-repl.lisp

terminal-build:
	@$(QUIET_PREFIX) "$(PYTHON)" tools/build-terminal.py

terminal-test: terminal-build
	@$(QUIET_PREFIX) "$(PYTHON)" tools/check-terminal.py

http-bundle:
	@$(QUIET_PREFIX) "$(PYTHON)" tools/build-http-bundle.py

http-test:
	@$(QUIET_PREFIX) "$(PYTHON)" tools/check-http.py

database-test:
	@$(QUIET_PREFIX) "$(SBCL)" $(LISP_FLAGS) --load tests/run-databases.lisp

.PHONY: run ui-test
FILE ?=
FOLDER ?=
run:
	@XEON_OPEN_FILE="$(FILE)" XEON_OPEN_FOLDER="$(FOLDER)" $(QUIET_PREFIX) "$(SBCL)" $(LISP_FLAGS) --load tools/run.lisp

ui-test:
	@$(QUIET_PREFIX) "$(SBCL)" $(LISP_FLAGS) --load tests/run-ui.lisp
