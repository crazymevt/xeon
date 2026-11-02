;;; Standalone headless SQLite suite: `make database-test`. Needs CFFI (Quicklisp) and
;;; the platform SQLite library, so it is kept out of the dependency-free `make test`.
(load (merge-pathnames "../tools/bootstrap.lisp" *load-truename*))
(unless (find-package :ql)
  (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
(uiop:symbol-call :ql :quickload "cffi" :silent t)
(asdf:load-system "xeon/databases")
(load (merge-pathnames "databases.lisp" *load-truename*))
(assert (not (find-package "XEON.UI")))
