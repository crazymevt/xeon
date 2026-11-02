;;; Launch the Xeon window: make run [FILE=path] [FOLDER=path]
(load (merge-pathnames "bootstrap.lisp" *load-truename*))
(unless (find-package :ql)
  (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
(uiop:symbol-call :ql :quickload "xeon/ui" :silent t)
(flet ((setting (name) (let ((v (uiop:getenv name))) (and v (plusp (length v)) v))))
  (uiop:symbol-call :xeon.ui :start :file (setting "XEON_OPEN_FILE") :folder (setting "XEON_OPEN_FOLDER")))
