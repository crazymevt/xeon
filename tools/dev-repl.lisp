(in-package #:cl-user)

(defun call-with-xeon-dev-repl (function)
  "Run FUNCTION with an opt-in, loopback-only Swank development server."
  (let ((port (parse-integer (or (uiop:getenv "XEON_REPL_PORT") "4005")
                             :junk-allowed nil)))
    (unless (<= 1 port 65535)
      (error "XEON_REPL_PORT must be between 1 and 65535."))
    (load (asdf:system-relative-pathname "xeon" "vendor/swank/swank-loader.lisp"))
    (let ((cache (merge-pathnames "xeon-dev-swank/" (uiop:temporary-directory))))
      (ensure-directories-exist cache)
      (setf (symbol-value (find-symbol "*FASL-DIRECTORY*" :swank-loader)) cache))
    (uiop:symbol-call :swank-loader :init)
    (uiop:symbol-call :swank :create-server
                      :port port :interface "127.0.0.1" :style :spawn :dont-close t)
    (unwind-protect
         (progn
           (format t "~&Xeon development REPL: 127.0.0.1:~D (Swank; local code execution enabled)~%" port)
           (finish-output)
           (funcall function))
      (uiop:symbol-call :swank :stop-server port))))
