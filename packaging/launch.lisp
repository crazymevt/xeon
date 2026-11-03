;;; Entry point for the packaged SBCL runtime.
(require :asdf)

(defparameter *xeon-root*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname *load-truename*)))

;; Keep build outputs relocatable, including the macOS libffi grovel output.
(asdf:initialize-output-translations
 `(:output-translations
   ((,*xeon-root* :**/ :*.*.*) (,*xeon-root* "cache/" :**/ :*.*.*))
   :ignore-inherited-configuration))

(load (merge-pathnames "quicklisp/setup.lisp" *xeon-root*))
(asdf:load-asd (merge-pathnames "xeon.asd" *xeon-root*))
(uiop:symbol-call :ql :quickload
                  #+(and unix (not darwin)) '("xeon/ui" "lgt/gtk/native")
                  #-(and unix (not darwin)) "xeon/ui"
                  :silent t)

(if (member "--check" (uiop:command-line-arguments) :test #'string=)
    (progn
      (assert (uiop:symbol-call :xeon.terminal :terminal-available-p))
      (uiop:symbol-call :xeon.http :initialize-transport)
      (uiop:symbol-call :xeon.databases :disconnect
                       (uiop:symbol-call :xeon.databases :connect ":memory:" :create t)))
    (progn
      #+(and unix (not darwin)) (uiop:symbol-call :lgt :use-backend :gtk)
      (uiop:symbol-call :xeon.ui :start)))
