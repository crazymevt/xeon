(defpackage #:xeon.repl
  (:use #:cl)
  (:export #:repl-function-help #:make-repl-session #:connect-repl #:disconnect-repl #:stop-repl-session
           #:repl-submit #:repl-compile #:repl-interrupt #:repl-clear #:repl-snapshot #:repl-history
           #:repl-inspect #:repl-inspect-result #:repl-inspector-action #:repl-debug-frame #:repl-debug-restart
           #:repl-debug-abort #:repl-debug-inspect-local))
(in-package #:xeon.repl)
(defun editor-error (control &rest args) (error (apply #'format nil control args)))
(defun whitespace-char-p (c) (find c '(#\Space #\Tab #\Return #\Newline #\Page)))
(defun number-string-p (text)
  (and (plusp (length text)) (some #'digit-char-p text)
       (every (lambda (c) (find c "0123456789+-./eEdDfFlLsS")) text)
       (ignore-errors (let ((*read-eval* nil)) (numberp (read-from-string text))))))
(defvar *wire-depth* 0)
(defvar *repl-session* nil)
(defun message (control &rest args)
  (when *repl-session* (record-repl *repl-session* (format nil "~?~%" control args))))
