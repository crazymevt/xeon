;;; Capture the Xeon window with the macOS compositor, then quit.
;;; XEON_SCREENSHOT=path.png XEON_OPEN_FILE=... XEON_OPEN_FOLDER=... sbcl --load tools/screenshot.lisp
;;; Needs screen-recording access for the terminal running it.
(load (merge-pathnames "bootstrap.lisp" *load-truename*))
(unless (find-package :ql)
  (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
(uiop:symbol-call :ql :quickload "xeon/ui" :silent t)
(in-package #:lgt.cocoa)

(defvar *capture-error* nil)
(defun capture-setting (name)
  (let ((v (uiop:getenv name))) (and v (plusp (length v)) v)))

(after 0.2
  (lambda ()
    (let ((window nil))
      (unwind-protect
           (handler-case
               (let ((workbench (xeon.ui:make-workbench :file (capture-setting "XEON_OPEN_FILE")
                                                        :folder (capture-setting "XEON_OPEN_FOLDER")
                                                        :remember-frame nil
                                                        :history (xeon.themes:make-workspace-history
                                                                  :path (merge-pathnames "xeon-screenshot-history.sexp" (uiop:temporary-directory))))))
                 (setf window (xeon.ui:workbench-window workbench))
                 (let ((script (capture-setting "XEON_SCREENSHOT_SCRIPT")))
                   (when script (funcall (compile nil (read-from-string script)) workbench)))
                 (show window)
                 (msg* :void (ns-app (current-backend)) "activateIgnoringOtherApps:" :char 1)
                 (msg* :void (native window) "makeKeyAndOrderFront:" :pointer (nil-pointer))
                 (flush-layouts)
                 (msg* :void (msg (objc-class "NSRunLoop") "mainRunLoop") "runUntilDate:"
                       :pointer (msg (objc-class "NSDate") "dateWithTimeIntervalSinceNow:"
                                     :double (float (or (ignore-errors (parse-integer (capture-setting "XEON_SCREENSHOT_WAIT"))) 1) 1d0)))
                 (multiple-value-bind (x y w h) (rect-values (msg* '(:struct ns-rect) (native window) "frame"))
                   (let ((path (namestring (merge-pathnames (or (capture-setting "XEON_SCREENSHOT") "build/xeon.png")))))
                     (ensure-directories-exist path)
                     (uiop:run-program (list "/usr/sbin/screencapture" "-x" "-R"
                                             (format nil "~d,~d,~d,~d" (round x) (round (- (main-screen-height) y h))
                                                     (round w) (round h))
                                             path)))))
             (error (e) (setf *capture-error* e) (format *error-output* "~&CAPTURE ERROR: ~A~%" e)))
        (when window (close-window window :force t))
        (quit)))))
(run :keep-alive t)
(when *capture-error* (uiop:quit 1))
