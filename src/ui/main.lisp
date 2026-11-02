(in-package #:xeon.ui)

(defun start (&key file folder)
  "Opens a Xeon window and runs the event loop until it closes."
  (setf (application-name *application*) "Xeon")
  (let ((workbench (make-workbench :file file :folder folder)))
    (setf *workbench* workbench)
    (show (workbench-window workbench))
    (run)))
