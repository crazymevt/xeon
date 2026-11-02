(in-package #:xeon.ui)

(defclass workbench ()
  ((window :accessor workbench-window)
   (manager :initform (xeon.core:make-buffer-manager) :reader workbench-manager)
   (themes :initarg :themes :reader workbench-themes)
   (palette :accessor workbench-palette)
   (history :initarg :history :accessor workbench-history)
   (documents :initform '() :accessor workbench-documents)
   (untitled-count :initform 0 :accessor workbench-untitled-count)
   ;; Widgets
   (tabs :accessor workbench-tabs)
   (outer-split :accessor workbench-outer-split)
   (inner-split :accessor workbench-inner-split)
   (sidebar :accessor workbench-sidebar)
   (tools :accessor workbench-tools)
   (rail :accessor workbench-rail)
   (status :accessor workbench-status)
   (explorer :initform nil :accessor workbench-explorer)
   (repl :initform nil :accessor workbench-repl)
   (settings :initform nil :accessor workbench-settings)
   ;; Remembered pane widths, so hiding and showing a pane restores it.
   (sidebar-width :initform 260 :accessor workbench-sidebar-width)
   (tools-width :initform 420 :accessor workbench-tools-width)
   (remember-layout :initarg :remember-layout :initform t :reader workbench-remember-layout)
   (layout-timer :initform nil :accessor workbench-layout-timer))
  (:documentation "One Xeon window: its documents, panes and runtime tools."))

(defvar *workbench* nil
  "The workbench of the window being built or driven, for interactive use.")

(defun report (workbench message &rest arguments)
  "Shows a short message in the status bar."
  (let ((text (apply #'format nil message arguments)))
    (when (and workbench (slot-boundp workbench 'status))
      (set-status-message workbench text))
    text))

(defmacro with-reported-errors ((workbench) &body body)
  "Runs BODY; an error is shown in the status bar rather than entering the debugger."
  `(handler-case (progn ,@body)
     (error (condition)
       (report ,workbench "~A" condition)
       nil)))
