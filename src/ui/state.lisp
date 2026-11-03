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
   ;; The editor groups, each a tabbed pane; several when the editor is split.
   (groups :initform '() :accessor workbench-groups)
   (active-tabs :initform nil :accessor workbench-active-tabs)
   (editor-area :accessor workbench-editor-area)
   (outer-split :accessor workbench-outer-split)
   (inner-split :accessor workbench-inner-split)
   (sidebar :accessor workbench-sidebar)
   (tools :accessor workbench-tools)
   (rail :accessor workbench-rail)
   (status :accessor workbench-status)
   (explorer :initform nil :accessor workbench-explorer)
   (projects :initform nil :accessor workbench-projects)
   (source-control :initform nil :accessor workbench-source-control)
   (databases :initform nil :accessor workbench-databases)
   (api :initform nil :accessor workbench-api)
   ;; The sidebar shows one view at a time: :explorer, :projects, :source-control, :databases or :api.
   (sidebar-stack :accessor workbench-sidebar-stack)
   (sidebar-pages :initform '() :accessor workbench-sidebar-pages)
   (sidebar-view :initform :explorer :accessor workbench-sidebar-view)
   (repl :initform nil :accessor workbench-repl)
   (settings :initform nil :accessor workbench-settings)
   ;; Remembered pane widths, so hiding and showing a pane restores it.
   (sidebar-width :initform 260 :accessor workbench-sidebar-width)
   (tools-width :initform 420 :accessor workbench-tools-width)
   (remember-layout :initarg :remember-layout :initform t :reader workbench-remember-layout)
   (layout-timer :initform nil :accessor workbench-layout-timer))
  (:documentation "One Xeon window: its documents, panes and runtime tools."))

;;; Editor groups. The editor area holds one tabbed pane, or several split side by
;;; side or one above another. Commands act on the active group: the one holding the
;;; keyboard focus, otherwise the one used last.

(defun inside-p (widget ancestor)
  (loop for w = widget then (parent w)
        while w
        thereis (eq w ancestor)))

(defun group-of (workbench widget)
  "The editor group WIDGET is in, or NIL."
  (find-if (lambda (group) (inside-p widget group)) (workbench-groups workbench)))

(defun workbench-tabs (workbench)
  "The active editor group."
  (let* ((focused (ignore-errors (focused-widget)))
         (group (and focused (group-of workbench focused))))
    (when group (setf (workbench-active-tabs workbench) group))
    (let ((active (workbench-active-tabs workbench)))
      (if (and active (member active (workbench-groups workbench)) (not (destroyed-p active)))
          active
          (setf (workbench-active-tabs workbench) (first (workbench-groups workbench)))))))

(defun (setf workbench-tabs) (group workbench)
  "Adds GROUP to the editor groups if it is new, and makes it the active one."
  (unless (member group (workbench-groups workbench))
    (setf (workbench-groups workbench) (append (workbench-groups workbench) (list group))))
  (setf (workbench-active-tabs workbench) group))

(defun all-tab-pages (workbench)
  "The pages of every editor group."
  (loop for group in (workbench-groups workbench) append (copy-list (children group))))

(defun show-page (workbench page)
  "Selects PAGE in its editor group and makes that group the active one."
  (let ((group (parent page)))
    (setf (workbench-active-tabs workbench) group)
    (select-page group page)))

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
