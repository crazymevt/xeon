(in-package #:xeon.ui)

;;; The Projects view: the systems defined in the open folder (ASDF systems,
;;; deps.edn / project.clj and info.rkt projects), found by the backend on a worker
;;; thread. A system can be loaded, tested or rebuilt in the REPL, or run as its own
;;; process. Discovery runs when the view is first shown and again after the folder
;;; changes, never on a timer.

(defparameter *dialect-labels*
  '((:common-lisp "Common Lisp" :lisp) (:clojure "Clojure" :clojure) (:racket "Racket" :racket)))

(defun dialect-label (dialect) (or (second (assoc dialect *dialect-labels*)) (string-capitalize dialect)))

(defclass projects ()
  ((workbench :initarg :workbench :reader projects-workbench)
   (root :initform nil :accessor projects-root)
   (systems :initform '() :accessor projects-systems)
   ;; :stale until discovery is asked for, then :loading and :loaded.
   (state :initform :stale :accessor projects-state)
   (generation :initform 0 :accessor projects-generation)
   (warning :initform nil :accessor projects-warning)
   ;; System key -> plist of :run :status (:running :exited :stopped) :code :started.
   (runs :initform (make-hash-table :test #'equal) :reader projects-runs)
   (widgets :initform (make-hash-table) :reader projects-widgets)))

;;; The sidebar views keep their widgets by name in a WIDGETS table.
(defun part (view name) (gethash name (slot-value view 'widgets)))
(defun (setf part) (widget view name) (setf (gethash name (slot-value view 'widgets)) widget))

(defun system-key (system)
  (list (namestring (xeon.folders:discovered-system-file system)) (xeon.folders:discovered-system-name system)))

(defun system-run (projects system) (gethash (system-key system) (projects-runs projects)))

(defun system-running-p (projects system)
  (eq :running (getf (system-run projects system) :status)))

(defun system-location (projects system)
  "The definition file relative to the open folder."
  (let ((file (xeon.folders:discovered-system-file system))
        (root (projects-root projects)))
    (if (and root (uiop:subpathp file root))
        (enough-namestring file root)
        (uiop:native-namestring file))))

(defun system-icon (projects system)
  (if (system-running-p projects system)
      (icon :play :size 16 :color :system-green)
      (file-icon (workbench-palette (projects-workbench projects))
                 (third (assoc (xeon.folders:discovered-system-dialect system) *dialect-labels*)))))

;;; Discovery

(defun discover-projects (workbench &key force)
  "Finds the folder's systems on a worker thread, unless they are already known."
  (let* ((projects (workbench-projects workbench))
         (root (explorer-folder workbench)))
    (when (and projects root (or force (eq (projects-state projects) :stale)))
      (let ((generation (incf (projects-generation projects)))
            (table (part projects :table))
            (backend *backend*))
        (setf (projects-state projects) :loading
              (projects-root projects) root)
        (refresh-projects projects)
        (sb-thread:make-thread
         (lambda ()
           (let ((*backend* backend) (systems nil) (warning nil))
             (handler-case
                 (multiple-value-setq (systems warning)
                   (xeon.folders:discover-folder-systems (xeon.folders:open-folder-project root)))
               (error (condition) (setf warning (princ-to-string condition))))
             (call-in-ui-thread
              (lambda ()
                (when (= generation (projects-generation projects))
                  (setf (projects-systems projects) systems
                        (projects-warning projects) warning
                        (projects-state projects) :loaded)
                  (refresh-projects projects)
                  (when warning (report workbench "~A" warning))))
              :owner table)))
         :name "Xeon project discovery")))))

(defun projects-folder-changed (workbench)
  "The explorer opened another folder: forget the old systems, and find the new ones
now if the view is showing."
  (let ((projects (workbench-projects workbench)))
    (when projects
      (incf (projects-generation projects))
      (setf (projects-state projects) :stale (projects-systems projects) '()
            (projects-root projects) (explorer-folder workbench))
      (if (sidebar-view-shown-p workbench :projects)
          (discover-projects workbench)
          (refresh-projects projects)))))

;;; Refreshing the view

(defun filtered-systems (projects)
  (let ((filter (string-trim " " (text (part projects :filter)))))
    (if (zerop (length filter))
        (projects-systems projects)
        (remove-if-not (lambda (system)
                         (or (search filter (xeon.folders:discovered-system-name system) :test #'char-equal)
                             (search filter (system-location projects system) :test #'char-equal)))
                       (projects-systems projects)))))

(defun selected-system (projects)
  (let* ((table (part projects :table))
         (index (first (selected-indices table)))
         (items (items table)))
    (and index (< index (length items)) (elt items index))))

(defun select-system (projects system)
  (let* ((table (part projects :table))
         (index (and system (position (system-key system) (coerce (items table) 'list)
                                      :key #'system-key :test #'equal))))
    (with-handlers-blocked (table :selection-changed)
      (setf (selected-indices table) (if index (list index) '())))))

(defun show-body (projects page)
  "Shows the :LOADING, :EMPTY or :LIST page; the spinner only turns while shown."
  (let ((spinner (part projects :spinner)))
    (unless (eq (and (active spinner) t) (eq page :loading))
      (setf (active spinner) (eq page :loading))))
  (select-page (part projects :body) (part projects page)))

(defun refresh-projects (projects)
  "Brings the list, counts and empty states up to date with the known systems."
  (let* ((table (part projects :table))
         (selected (selected-system projects))
         (systems (filtered-systems projects))
         (all (projects-systems projects)))
    (unless (equal (mapcar #'system-key systems) (map 'list #'system-key (items table)))
      (with-handlers-blocked (table :selection-changed)
        (setf (items table) systems))
      (select-system projects (and selected (find (system-key selected) systems
                                                  :key #'system-key :test #'equal))))
    (setf (text (part projects :count))
          (cond ((not (eq (projects-state projects) :loaded)) "")
                ((= (length systems) (length all)) (format nil "~D" (length all)))
                (t (format nil "~D of ~D" (length systems) (length all)))))
    (set-enabled (part projects :filter) (and all t))
    (set-enabled (part projects :refresh) (and (projects-root projects) (not (eq (projects-state projects) :loading))))
    (case (projects-state projects)
      ((:loading :stale) (show-body projects :loading))
      (t (cond (systems (show-body projects :list))
               (t (setf (text (part projects :empty-title)) (if all "No matching systems" "No projects found")
                        (text (part projects :empty-text))
                        (if all
                            "No system name or definition file contains the filter text."
                            (or (projects-warning projects)
                                "Xeon looks for ASDF .asd files, deps.edn or project.clj, and info.rkt in this folder.")))
                  (setf (visible (part projects :empty-open)) (null all))
                  (show-body projects :empty)))))
    (refresh-system-details projects)))

(defun run-status-text (run)
  (case (getf run :status)
    (:running (multiple-value-bind (s m h) (decode-universal-time (getf run :started))
                (declare (ignore s))
                (format nil "Running since ~2,'0D:~2,'0D" h m)))
    (:stopped "Last run was stopped")
    (:exited (if (eql 0 (getf run :code))
                 "Last run finished"
                 (format nil "Last run failed · exit code ~A" (getf run :code))))))

(defun refresh-system-details (projects)
  "Describes the selected system and enables the actions it supports."
  (let* ((system (selected-system projects))
         (run (and system (system-run projects system)))
         (tools (workbench-repl (projects-workbench projects)))
         (status (and tools (repl-status tools)))
         (idle (member status '(:ready :disconnected :error)))
         (lisp-p (and system (eq :common-lisp (xeon.folders:discovered-system-dialect system)))))
    (setf (text (part projects :title)) (if system (xeon.folders:discovered-system-name system) "No system selected")
          (text (part projects :subtitle))
          (if system
              (format nil "~A · ~A" (dialect-label (xeon.folders:discovered-system-dialect system))
                      (system-location projects system))
              "Select a system to load, test, rebuild or run it.")
          (text (part projects :run-status)) (or (and run (run-status-text run)) ""))
    (setf (visible (part projects :run-status)) (and run t))
    (set-enabled (part projects :load) (and system idle))
    (set-enabled (part projects :test) (and system idle))
    (set-enabled (part projects :rebuild) (and lisp-p idle))
    (set-enabled (part projects :run) system)
    (let ((running (and system (system-running-p projects system))))
      (setf (label (part projects :run)) (if running "Stop" "Run")
            (tooltip (part projects :run))
            (if running
                "Stop the running program"
                "Run the system's entry point as its own process; output goes to a log")))))

(defun restyle-projects (workbench)
  "The theme changed: row icons take their colors from it."
  (let ((projects (workbench-projects workbench)))
    (when projects
      (let ((table (part projects :table)))
        (map nil (lambda (system) (refresh-item table system)) (items table))))))

(defun refresh-project-actions (workbench)
  "The REPL's state changed: actions that need an idle REPL follow it."
  (let ((projects (workbench-projects workbench)))
    (when projects (refresh-system-details projects))))

;;; Actions

(defun project-action-verb (action)
  (ecase action (:load "Loading") (:test "Testing") (:recompile "Rebuilding")))

(defun project-repl-action (workbench system action)
  "Loads, tests or rebuilds SYSTEM in the REPL, starting a runtime of its dialect when
none is connected."
  (let* ((tools (workbench-repl workbench))
         (dialect (xeon.folders:discovered-system-dialect system))
         (source (xeon.folders:system-action-source system action))
         (snapshot (repl-snapshot tools))
         (name (xeon.folders:discovered-system-name system)))
    (case (getf snapshot :status)
      (:ready
       (unless (eq dialect (getf snapshot :dialect))
         (error "The REPL runs ~A; disconnect it to work with ~A, a ~A system."
                (dialect-label (getf snapshot :dialect)) name (dialect-label dialect)))
       (xeon.repl:repl-submit (tools-session tools) source)
       (report workbench "~A ~A…" (project-action-verb action) name))
      ((:disconnected :error)
       (choose-repl-dialect tools dialect)
       (start-repl tools)
       (setf (tools-pending tools) (list dialect source name action))
       (report workbench "Starting ~A for ~A…" (dialect-label dialect) name))
      (t (error "The REPL is busy. Wait for it to finish, or interrupt it, and try again.")))
    (select-tool workbench :repl)))

(defun run-pending-project-action (tools state)
  "Submits the action that waited for the runtime to start, once it is ready."
  (let ((pending (tools-pending tools)))
    (when pending
      (destructuring-bind (dialect source name action) pending
        (case state
          (:ready
           (setf (tools-pending tools) nil)
           (when (eq dialect (getf (repl-snapshot tools) :dialect))
             (with-reported-errors ((tools-workbench tools))
               (xeon.repl:repl-submit (tools-session tools) source)
               (report (tools-workbench tools) "~A ~A…" (project-action-verb action) name))))
          (:error
           (setf (tools-pending tools) nil)
           (report (tools-workbench tools) "The runtime did not start, so ~A was not ~A."
                   name (ecase action (:load "loaded") (:test "tested") (:recompile "rebuilt")))))))))

(defun watch-project-run (projects system entry)
  "Waits for a run to end on its own thread, then shows how it ended."
  (let ((backend *backend*)
        (table (part projects :table))
        (workbench (projects-workbench projects)))
    (sb-thread:make-thread
     (lambda ()
       (let* ((*backend* backend)
              (code (ignore-errors (uiop:wait-process (xeon.folders:project-run-process (getf entry :run))))))
         (call-in-ui-thread
          (lambda ()
            (let ((current (system-run projects system)))
              (when (eq (getf current :run) (getf entry :run))
                (let ((stopped (getf current :stopping))
                      (name (xeon.folders:discovered-system-name system)))
                  (setf (gethash (system-key system) (projects-runs projects))
                        (list :run (getf entry :run) :started (getf entry :started)
                              :status (if stopped :stopped :exited) :code code))
                  (unless (destroyed-p table)
                    (refresh-item table system)
                    (refresh-system-details projects))
                  (cond (stopped (report workbench "~A stopped." name))
                        ((eql code 0) (report workbench "~A finished." name))
                        (t (report workbench "~A failed with exit code ~A · Open Run Log for its output."
                                   name code)))))))
          :owner table)))
     :name "Xeon project run")))

(defun run-system (workbench system)
  "Runs SYSTEM's entry point as a separate process, or stops it if it is running."
  (let* ((projects (workbench-projects workbench))
         (entry (system-run projects system))
         (name (xeon.folders:discovered-system-name system)))
    (if (eq :running (getf entry :status))
        (progn (setf (getf (gethash (system-key system) (projects-runs projects)) :stopping) t)
               (uiop:terminate-process (xeon.folders:project-run-process (getf entry :run))))
        (let ((entry (list :run (xeon.folders:launch-project system) :status :running
                           :started (get-universal-time))))
          (setf (gethash (system-key system) (projects-runs projects)) entry)
          (refresh-item (part projects :table) system)
          (refresh-system-details projects)
          (watch-project-run projects system entry)
          (report workbench "Running ~A…" name)))))

(defun open-run-log (workbench system)
  (let ((run (system-run (workbench-projects workbench) system)))
    (unless run (error "~A has not been run yet." (xeon.folders:discovered-system-name system)))
    (open-document workbench (xeon.folders:project-run-log (getf run :run)))))

(defun stop-project-runs (workbench)
  "Stops the programs this window started, as it closes."
  (let ((projects (workbench-projects workbench)))
    (when projects
      (loop for entry being the hash-values of (projects-runs projects)
            when (eq :running (getf entry :status))
              do (ignore-errors
                  (uiop:terminate-process (xeon.folders:project-run-process (getf entry :run))))))))

(defun system-context-menu (workbench projects system)
  (let ((lisp-p (eq :common-lisp (xeon.folders:discovered-system-dialect system)))
        (running (system-running-p projects system))
        (file (xeon.folders:discovered-system-file system)))
    (ui (menu
          (menu-item :label "Load" :on-activate (action (workbench) (project-repl-action workbench system :load)))
          (menu-item :label "Test" :on-activate (action (workbench) (project-repl-action workbench system :test)))
          (menu-item :label "Rebuild" :enabled lisp-p
                     :on-activate (action (workbench) (project-repl-action workbench system :recompile)))
          (menu-separator)
          (menu-item :label (if running "Stop" "Run") :on-activate (action (workbench) (run-system workbench system)))
          (menu-item :label "Open Run Log" :enabled (and (system-run projects system) t)
                     :on-activate (action (workbench) (open-run-log workbench system)))
          (menu-separator)
          (menu-item :label "Open Definition" :on-activate (action (workbench) (open-document workbench file)))
          (menu-item :label "Reveal in Finder"
                     :on-activate (action (workbench) (open-url (uiop:pathname-directory-pathname file))))
          (menu-item :label "Copy System Name"
                     :on-activate (action (workbench)
                                    (setf (clipboard-text) (xeon.folders:discovered-system-name system))))))))

;;; Building

(defun make-empty-page (projects)
  (let* ((workbench (projects-workbench projects))
         (title (make-instance 'label :text "" :font (make-font :size 13 :weight :bold) :alignment :center))
         (text (make-instance 'label :text "" :text-color :secondary-text :wrap t :alignment :center
                                     :font (make-font :size 11)))
         (open (small-button "Open Folder…")))
    (setf (part projects :empty-title) title (part projects :empty-text) text (part projects :empty-open) open)
    (on open :clicked (action (workbench) (choose-and-open-folder workbench)))
    (ui (vbox :padding '(28 20 20 20) :spacing 6
          (hbox (spacer) (:widget (make-instance 'image-view :image (icon :projects :size 32 :color :secondary-text))) (spacer))
          (:widget title)
          (:widget text)
          (hbox :padding '(6 0 0 0) (spacer) (:widget open) (spacer))
          (spacer)))))

(defun make-loading-page (projects)
  (let ((spinner (make-instance 'spinner :active nil)))
    (setf (part projects :spinner) spinner)
    (ui (vbox :padding '(28 20 20 20) :spacing 8
          (hbox (spacer) (:widget spinner) (spacer))
          (label :text "Finding projects…" :text-color :secondary-text :font (make-font :size 11)
                 :alignment :center)
          (spacer)))))

(defun make-projects-panel (workbench)
  (let* ((projects (make-instance 'projects :workbench workbench))
         (heading (make-instance 'label :text "PROJECTS" :font (make-font :size 11 :weight :bold)
                                        :text-color :secondary-text :valign :center))
         (count (make-instance 'label :text "" :font (make-font :size 11) :text-color :secondary-text
                                      :valign :center))
         (filter (make-instance 'search-entry :placeholder "Filter systems" :control-size :small))
         (refresh (make-instance 'button :style :icon :label "Refresh" :tooltip "Find projects again"
                                         :image (icon :refresh :size 16) :control-size :small))
         (table (make-instance 'table-view
                               :headers t :sortable nil
                               :columns (list (make-column "System" #'xeon.folders:discovered-system-name
                                                           :icon-function (lambda (s) (system-icon projects s)))
                                              (make-column "Definition"
                                                           (lambda (s) (system-location projects s))))))
         (body (make-instance 'stack))
         (title (make-instance 'label :text "" :font (make-font :size 13 :weight :bold)))
         (subtitle (hint-label ""))
         (run-status (hint-label ""))
         (load (small-button "Load" :tooltip "Load the system into the REPL"))
         (test (small-button "Test" :tooltip "Run the system's tests in the REPL"))
         (rebuild (small-button "Rebuild" :tooltip "Recompile every file of the system in the REPL"))
         (run (small-button "Run")))
    (setf (workbench-projects workbench) projects
          (part projects :count) count (part projects :filter) filter (part projects :refresh) refresh
          (part projects :table) table (part projects :body) body (part projects :title) title
          (part projects :subtitle) subtitle (part projects :run-status) run-status
          (part projects :load) load (part projects :test) test (part projects :rebuild) rebuild
          (part projects :run) run)
    (let ((loading (make-loading-page projects))
          (empty (make-empty-page projects))
          (list (ui (vbox :spacing 0
                      (:widget table :expand t)
                      (separator)
                      (vbox :padding '(10 12 12 12) :spacing 4
                        (:widget title)
                        (:widget subtitle)
                        (:widget run-status)
                        (hbox :padding '(6 0 0 0) :spacing 6
                          (:widget load) (:widget test) (:widget rebuild) (spacer) (:widget run)))))))
      (setf (part projects :loading) loading (part projects :empty) empty (part projects :list) list)
      (add-page body loading "Loading")
      (add-page body empty "Empty")
      (add-page body list "Systems"))
    (flet ((with-selected (function)
             (action (workbench)
               (let ((system (selected-system projects)))
                 (when system (funcall function system))))))
      (on load :clicked (with-selected (lambda (s) (project-repl-action workbench s :load))))
      (on test :clicked (with-selected (lambda (s) (project-repl-action workbench s :test))))
      (on rebuild :clicked (with-selected (lambda (s) (project-repl-action workbench s :recompile))))
      (on run :clicked (with-selected (lambda (s) (run-system workbench s)))))
    (on refresh :clicked (action (workbench) (discover-projects workbench :force t)))
    (on filter :changed (lambda (e) (declare (ignore e)) (refresh-projects projects)))
    (on table :selection-changed (lambda (e) (declare (ignore e)) (refresh-system-details projects)))
    (on table :row-activated
        (lambda (e)
          (with-reported-errors (workbench)
            (open-document workbench (xeon.folders:discovered-system-file (event-item e))))))
    (on table :context-request
        (lambda (e)
          (let ((system (item-at-point table (event-x e) (event-y e))))
            (when (typep system 'xeon.folders::discovered-system)
              (select-system projects system)
              (refresh-system-details projects)
              (popup-menu (system-context-menu workbench projects system) table (event-x e) (event-y e))
              (consume e)))))
    (refresh-projects projects)
    (ui (vbox :spacing 0
          (hbox :padding '(10 12 6 12) :spacing 6 (:widget heading) (spacer) (:widget count))
          (hbox :padding '(0 8 8 12) :spacing 6 (:widget filter :expand t) (:widget refresh))
          (separator)
          (:widget body :expand t)))))
