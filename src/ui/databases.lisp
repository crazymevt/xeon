(in-package #:xeon.ui)

;;; Databases. The sidebar lists the stored connections. A database tab lists its
;;; tables and views and shows the rows of the one selected; a query tab is an SQL
;;; editor over a result grid. Each connection's statements run one at a time on its
;;; own worker thread (xeon.databases); results come back to the UI thread as calls,
;;; so nothing here polls. The connections file is read when the view is first shown.

(defvar *database-store-file* nil
  "The stored connections file, or NIL for databases.sexp in Xeon's configuration directory.")

(defun database-store-path ()
  (or *database-store-file*
      (merge-pathnames "databases.sexp" (xeon.themes:default-configuration-directory))))

(defparameter *result-row-limit* 1000
  "The most rows a table or query shows; more are left unread.")

(defclass databases ()
  ((workbench :initarg :workbench :reader databases-workbench)
   (store :initform nil :accessor databases-store)
   ;; Profile id -> its XEON.DATABASES:DATABASE-WORKER, while connected.
   (workers :initform (make-hash-table :test #'equal) :reader databases-workers)
   (widgets :initform (make-hash-table) :reader databases-widgets)))

(defun profiles (databases)
  (let ((store (databases-store databases)))
    (and store (xeon.databases:connection-store-profiles store))))

(defun profile-location (profile)
  "The database file, with the home folder shortened to ~."
  (let ((path (xeon.databases:profile-path profile))
        (home (uiop:native-namestring (user-homedir-pathname))))
    (if (and (> (length path) (length home)) (string= home path :end2 (length home)))
        (concatenate 'string "~/" (subseq path (length home)))
        path)))

(defun profile-connected-p (databases profile)
  (let ((worker (gethash (xeon.databases:profile-id profile) (databases-workers databases))))
    (and worker (not (xeon.databases:database-worker-stopped-p worker)))))

(defun profile-icon (databases profile)
  (if (profile-connected-p databases profile)
      (icon :database :size 16 :color :system-green)
      (icon :database :size 16 :color :secondary-text)))

;;; Workers

(defun profile-worker (databases profile)
  "The worker for PROFILE's database, started on first use."
  (let* ((id (xeon.databases:profile-id profile))
         (worker (gethash id (databases-workers databases))))
    (if (and worker (not (xeon.databases:database-worker-stopped-p worker)))
        worker
        (prog1 (setf (gethash id (databases-workers databases))
                     (xeon.databases:make-database-worker
                      (xeon.databases:profile-pathname profile)
                      :read-only (xeon.databases:profile-read-only-p profile)
                      :name (format nil "Xeon database ~A" (xeon.databases:profile-name profile))))
          (refresh-profile-row databases profile)
          (refresh-profile-details databases)))))

(defun run-database-job (databases profile owner function on-done)
  "Runs FUNCTION with PROFILE's connection on its worker, then ON-DONE with its value and
NIL, or NIL and an error message, on the UI thread, unless OWNER is gone by then."
  (let ((backend *backend*))
    (xeon.databases:submit-database-job
     (profile-worker databases profile) function
     (lambda (value error)
       (let ((*backend* backend))
         (call-in-ui-thread (lambda () (funcall on-done value error)) :owner owner))))))

(defun disconnect-profile (databases profile)
  (let ((worker (gethash (xeon.databases:profile-id profile) (databases-workers databases))))
    (when worker
      (xeon.databases:stop-database-worker worker)
      (remhash (xeon.databases:profile-id profile) (databases-workers databases))
      (refresh-profile-row databases profile)
      (refresh-profile-details databases))))

(defun stop-database-workers (workbench)
  "Closes every connection, as the window closes."
  (let ((databases (workbench-databases workbench)))
    (when databases
      (loop for worker being the hash-values of (databases-workers databases)
            do (xeon.databases:stop-database-worker worker))
      (clrhash (databases-workers databases)))))

;;; Result grids, shared by database and query tabs

(defclass result-grid ()
  ((widgets :initform (make-hash-table) :reader grid-widgets)
   (result :initform nil :accessor grid-result)
   (root :accessor grid-root)))

(defun display-text (string)
  "STRING on one line, bounded for a cell."
  (let ((line (substitute-if #\Space (lambda (c) (member c '(#\Newline #\Return #\Tab))) string)))
    (if (> (length line) 400) (concatenate 'string (subseq line 0 400) "…") line)))

(defun cell-label (value)
  (cond ((eq value :null) "NULL")
        ((eq value :blob) "BLOB")
        ((floatp value) (let ((*read-default-float-format* 'double-float)) (princ-to-string value)))
        ((stringp value) (display-text value))
        (t (princ-to-string value))))

(defun cell-copy-text (value)
  (cond ((eq value :null) "")
        ((stringp value) value)
        (t (cell-label value))))

(defun result-column (name index rows)
  "A grid column for the INDEXth value of each row: sized to its title and the first rows,
and right-aligned when it holds numbers."
  (let ((characters (+ 2 (length name))) (numeric nil) (text nil))
    (loop for row across rows
          repeat 100
          do (let ((value (svref row index)))
               (setf characters (max characters (length (cell-label value))))
               (cond ((realp value) (setf numeric t))
                     ((stringp value) (setf text t)))))
    (make-column name (lambda (row) (svref row index))
                 :formatter #'cell-label
                 :alignment (if (and numeric (not text)) :right :left)
                 :width (+ 18 (* 7 (min 48 (max 5 characters)))))))

(defun grid-page (grid page)
  (let ((spinner (part grid :spinner)))
    (unless (eq (and (active spinner) t) (eq page :loading))
      (setf (active spinner) (eq page :loading))))
  (select-page (part grid :body) (part grid page)))

(defun show-grid-placeholder (grid title text)
  (setf (grid-result grid) nil
        (text (part grid :placeholder-title)) title
        (text (part grid :placeholder-text)) text)
  (grid-page grid :placeholder))

(defun show-grid-loading (grid text)
  (setf (text (part grid :loading-text)) text)
  (grid-page grid :loading))

(defun show-grid-error (grid message)
  (setf (grid-result grid) nil)
  (let ((stopped (search "interrupted" message)))
    (setf (text (part grid :error-title)) (if stopped "Query stopped" "The statement failed")
          (text (part grid :error-text))
          (if stopped "The statement was stopped before it finished; changes it made were rolled back." message)))
  (grid-page grid :error))

(defun result-summary (result)
  (let ((rows (length (xeon.databases:result-rows result)))
        (statements (xeon.databases:result-statements result))
        (changes (xeon.databases:result-changes result))
        (ms (round (* 1000 (xeon.databases:result-seconds result)))))
    (format nil "~{~A~^ · ~}"
            (remove nil
                    (list (when (xeon.databases:result-columns result)
                            (format nil "~:D row~:P~:[~; (more not shown)~]"
                                    rows (xeon.databases:result-truncated-p result)))
                          (when (plusp changes) (format nil "~:D row~:P changed" changes))
                          (when (> statements 1) (format nil "~D statements" statements))
                          (format nil "~:D ms" ms))))))

(defun show-grid-result (grid result)
  (setf (grid-result grid) result)
  (let ((columns (xeon.databases:result-columns result)))
    (if columns
        (let ((table (part grid :table))
              (rows (xeon.databases:result-rows result)))
          (with-handlers-blocked (table :selection-changed)
            (setf (items table) #()
                  (columns table) (loop for name in columns for i from 0
                                        collect (result-column name i rows))
                  (items table) rows))
          (setf (text (part grid :footer)) (result-summary result))
          (grid-page grid :table-page))
        (let ((changes (xeon.databases:result-changes result)))
          (setf (text (part grid :message-title))
                (if (plusp changes) (format nil "~:D row~:P changed" changes) "Statement ran")
                (text (part grid :message-text)) (result-summary result))
          (grid-page grid :message)))))

(defun copy-rows (grid rows &key headers)
  (let ((result (grid-result grid)))
    (when result
      (setf (clipboard-text)
            (with-output-to-string (out)
              (when headers (format out "~{~A~^	~}~%" (xeon.databases:result-columns result)))
              (loop for row across (coerce rows 'vector)
                    do (format out "~{~A~^	~}~%" (map 'list #'cell-copy-text row))))))))

(defun grid-context-menu (grid)
  (let ((table (part grid :table)))
    (ui (menu
          (menu-item :label "Copy Rows" :enabled (and (selected-items table) t)
                     :on-activate (lambda (e) (declare (ignore e)) (copy-rows grid (selected-items table))))
          (menu-item :label "Copy All Rows with Column Names"
                     :on-activate (lambda (e) (declare (ignore e))
                                    (copy-rows grid (xeon.databases:result-rows (grid-result grid)) :headers t)))))))

(defun centered-message (icon-name title text &rest widgets)
  (ui (vbox :padding '(28 24 24 24) :spacing 6
        (spacer)
        (hbox (spacer) (:widget (make-instance 'image-view :image (icon icon-name :size 32 :color :secondary-text))) (spacer))
        (:widget title)
        (:widget text)
        (:widget (let ((box (make-instance 'hbox :spacing 8 :padding '(6 0 0 0))))
                   (add box (make-instance 'spacer))
                   (dolist (w widgets) (add box w))
                   (add box (make-instance 'spacer))
                   box))
        (spacer))))

(defun message-title () (make-instance 'label :text "" :font (make-font :size 13 :weight :bold) :alignment :center))
(defun message-text (&rest initargs)
  (apply #'make-instance 'label :text "" :text-color :secondary-text :wrap t :alignment :center
                                :font (make-font :size 11) initargs))

(defun make-result-grid ()
  (let* ((grid (make-instance 'result-grid))
         (body (make-instance 'stack))
         (spinner (make-instance 'spinner :active nil))
         (table (make-instance 'table-view :headers t :sortable t :selection-mode :multiple))
         (footer (make-instance 'label :text "" :text-color :secondary-text :font (make-font :size 11)
                                       :valign :center)))
    (flet ((keep (name widget) (setf (part grid name) widget)))
      (keep :body body) (keep :spinner spinner) (keep :table table) (keep :footer footer)
      (let ((placeholder (centered-message :table (keep :placeholder-title (message-title))
                                           (keep :placeholder-text (message-text))))
            (loading (ui (vbox :padding '(28 24 24 24) :spacing 8
                           (spacer)
                           (hbox (spacer) (:widget spinner) (spacer))
                           (:widget (keep :loading-text (message-text)))
                           (spacer))))
            (failure (centered-message :error (keep :error-title (message-title))
                                       (keep :error-text (message-text :selectable t))))
            (message (centered-message :check (keep :message-title (message-title))
                                       (keep :message-text (message-text))))
            (rows (ui (vbox :spacing 0
                        (:widget table :expand t)
                        (separator)
                        (hbox :padding '(4 12 4 12) (:widget footer) (spacer))))))
        (keep :placeholder placeholder) (keep :loading loading) (keep :error failure)
        (keep :message message) (keep :table-page rows)
        (add-page body placeholder "Empty") (add-page body loading "Running")
        (add-page body failure "Error") (add-page body message "Done") (add-page body rows "Rows")))
    (on table :context-request
        (lambda (e)
          (when (grid-result grid)
            (let ((row (item-at-point table (event-x e) (event-y e))))
              (when (and row (not (member row (selected-items table))))
                (setf (selected-indices table) (list (position row (items table))))))
            (popup-menu (grid-context-menu grid) table (event-x e) (event-y e))
            (consume e))))
    (setf (grid-root grid) body)
    grid))

;;; Database tabs: the tables and views, and the rows of the one selected

(defclass database-page (vbox)
  ((workbench :initarg :workbench :reader page-workbench)
   (profile :initarg :profile :reader page-profile)
   (objects :initform '() :accessor page-objects)
   ;; :loading, :loaded or :failed, with the failure's message.
   (state :initform :loading :accessor page-state)
   (failure :initform nil :accessor page-failure)
   ;; The table or view whose rows are shown; filtering the list may hide it.
   (shown :initform nil :accessor page-shown-object)
   (generation :initform 0 :accessor page-generation)
   (grid :accessor page-grid)
   (widgets :initform (make-hash-table) :reader page-widgets))
  (:default-initargs :spacing 0))

(defun database-pages (workbench &optional profile)
  (remove-if-not (lambda (page) (and (typep page '(or database-page query-page))
                                     (or (null profile) (eq profile (page-profile page)))))
                 (all-tab-pages workbench)))

(defun object-name (object) (car object))
(defun object-kind (object) (cdr object))

(defun object-icon (workbench object)
  (file-icon (workbench-palette workbench) (object-kind object)))

(defun selected-object (page)
  (let* ((table (part page :objects))
         (index (first (selected-indices table)))
         (items (items table)))
    (and index (< index (length items)) (svref items index))))

(defun select-object (page object)
  (let* ((table (part page :objects))
         (index (and object (position object (items table) :test #'equal))))
    (with-handlers-blocked (table :selection-changed)
      (setf (selected-indices table) (if index (list index) '())))))

(defun filtered-objects (page)
  (let ((filter (string-trim " " (text (part page :filter)))))
    (if (zerop (length filter))
        (page-objects page)
        (remove-if-not (lambda (o) (search filter (object-name o) :test #'char-equal)) (page-objects page)))))

(defun profile-subtitle (profile)
  (format nil "SQLite · ~A~:[~; · Read only~]" (profile-location profile)
          (xeon.databases:profile-read-only-p profile)))

(defun refresh-database-header (page)
  (let ((profile (page-profile page)))
    (setf (text (part page :title)) (xeon.databases:profile-name profile)
          (text (part page :subtitle)) (profile-subtitle profile))))

(defun refresh-object-list (page)
  "Brings the list, its count and its empty states up to date."
  (let* ((table (part page :objects))
         (selected (page-shown-object page))
         (shown (filtered-objects page))
         (all (page-objects page))
         (tables (count :table all :key #'object-kind))
         (views (count :view all :key #'object-kind)))
    (unless (equal shown (coerce (items table) 'list))
      (with-handlers-blocked (table :selection-changed)
        (setf (items table) shown))
      ;; The shown table stays selected whenever the filter lets it be listed.
      (select-object page (and selected (find selected shown :test #'equal))))
    (setf (text (part page :count))
          (if (eq (page-state page) :loaded)
              (format nil "~:D table~:P~:[~*~; · ~:D view~:P~]~:[~*~; · ~:D shown~]"
                      tables (plusp views) views (/= (length shown) (length all)) (length shown))
              ""))
    (set-enabled (part page :filter) (and all t))
    (set-enabled (part page :refresh) (not (eq (page-state page) :loading)))
    (let ((spinner (part page :spinner)))
      (unless (eq (and (active spinner) t) (eq (page-state page) :loading))
        (setf (active spinner) (eq (page-state page) :loading))))
    (select-page (part page :body)
                 (cond ((eq (page-state page) :loading) (part page :loading))
                       (shown (part page :list))
                       (t (setf (text (part page :empty-title))
                                (cond ((eq (page-state page) :failed) "Cannot open the database")
                                      (all "No matching tables")
                                      (t "No tables"))
                                (text (part page :empty-text))
                                (cond ((eq (page-state page) :failed) (page-failure page))
                                      (all "No table or view name contains the filter text.")
                                      (t "Create one with a query.")))
                          (part page :empty))))))

(defun show-object-rows (page object &key reload)
  "Shows OBJECT's rows, reading them on the connection's worker."
  (let ((grid (page-grid page))
        (generation (incf (page-generation page)))
        (workbench (page-workbench page)))
    (setf (page-shown-object page) object)
    (setf (text (part page :object-title)) (if object (object-name object) "")
          (image (part page :object-icon)) (and object (object-icon workbench object)))
    (set-enabled (part page :edit-query) object)
    (cond ((null object)
           (show-grid-placeholder grid "Select a table"
                                  "Choose a table or view on the left to see its rows."))
          (t
           (unless reload (show-grid-loading grid (format nil "Reading ~A…" (object-name object))))
           (let ((sql (xeon.databases:table-select-sql (object-name object))))
             (run-database-job (workbench-databases workbench) (page-profile page) page
                               (lambda (connection)
                                 (xeon.databases:execute connection sql :limit *result-row-limit*))
                               (lambda (result error)
                                 (when (= generation (page-generation page))
                                   (if error
                                       (show-grid-error grid error)
                                       (show-grid-result grid result))))))))))

(defun refresh-database-page (page &key (rows t))
  "Reads the tables and views again; with ROWS, the selected one's rows too."
  (let ((workbench (page-workbench page)))
    (setf (page-state page) :loading)
    (refresh-object-list page)
    (run-database-job (workbench-databases workbench) (page-profile page) page
                      #'xeon.databases:schema-objects
                      (lambda (objects error)
                        (let ((shown (page-shown-object page)))
                          (setf (page-objects page) (if error '() objects)
                                (page-state page) (if error :failed :loaded)
                                (page-failure page) error)
                          (refresh-object-list page)
                          (let ((still (and shown (find shown objects :test #'equal))))
                            (cond ((null still) (show-object-rows page nil))
                                  (rows (show-object-rows page still :reload t)))))))))

(defun make-database-list-pages (page)
  (let* ((spinner (make-instance 'spinner :active nil))
         (loading (ui (vbox :padding '(28 16 16 16) :spacing 8
                        (hbox (spacer) (:widget spinner) (spacer))
                        (label :text "Reading tables…" :text-color :secondary-text :font (make-font :size 11)
                               :alignment :center)
                        (spacer))))
         (title (message-title))
         (text (message-text :selectable t))
         (empty (ui (vbox :padding '(28 16 16 16) :spacing 6
                      (hbox (spacer) (:widget (make-instance 'image-view :image (icon :table :size 28 :color :secondary-text))) (spacer))
                      (:widget title) (:widget text) (spacer)))))
    (setf (part page :spinner) spinner (part page :loading) loading
          (part page :empty-title) title (part page :empty-text) text (part page :empty) empty)
    (values loading empty)))

(defun make-database-page (workbench profile)
  (let* ((page (make-instance 'database-page :workbench workbench :profile profile))
         (grid (make-result-grid))
         (title (make-instance 'label :text "" :font (make-font :size 13 :weight :bold)))
         (subtitle (hint-label ""))
         (refresh (make-instance 'button :style :icon :label "Refresh" :tooltip "Read the tables and rows again"
                                         :image (icon :refresh :size 16) :control-size :small))
         (new-query (small-button "New Query" :image (icon :query :size 14)
                                              :tooltip "Open an SQL editor for this database"))
         (filter (make-instance 'search-entry :placeholder "Filter tables" :control-size :small))
         (count (make-instance 'label :text "" :font (make-font :size 11) :text-color :secondary-text))
         (objects (make-instance 'table-view
                                 :headers nil :sortable nil :alternating nil
                                 :columns (list (make-column "Name" #'object-name
                                                             :icon-function (lambda (o) (object-icon workbench o))))))
         (body (make-instance 'stack))
         (object-icon (make-instance 'image-view :image nil))
         (object-title (make-instance 'label :text "" :font (make-font :size 12 :weight :bold) :valign :center))
         (edit-query (small-button "Edit as Query" :tooltip "Open this table's query in an SQL editor")))
    (setf (page-grid page) grid)
    (flet ((keep (name widget) (setf (part page name) widget)))
      (keep :title title) (keep :subtitle subtitle) (keep :refresh refresh) (keep :filter filter)
      (keep :count count) (keep :objects objects) (keep :body body) (keep :object-icon object-icon)
      (keep :object-title object-title) (keep :edit-query edit-query) (keep :list objects))
    (multiple-value-bind (loading empty) (make-database-list-pages page)
      (add-page body loading "Loading") (add-page body empty "Empty") (add-page body objects "Tables"))
    (add page (ui (hbox :padding '(10 14 10 14) :spacing 10
                    (vbox :spacing 2 (:widget title) (:widget subtitle))
                    (spacer)
                    (:widget refresh) (:widget new-query))))
    (add page (make-instance 'separator))
    (add page (setf (part page :split) (ui (paned
                    (vbox :spacing 0
                      (hbox :padding '(8 8 8 8) (:widget filter :expand t))
                      (:widget body :expand t)
                      (separator)
                      (hbox :padding '(4 10 4 10) (:widget count) (spacer)))
                    (vbox :spacing 0
                      (hbox :padding '(6 12 6 12) :spacing 6
                        (:widget object-icon) (:widget object-title) (spacer) (:widget edit-query))
                      (separator)
                      (:widget (grid-root grid) :expand t)))))
         :expand t)
    (on refresh :clicked (action (workbench) (refresh-database-page page)))
    (on new-query :clicked (action (workbench) (open-query workbench profile)))
    (on edit-query :clicked
        (action (workbench)
          (let ((object (page-shown-object page)))
            (when object
              (open-query workbench profile
                          :text (format nil "~A~%LIMIT ~D;" (xeon.databases:table-select-sql (object-name object))
                                        *result-row-limit*))))))
    (on filter :changed (lambda (e) (declare (ignore e)) (refresh-object-list page)))
    (on objects :selection-changed
        (lambda (e) (declare (ignore e)) (show-object-rows page (selected-object page))))
    (refresh-database-header page)
    (show-object-rows page nil)
    page))

(defun add-database-tab (workbench page label glyph divider)
  "Adds a database or query PAGE as a tab, in place of an empty untitled document, and
puts its divider DIVIDER points in once it has a size."
  (let ((tabs (workbench-tabs workbench))
        (scratch (disposable-document workbench)))
    (add tabs page :tab-label label
                   :tab-tooltip (xeon.databases:profile-path (page-profile page))
                   :tab-icon (icon glyph :size 14))
    (select-page tabs page)
    (when scratch (close-document workbench scratch t))
    (setf (divider-position (part page :split)) divider)))

(defun open-database (workbench profile)
  "Shows PROFILE's database tab, opening it the first time."
  (let ((page (find-if (lambda (p) (typep p 'database-page)) (database-pages workbench profile))))
    (unless page
      (setf page (make-database-page workbench profile))
      (add-database-tab workbench page (xeon.databases:profile-name profile) :database 240)
      (refresh-database-page page))
    (show-page workbench page)
    (focus (part page :objects))
    page))

;;; Query tabs: an SQL editor over a result grid

(defclass query-page (vbox)
  ((workbench :initarg :workbench :reader page-workbench)
   (profile :initarg :profile :reader page-profile)
   (document :initarg :document :reader query-document)
   (editor :accessor query-editor)
   (grid :accessor page-grid)
   (running :initform nil :accessor query-running-p)
   (generation :initform 0 :accessor page-generation)
   (widgets :initform (make-hash-table) :reader page-widgets))
  (:default-initargs :spacing 0))

(defclass query-editor (editor-view)
  ((page :initarg :page :reader editor-query-page)))

(defmethod handle-event ((view query-editor) (event key-event))
  ;; Primary+Return runs; the menu shortcut does the same where menus take keys first.
  (when (and (eq (event-signal event) :key-down) (not (event-consumed-p event))
             (eq (event-key event) :return) (modifier-down-p event :primary)
             (not (modifier-down-p event :shift)))
    (let ((page (editor-query-page view)))
      (with-reported-errors ((page-workbench page)) (run-query page)))
    (consume event))
  (call-next-method))

(defun active-query (workbench)
  (let ((page (selected-page (workbench-tabs workbench))))
    (and page (typep page 'query-page) page)))

(defun query-source (page &key all)
  "The selected SQL, or all of it."
  (let* ((view (query-editor page))
         (selected (if all "" (selected-text view))))
    (if (blank-p selected) (buffer-text (buffer view)) selected)))

(defun refresh-query-actions (page)
  (let ((running (query-running-p page)))
    (set-enabled (part page :run) (not running))
    (set-enabled (part page :stop) running)))

(defun run-query (page &key all)
  "Runs the selected statements, or with ALL or no selection every statement, and shows
what they return."
  (let ((sql (query-source page :all all))
        (workbench (page-workbench page))
        (grid (page-grid page)))
    (when (blank-p sql) (error "Write an SQL statement to run."))
    (when (query-running-p page) (error "A query is already running. Stop it, or wait for it to finish."))
    (let ((generation (incf (page-generation page)))
          (profile (page-profile page)))
      (setf (query-running-p page) t)
      (refresh-query-actions page)
      (show-grid-loading grid "Running…")
      (run-database-job (workbench-databases workbench) profile page
                        (lambda (connection)
                          (xeon.databases:execute connection sql :limit *result-row-limit*))
                        (lambda (result error)
                          (when (= generation (page-generation page))
                            (setf (query-running-p page) nil)
                            (refresh-query-actions page)
                            (cond (error (show-grid-error grid error))
                                  (t (show-grid-result grid result)
                                     ;; The statements may have changed the tables.
                                     (dolist (other (database-pages workbench profile))
                                       (when (typep other 'database-page)
                                         (refresh-database-page other :rows nil)))))))))))

(defun stop-query (page)
  (when (query-running-p page)
    (let ((worker (gethash (xeon.databases:profile-id (page-profile page))
                           (databases-workers (workbench-databases (page-workbench page))))))
      (when worker (xeon.databases:interrupt-database-worker worker)))))

(defun query-tab-label (profile) (format nil "~A query" (xeon.databases:profile-name profile)))

(defun make-query-page (workbench profile text)
  (let* ((buffer (xeon.core:make-buffer :name "query.sql" :text text :history-limit 0))
         (document (progn (xeon.syntax:set-buffer-language buffer :sql)
                          (make-instance 'document :workbench workbench :buffer buffer)))
         (page (make-instance 'query-page :workbench workbench :profile profile :document document))
         (themes (workbench-themes workbench))
         (editor (make-instance 'query-editor
                                :page page :document document :buffer (document-text document)
                                :font (editor-font themes) :tab-width (xeon.themes:editor-tab-size themes)
                                :text-styles (syntax-text-styles (workbench-palette workbench))))
         (grid (make-result-grid))
         (title (make-instance 'label :text (xeon.databases:profile-name profile) :font (make-font :size 13 :weight :bold)))
         (subtitle (hint-label (profile-subtitle profile)))
         (run (small-button "Run" :image (icon :play :size 14)
                                  :tooltip (format nil "Run the selection, or every statement · ~A"
                                                   (shortcut-label "Primary+Return"))))
         (stop (small-button "Stop" :image (icon :stop :size 14) :enabled nil
                                    :tooltip "Stop the running statement")))
    (setf (query-editor page) editor (page-grid page) grid
          (part page :title) title (part page :subtitle) subtitle (part page :run) run (part page :stop) stop)
    (push editor (document-views document))
    (restart-highlighting document)
    (add page (ui (hbox :padding '(10 14 10 14) :spacing 8
                    (vbox :spacing 2 (:widget title) (:widget subtitle))
                    (spacer)
                    (:widget stop) (:widget run))))
    (add page (make-instance 'separator))
    (add page (setf (part page :split) (ui (paned :orientation :vertical
                                             (:widget editor)
                                             (:widget (grid-root grid)))))
         :expand t)
    (on run :clicked (action (workbench) (run-query page)))
    (on stop :clicked (action (workbench) (stop-query page)))
    (show-grid-placeholder grid "No results yet"
                           (format nil "Write SQL above and press ~A to run it." (shortcut-label "Primary+Return")))
    page))

(defun open-query (workbench profile &key (text ""))
  "Opens a new query tab on PROFILE's database."
  (let ((page (make-query-page workbench profile text))
        (tabs (workbench-tabs workbench)))
    (add-database-tab workbench page (query-tab-label profile) :query 220)
    (select-page tabs page)
    (let ((editor (query-editor page)))
      (focus editor)
      (set-selection editor (length text) (length text)))
    page))

(defun release-database-page (page)
  "A database or query tab closed."
  (incf (page-generation page))
  (when (typep page 'query-page)
    (stop-query page)
    (let ((d (query-document page)))
      (release-editor-view (query-editor page))
      (setf (document-views d) '())
      (release-document d))))

;;; The sidebar view

(defun ensure-databases-loaded (workbench)
  "Reads the stored connections the first time the view is shown."
  (let ((databases (workbench-databases workbench)))
    (when (and databases (null (databases-store databases)))
      (let ((store (xeon.databases:load-connection-store (database-store-path))))
        (setf (databases-store databases) store)
        (refresh-databases databases)
        (let ((warning (xeon.databases:connection-store-warning store)))
          (when warning (report workbench "~A" warning)))))))

(defun filtered-profiles (databases)
  (let ((filter (string-trim " " (text (part databases :filter)))))
    (if (zerop (length filter))
        (profiles databases)
        (remove-if-not (lambda (p)
                         (or (search filter (xeon.databases:profile-name p) :test #'char-equal)
                             (search filter (xeon.databases:profile-path p) :test #'char-equal)))
                       (profiles databases)))))

(defun selected-profile (databases)
  (let* ((table (part databases :table))
         (index (first (selected-indices table)))
         (items (items table)))
    (and index (< index (length items)) (svref items index))))

(defun select-profile (databases profile)
  (let* ((table (part databases :table))
         (index (and profile (position profile (items table)))))
    (with-handlers-blocked (table :selection-changed)
      (setf (selected-indices table) (if index (list index) '())))))

(defun refresh-profile-row (databases profile)
  (let ((table (part databases :table)))
    (when (find profile (items table)) (refresh-item table profile))))

(defun refresh-databases (databases)
  "Brings the list, count and empty states up to date with the stored connections."
  (let* ((table (part databases :table))
         (selected (selected-profile databases))
         (shown (filtered-profiles databases))
         (all (profiles databases)))
    (unless (equal shown (coerce (items table) 'list))
      (with-handlers-blocked (table :selection-changed)
        (setf (items table) shown))
      (select-profile databases (and selected (find selected shown))))
    (setf (text (part databases :count))
          (cond ((null all) "")
                ((= (length shown) (length all)) (format nil "~D" (length all)))
                (t (format nil "~D of ~D" (length shown) (length all)))))
    (set-enabled (part databases :filter) (and all t))
    (cond (shown (select-page (part databases :body) (part databases :list)))
          (t (setf (text (part databases :empty-title)) (if all "No matching databases" "No databases")
                   (text (part databases :empty-text))
                   (if all
                       "No connection name or file contains the filter text."
                       "Add a SQLite database file to browse its tables and run queries."))
             (setf (visible (part databases :empty-actions)) (null all))
             (select-page (part databases :body) (part databases :empty))))
    (refresh-profile-details databases)))

(defun refresh-profile-details (databases)
  (let ((profile (selected-profile databases)))
    (setf (text (part databases :title))
          (if profile (xeon.databases:profile-name profile) "No database selected")
          (text (part databases :subtitle))
          (cond ((null profile) "Select a database to browse its tables or query it.")
                ((not (probe-file (xeon.databases:profile-pathname profile)))
                 (format nil "~A · File not found" (profile-location profile)))
                (t (format nil "~A · ~:[Disconnected~;Connected~]" (profile-subtitle profile)
                           (profile-connected-p databases profile)))))
    (set-enabled (part databases :open) profile)
    (set-enabled (part databases :query) profile)))

(defun restyle-databases (workbench)
  "The theme or editor settings changed: icons and query editors follow."
  (let ((databases (workbench-databases workbench)))
    (when databases
      (let ((table (part databases :table)))
        (map nil (lambda (p) (refresh-item table p)) (items table)))
      (let ((themes (workbench-themes workbench))
            (palette (workbench-palette workbench)))
        (dolist (page (database-pages workbench))
          (etypecase page
            (database-page
             (let ((objects (part page :objects)))
               (map nil (lambda (o) (refresh-item objects o)) (items objects)))
             (let ((object (page-shown-object page)))
               (setf (image (part page :object-icon)) (and object (object-icon workbench object)))))
            (query-page
             (let ((view (query-editor page)))
               (setf (text-styles view) (syntax-text-styles palette)
                     (font view) (editor-font themes)
                     (tab-width view) (xeon.themes:editor-tab-size themes))))))))))

(defun show-profile (workbench profile)
  "Shows the Databases view with PROFILE selected."
  (let ((databases (workbench-databases workbench)))
    (show-sidebar-view workbench :databases)
    (setf (text (part databases :filter)) "")
    (refresh-databases databases)
    (select-profile databases profile)
    (refresh-profile-details databases)))

(defun add-database-file (workbench path &key create)
  "Stores a connection to the SQLite file at PATH, creating the database with CREATE, then
opens it."
  (ensure-databases-loaded workbench)
  (when create
    (when (probe-file path)
      (error "~A already exists. Add it with Add Database… instead." (file-namestring path)))
    (xeon.databases:disconnect (xeon.databases:connect path :create t)))
  (multiple-value-bind (profile existing)
      (xeon.databases:add-connection-profile (databases-store (workbench-databases workbench)) path)
    (show-profile workbench profile)
    (open-database workbench profile)
    (report workbench (if existing "~A was already added." (if create "Created ~A." "Added ~A."))
            (xeon.databases:profile-name profile))
    profile))

(defun choose-and-add-database (workbench)
  (let ((path (choose-file :mode :open :title "Add a SQLite Database"
                           :parent (workbench-window workbench) :directory (explorer-folder workbench))))
    (when path (add-database-file workbench path))))

(defun choose-and-create-database (workbench)
  (let ((path (choose-file :mode :save :title "New SQLite Database" :name "database.sqlite"
                           :parent (workbench-window workbench) :directory (explorer-folder workbench))))
    (when path (add-database-file workbench path :create t))))

(defun rename-profile (workbench profile)
  (let ((name (prompt-text "Rename the connection"
                           :detail (format nil "The name Xeon shows for ~A." (profile-location profile))
                           :default (xeon.databases:profile-name profile) :ok "Rename"
                           :parent (workbench-window workbench))))
    (when (and name (not (blank-p name)))
      (let ((databases (workbench-databases workbench)))
        (xeon.databases:rename-connection-profile (databases-store databases) profile name)
        (refresh-profile-row databases profile)
        (refresh-profile-details databases)
        (dolist (page (database-pages workbench profile))
          (etypecase page
            (database-page (refresh-database-header page)
             (setf (tab-label (parent page) page) (xeon.databases:profile-name profile)))
            (query-page (setf (text (part page :title)) (xeon.databases:profile-name profile)
                              (tab-label (parent page) page) (query-tab-label profile)))))
        (update-title workbench)))))

(defun remove-profile (workbench profile)
  (when (confirm (format nil "Remove “~A” from Xeon?" (xeon.databases:profile-name profile))
                 :detail "Its tabs close. The database file is not deleted."
                 :yes "Remove" :parent (workbench-window workbench))
    (let ((databases (workbench-databases workbench)))
      (dolist (page (database-pages workbench profile))
        (close-page (parent page) page :force t))
      (disconnect-profile databases profile)
      (xeon.databases:remove-connection-profile (databases-store databases) profile)
      (refresh-databases databases)
      (report workbench "Removed ~A." (xeon.databases:profile-name profile)))))

(defun profile-context-menu (workbench databases profile)
  (let ((path (xeon.databases:profile-pathname profile)))
    (ui (menu
          (menu-item :label "Open" :on-activate (action (workbench) (open-database workbench profile)))
          (menu-item :label "New Query" :on-activate (action (workbench) (open-query workbench profile)))
          (menu-separator)
          (menu-item :label "Rename…" :on-activate (action (workbench) (rename-profile workbench profile)))
          (menu-item :label "Disconnect" :enabled (profile-connected-p databases profile)
                     :on-activate (action (workbench) (disconnect-profile databases profile)))
          (menu-separator)
          (menu-item :label "Copy Path"
                     :on-activate (action (workbench) (setf (clipboard-text) (xeon.databases:profile-path profile))))
          (menu-item :label "Reveal in Finder"
                     :on-activate (action (workbench) (open-url (uiop:pathname-directory-pathname path))))
          (menu-separator)
          (menu-item :label "Remove Connection…"
                     :on-activate (action (workbench) (remove-profile workbench profile)))))))

(defun add-database-menu (workbench)
  (ui (menu
        (menu-item :label "Add SQLite Database…" :on-activate (action (workbench) (choose-and-add-database workbench)))
        (menu-item :label "New SQLite Database…" :on-activate (action (workbench) (choose-and-create-database workbench))))))

(defun make-databases-panel (workbench)
  (let* ((databases (make-instance 'databases :workbench workbench))
         (heading (make-instance 'label :text "DATABASES" :font (make-font :size 11 :weight :bold)
                                        :text-color :secondary-text :valign :center))
         (count (make-instance 'label :text "" :font (make-font :size 11) :text-color :secondary-text
                                      :valign :center))
         (add (make-instance 'button :style :icon :label "Add Database" :tooltip "Add or create a database"
                                     :image (icon :add :size 16) :control-size :small))
         (filter (make-instance 'search-entry :placeholder "Filter databases" :control-size :small))
         (table (make-instance 'table-view
                               :headers t :sortable nil
                               :columns (list (make-column "Database" #'xeon.databases:profile-name
                                                           :icon-function (lambda (p) (profile-icon databases p)))
                                              (make-column "Location" #'profile-location
                                                           :text-color :secondary-text))))
         (body (make-instance 'stack))
         (title (make-instance 'label :text "" :font (make-font :size 13 :weight :bold)))
         (subtitle (hint-label ""))
         (open (small-button "Open" :tooltip "Show the database's tables"))
         (query (small-button "New Query" :tooltip "Open an SQL editor for the database"))
         (empty-title (message-title))
         (empty-text (message-text))
         (empty-add (small-button "Add Database…"))
         (empty-create (small-button "New Database…"))
         (empty-actions (ui (hbox :padding '(6 0 0 0) :spacing 8
                              (spacer) (:widget empty-add) (:widget empty-create) (spacer))))
         (empty (ui (vbox :padding '(28 20 20 20) :spacing 6
                      (hbox (spacer) (:widget (make-instance 'image-view :image (icon :database :size 32 :color :secondary-text))) (spacer))
                      (:widget empty-title) (:widget empty-text) (:widget empty-actions) (spacer))))
         (list (ui (vbox :spacing 0
                     (:widget table :expand t)
                     (separator)
                     (vbox :padding '(10 12 12 12) :spacing 4
                       (:widget title)
                       (:widget subtitle)
                       (hbox :padding '(6 0 0 0) :spacing 6 (:widget open) (:widget query) (spacer)))))))
    (setf (workbench-databases workbench) databases)
    (flet ((keep (name widget) (setf (part databases name) widget)))
      (keep :count count) (keep :add add) (keep :filter filter) (keep :table table) (keep :body body)
      (keep :title title) (keep :subtitle subtitle) (keep :open open) (keep :query query)
      (keep :empty-title empty-title) (keep :empty-text empty-text) (keep :empty-actions empty-actions)
      (keep :empty empty) (keep :list list))
    (add-page body empty "Empty")
    (add-page body list "Databases")
    (flet ((with-selected (function)
             (action (workbench)
               (let ((profile (selected-profile databases)))
                 (when profile (funcall function profile))))))
      (on open :clicked (with-selected (lambda (p) (open-database workbench p))))
      (on query :clicked (with-selected (lambda (p) (open-query workbench p)))))
    (on add :clicked (lambda (e) (declare (ignore e))
                       (popup-menu (add-database-menu workbench) add 0 (or (widget-height add) 0))))
    (on empty-add :clicked (action (workbench) (choose-and-add-database workbench)))
    (on empty-create :clicked (action (workbench) (choose-and-create-database workbench)))
    (on filter :changed (lambda (e) (declare (ignore e)) (refresh-databases databases)))
    (on table :selection-changed (lambda (e) (declare (ignore e)) (refresh-profile-details databases)))
    (on table :row-activated
        (lambda (e) (with-reported-errors (workbench) (open-database workbench (event-item e)))))
    (on table :context-request
        (lambda (e)
          (let ((profile (item-at-point table (event-x e) (event-y e))))
            (when (typep profile 'xeon.databases:connection-profile)
              (select-profile databases profile)
              (refresh-profile-details databases)
              (popup-menu (profile-context-menu workbench databases profile) table (event-x e) (event-y e))
              (consume e)))))
    (refresh-databases databases)
    (ui (vbox :spacing 0
          (hbox :padding '(10 12 6 12) :spacing 6 (:widget heading) (spacer) (:widget count) (:widget add))
          (hbox :padding '(0 12 8 12) (:widget filter :expand t))
          (separator)
          (:widget body :expand t)))))
