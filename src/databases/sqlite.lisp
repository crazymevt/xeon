(in-package #:xeon.databases)

;;; SQLite's native dependency and platform library names are isolated here.
(cffi:define-foreign-library sqlite
  (:darwin (:or "libsqlite3.dylib" "/usr/lib/libsqlite3.dylib"))
  (:windows "sqlite3.dll") (:unix "libsqlite3.so.0"))
(defvar *loaded* nil)
(cffi:defcfun ("sqlite3_open_v2" %open) :int (path :string) (db :pointer) (flags :int) (vfs :pointer))
(cffi:defcfun ("sqlite3_close" %close) :int (db :pointer))
(cffi:defcfun ("sqlite3_errmsg" %error) :string (db :pointer))
(cffi:defcfun ("sqlite3_prepare_v2" %prepare) :int (db :pointer) (sql :pointer) (bytes :int) (stmt :pointer) (tail :pointer))
(cffi:defcfun ("sqlite3_step" %step) :int (stmt :pointer))
(cffi:defcfun ("sqlite3_finalize" %finalize) :int (stmt :pointer))
(cffi:defcfun ("sqlite3_column_count" %count) :int (stmt :pointer))
(cffi:defcfun ("sqlite3_column_name" %name) :string (stmt :pointer) (index :int))
(cffi:defcfun ("sqlite3_column_type" %type) :int (stmt :pointer) (index :int))
(cffi:defcfun ("sqlite3_column_text" %text) :string (stmt :pointer) (index :int))
(cffi:defcfun ("sqlite3_column_int64" %integer) :int64 (stmt :pointer) (index :int))
(cffi:defcfun ("sqlite3_column_double" %double) :double (stmt :pointer) (index :int))
(cffi:defcfun ("sqlite3_changes" %changes) :int (db :pointer))
(cffi:defcfun ("sqlite3_total_changes" %total-changes) :int (db :pointer))
(cffi:defcfun ("sqlite3_interrupt" %interrupt) :void (db :pointer))
(cffi:defcfun ("sqlite3_busy_timeout" %timeout) :int (db :pointer) (milliseconds :int))

(defstruct (connection (:constructor %connection (path handle))) path handle)
(defun connection-open-p (connection) (not (null (connection-handle connection))))

(defun connect (path &key create read-only)
  (when (and create read-only) (error "Cannot create a read-only database"))
  (unless *loaded* (cffi:use-foreign-library sqlite) (setf *loaded* t))
  (when (if create (probe-file path) (not (probe-file path)))
    (error (if create "Database already exists: ~A" "Database does not exist: ~A") path))
  (cffi:with-foreign-object (out :pointer)
    (let* ((code (%open (uiop:native-namestring path) out (cond (read-only 1) (create 6) (t 2)) (cffi:null-pointer)))
           (handle (cffi:mem-ref out :pointer)))
      (unless (zerop code)
        (let ((message (%error handle))) (%close handle) (error "SQLite: ~A" message)))
      (%timeout handle 1000)
      (let ((connection (%connection (pathname path) handle)))
        (handler-case (progn (query connection "PRAGMA schema_version")
                             (when create (query connection "VACUUM")) connection)
          (error (e) (disconnect connection) (error e)))))))

(defun disconnect (connection)
  (when (connection-open-p connection)
    (unless (zerop (%close (connection-handle connection))) (error "Database is busy"))
    (setf (connection-handle connection) nil)))

(defun interrupt (connection)
  "Asks a statement running on CONNECTION in another thread to stop. Safe from any thread
while the connection stays open."
  (when (connection-open-p connection) (%interrupt (connection-handle connection))))

(defun check-sql (connection sql)
  (when (find #\Null sql) (error "SQL cannot contain NUL characters"))
  (unless (connection-open-p connection) (error "Connection is closed")))

(defun step-rows (db stmt limit)
  "Steps a prepared statement: its column names, at most LIMIT rows as lists, and whether
more rows were left unread."
  (let ((names (loop for i below (%count stmt) collect (%name stmt i))) (rows nil) (n 0))
    (loop for code = (%step stmt)
          do (case code
               (101 (return (values names (nreverse rows) nil)))
               (100 (when (= n limit) (return (values names (nreverse rows) t)))
                (incf n)
                (push (loop for i below (%count stmt)
                            collect (case (%type stmt i)
                                      (1 (%integer stmt i)) (2 (%double stmt i))
                                      (3 (%text stmt i)) (4 :blob) (5 :null)))
                      rows))
               (otherwise (error "SQLite: ~A" (%error db)))))))

(defun query (connection sql &key (limit 200))
  "One statement; return column names, bounded rows, and truncation flag."
  (check-sql connection sql)
  (let ((db (connection-handle connection)))
    (cffi:with-foreign-string (source sql :encoding :utf-8)
     (cffi:with-foreign-objects ((out :pointer) (tail :pointer))
      (unless (zerop (%prepare db source -1 out tail)) (error "SQLite: ~A" (%error db)))
      (let ((stmt (cffi:mem-ref out :pointer)))
        (when (cffi:null-pointer-p stmt) (error "Enter a SQL statement"))
        (unwind-protect
             (progn
               (unless (every (lambda (c) (find c '(#\Space #\Tab #\Return #\Newline)))
                              (cffi:foreign-string-to-lisp (cffi:mem-ref tail :pointer)))
                 (error "Run one SQL statement at a time"))
               (step-rows db stmt limit))
          (%finalize stmt)))))))

(defstruct (result (:constructor make-result (columns rows truncated-p changes statements seconds)))
  "What running SQL showed: the COLUMNS and ROWS (a vector of vectors) of the last statement
that returned rows, or of the last statement when none did; whether rows were left unread,
the rows the script changed, how many statements ran, and how long they took."
  columns rows truncated-p changes statements seconds)

(defun execute (connection sql &key (limit 1000))
  "Runs every statement in SQL, in order, and returns a RESULT. Rows are bounded by LIMIT.
A failing statement stops the script; its error says which statement it was."
  (check-sql connection sql)
  (let* ((db (connection-handle connection))
         (started (get-internal-real-time))
         (before (%total-changes db))
         (count 0) (shown nil))
    (flet ((fail (message)
             (error "SQLite~:[~*~; (statement ~D)~]: ~A" (> count 1) count message)))
      (cffi:with-foreign-string (source sql :encoding :utf-8)
        (cffi:with-foreign-objects ((out :pointer) (tail :pointer))
          (let ((next source))
            (loop
              (unless (zerop (%prepare db next -1 out tail))
                (incf count) (fail (%error db)))
              (let ((stmt (cffi:mem-ref out :pointer)))
                ;; Only white space or comments remained.
                (when (cffi:null-pointer-p stmt) (return))
                (incf count)
                (unwind-protect
                     (multiple-value-bind (names rows more)
                         (handler-case (step-rows db stmt limit)
                           (error () (fail (%error db))))
                       (when (or names (null (first shown)))
                         (setf shown (list names rows more))))
                  (%finalize stmt)))
              (setf next (cffi:mem-ref tail :pointer)))))))
    (when (zerop count) (error "Enter a SQL statement"))
    (destructuring-bind (names rows more) shown
      (make-result names (map 'simple-vector (lambda (row) (coerce row 'simple-vector)) rows) more
                   (- (%total-changes db) before) count
                   (/ (- (get-internal-real-time) started) internal-time-units-per-second)))))

(defun quote-name (name)
  (format nil "~C~A~C" #\" (with-output-to-string (s) (loop for c across name do (write-char c s) (when (char= c #\") (write-char c s)))) #\"))

(defun table-select-sql (name)
  "The query that shows every row of the table or view NAME."
  (format nil "SELECT * FROM ~A" (quote-name name)))

(defun sql-value (value)
  (when (and (stringp value) (find #\Null value)) (error "NUL text editing is not supported"))
  (cond ((eq value :null) "NULL") ((eq value :blob) (error "BLOBs are read-only"))
        ((integerp value) (format nil "~D" value))
        ((floatp value) (substitute #\e #\d (string-downcase (format nil "~,17E" value))))
        ((stringp value) (format nil "~C~A~C" #\' (with-output-to-string (s) (loop for c across value do (write-char c s) (when (char= c #\') (write-char c s)))) #\'))
        (t (error "Unsupported database value"))))

(defun tables (connection)
  (mapcar #'first (nth-value 1 (query connection "SELECT name FROM sqlite_schema WHERE type='table' AND name NOT LIKE 'sqlite_%' ORDER BY name" :limit 10000))))

(defun schema-objects (connection)
  "The tables and views, as (NAME . :TABLE) or (NAME . :VIEW), by name."
  (mapcar (lambda (row) (cons (first row) (if (equal (second row) "view") :view :table)))
          (nth-value 1 (query connection "SELECT name, type FROM sqlite_schema WHERE type IN ('table', 'view') AND name NOT LIKE 'sqlite_%' ORDER BY name COLLATE NOCASE" :limit 100000))))

(defun save-row (connection table names original updated)
  "Optimistic update. Caller wraps multiple rows in a transaction."
  (when (member :blob original) (error "Rows containing BLOBs are read-only"))
  (query connection
         (format nil "UPDATE ~A SET ~{~A~^, ~} WHERE ~{~A~^ AND ~}"
                 (quote-name table)
                 (loop for name in names for value in updated collect (format nil "~A = ~A" (quote-name name) (sql-value value)))
                 (loop for name in names for value in original collect (format nil "~A IS ~A" (quote-name name) (sql-value value)))))
  (unless (= 1 (%changes (connection-handle connection))) (error "Row changed externally; reload before saving")))
