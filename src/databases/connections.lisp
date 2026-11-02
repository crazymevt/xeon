(in-package #:xeon.databases)

;;; Stored connections: the databases the user has added, kept in a small data file
;;; read without reader macros or evaluation. Only SQLite files are supported; KIND
;;; leaves room for other drivers.

(defparameter *maximum-profiles* 200)

(defstruct (connection-profile (:conc-name profile-)
                               (:constructor %profile (id name kind path read-only-p)))
  id name kind path read-only-p)

(defun profile-pathname (profile)
  (uiop:parse-native-namestring (profile-path profile)))

(defstruct (connection-store (:constructor %connection-store (path)))
  "The stored connections, in the order they were added, and the file they live in.
WARNING explains why a damaged file was ignored."
  path (profiles '()) warning)

(defun valid-text-p (value maximum)
  (and (stringp value) (<= 1 (length value) maximum) (not (find #\Null value))))

(defun parse-profile (entry)
  (unless (and (xeon.themes::finite-list-p entry) (= 10 (length entry))
               (equal '(:id :name :kind :path :read-only) (loop for key in entry by #'cddr collect key)))
    (error "Invalid connection entry."))
  (destructuring-bind (&key id name kind path read-only) entry
    (unless (and (valid-text-p id 64) (valid-text-p name 200) (eq kind :sqlite)
                 (valid-text-p path 4096) (member read-only '(t nil)))
      (error "Invalid connection entry."))
    (%profile id name kind path read-only)))

(defun load-connection-store (path)
  "Reads the stored connections at PATH. A missing file is an empty store; a damaged one
is ignored, with a warning, and is not overwritten until a connection is changed."
  (let ((store (%connection-store path)))
    (when (and path (probe-file path))
      (handler-case
          (let ((data (xeon.themes::read-theme-data path 1048576)))
            (unless (and (xeon.themes::finite-list-p data) (= 4 (length data))
                         (eq (first data) :version) (eql (second data) 1)
                         (eq (third data) :connections) (xeon.themes::finite-list-p (fourth data))
                         (<= (length (fourth data)) *maximum-profiles*))
              (error "Invalid connections file."))
            (let ((profiles (mapcar #'parse-profile (fourth data))))
              (unless (= (length profiles)
                         (length (remove-duplicates profiles :key #'profile-id :test #'equal)))
                (error "Duplicate connection."))
              (setf (connection-store-profiles store) profiles)))
        (error (condition)
          (setf (connection-store-warning store)
                (format nil "Stored database connections were ignored: ~A" condition)))))
    store))

(defun save-connection-store (store)
  (let ((path (connection-store-path store)))
    (when path
      (xeon.themes::write-data-file
       path
       (list :version 1
             :connections (mapcar (lambda (p)
                                    (list :id (profile-id p) :name (profile-name p) :kind (profile-kind p)
                                          :path (profile-path p) :read-only (and (profile-read-only-p p) t)))
                                  (connection-store-profiles store)))
       :if-exists :overwrite)
      (setf (connection-store-warning store) nil))
    store))

(defun find-connection-profile (store id)
  (find id (connection-store-profiles store) :key #'profile-id :test #'equal))

(defun new-profile-id (store)
  (loop for id = (format nil "c~36R" (random (expt 36 10) (make-random-state t)))
        unless (find-connection-profile store id) return id))

(defun same-path-p (a b)
  (funcall #+(or win32 darwin) #'string-equal #-(or win32 darwin) #'string= a b))

(defun add-connection-profile (store path &key name read-only)
  "Stores a connection to the SQLite database at PATH and returns it. A database already
stored is returned as it is, with a second value of T."
  (let* ((path (uiop:native-namestring (merge-pathnames path)))
         (existing (find path (connection-store-profiles store) :key #'profile-path :test #'same-path-p)))
    (when existing (return-from add-connection-profile (values existing t)))
    (when (>= (length (connection-store-profiles store)) *maximum-profiles*)
      (error "Xeon keeps at most ~D database connections." *maximum-profiles*))
    (let* ((name (string-trim " " (or name (file-namestring (uiop:parse-native-namestring path)))))
           (profile (%profile (new-profile-id store) name :sqlite path (and read-only t))))
      (unless (valid-text-p name 200) (error "Enter a name of up to 200 characters."))
      (unless (valid-text-p path 4096) (error "The database path is too long."))
      (setf (connection-store-profiles store) (append (connection-store-profiles store) (list profile)))
      (save-connection-store store)
      (values profile nil))))

(defun rename-connection-profile (store profile name)
  (let ((name (string-trim " " name)))
    (unless (valid-text-p name 200) (error "Enter a name of up to 200 characters."))
    (setf (profile-name profile) name)
    (save-connection-store store)
    profile))

(defun remove-connection-profile (store profile)
  "Forgets PROFILE. The database file itself is left alone."
  (setf (connection-store-profiles store) (remove profile (connection-store-profiles store)))
  (save-connection-store store)
  store)
