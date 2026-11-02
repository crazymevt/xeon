(in-package #:xeon.http)

;;; API environments: named sets of variables, some of them secret, that requests use
;;; through {{name}}. A system environment is available everywhere; a project
;;; environment belongs to one project folder. All of them, secrets included, live in
;;; one file in the user's configuration directory, written owner-only (0600), and never
;;; in a project folder, so they cannot be committed with the requests.
;;;
;;; The file is plain data, read without evaluation or reader macros:
;;;   (:version 1 :environments ((:id .. :name .. :scope :system|"<project root>"
;;;                                :variables (("name" "value" secretp) ..)) ..)
;;;    :active (("<project root>" . "<id>") (:system . "<id>")))

(defparameter *maximum-environments* 200)
(defparameter *maximum-environment-variables* 500)

(defstruct (api-environment (:conc-name environment-)
                            (:constructor %environment (id name scope variables)))
  "VARIABLES are (name value secretp) lists, in order. SCOPE is :SYSTEM or a project root
namestring."
  id name scope variables)

(defstruct (environment-store (:constructor %environment-store (path)))
  path (environments '()) (active '()) warning)

(defun environment-variable-name (variable) (first variable))
(defun environment-variable-value (variable) (second variable))
(defun environment-variable-secret-p (variable) (third variable))

(defun text-ok-p (value maximum &key (empty nil))
  (and (stringp value) (<= (if empty 0 1) (length value) maximum) (not (find #\Null value))))

(defun check-environment-variables (variables)
  (unless (<= (length variables) *maximum-environment-variables*)
    (fail-request (format nil "An environment holds at most ~D variables" *maximum-environment-variables*)))
  (dolist (v variables)
    (unless (and (consp v) (= 3 (length v)) (http-token-p (first v))
                 (text-ok-p (second v) 65536 :empty t)
                 (notany (lambda (c) (find c '(#\Return #\Newline))) (second v))
                 (member (third v) '(t nil)))
      (fail-request "Environment variables need a valid name and a single-line value")))
  (unless (= (length variables) (length (remove-duplicates variables :key #'first :test #'string=)))
    (fail-request "Environment variable names must be unique"))
  variables)

(defun scope-key (scope)
  "A project folder's key: its true name where it exists, so symbolic links and the
spellings of one folder share their environments."
  (if (eq scope :system)
      :system
      (let ((directory (uiop:ensure-directory-pathname scope)))
        (uiop:native-namestring (or (ignore-errors (truename directory)) directory)))))

(defun parse-environment (entry)
  (unless (and (xeon.themes::finite-list-p entry) (= 8 (length entry))
               (equal '(:id :name :scope :variables) (loop for k in entry by #'cddr collect k)))
    (error "Invalid environment entry."))
  (destructuring-bind (&key id name scope variables) entry
    (unless (and (text-ok-p id 64) (text-ok-p name 200)
                 (or (eq scope :system) (text-ok-p scope 4096))
                 (xeon.themes::finite-list-p variables)
                 (every #'xeon.themes::finite-list-p variables))
      (error "Invalid environment entry."))
    (check-environment-variables variables)
    (%environment id name scope (mapcar #'copy-list variables))))

(defun load-environment-store (path)
  "Reads the environments at PATH. A missing file is an empty store; a damaged one is
ignored, with a warning, and not overwritten until an environment is changed."
  (let ((store (%environment-store path)))
    (when (and path (probe-file path))
      (handler-case
          (let ((data (xeon.themes::read-theme-data path (* 8 1024 1024))))
            (unless (and (xeon.themes::finite-list-p data) (= 6 (length data))
                         (eq (first data) :version) (eql (second data) 1)
                         (eq (third data) :environments) (xeon.themes::finite-list-p (fourth data))
                         (<= (length (fourth data)) *maximum-environments*)
                         (eq (fifth data) :active) (xeon.themes::finite-list-p (sixth data)))
              (error "Invalid environments file."))
            (let ((environments (mapcar #'parse-environment (fourth data))))
              (unless (= (length environments)
                         (length (remove-duplicates environments :key #'environment-id :test #'equal)))
                (error "Duplicate environment."))
              (setf (environment-store-environments store) environments
                    (environment-store-active store)
                    (loop for pair in (sixth data)
                          when (and (consp pair) (or (eq (car pair) :system) (text-ok-p (car pair) 4096))
                                    (text-ok-p (cdr pair) 64))
                            collect (cons (car pair) (cdr pair))))))
        (error (condition)
          (setf (environment-store-warning store)
                (format nil "API environments were ignored: ~A" condition)))))
    store))

(defun save-environment-store (store)
  "Writes STORE owner-only: new files start 0600 and the mode is enforced after each write."
  (let ((path (environment-store-path store)))
    (when path
      (xeon.themes::write-data-file
       path
       (list :version 1
             :environments (mapcar (lambda (e)
                                     (list :id (environment-id e) :name (environment-name e)
                                           :scope (environment-scope e)
                                           :variables (environment-variables e)))
                                   (environment-store-environments store))
             :active (environment-store-active store))
       :if-exists :overwrite
       :header (format nil ";;; Xeon API environments. Contains secrets: keep this file private.~%"))
      (xeon.core::set-file-mode path #o600)
      (setf (environment-store-warning store) nil))
    store))

(defun find-environment (store id)
  (find id (environment-store-environments store) :key #'environment-id :test #'equal))

(defun project-environments (store project-root)
  "The environments usable in PROJECT-ROOT (or with no project, NIL): system ones first."
  (let ((key (and project-root (scope-key project-root))))
    (append (remove-if-not (lambda (e) (eq :system (environment-scope e))) (environment-store-environments store))
            (when key
              (remove-if-not (lambda (e) (equal key (environment-scope e))) (environment-store-environments store))))))

(defun active-environment (store project-root)
  "The environment chosen for PROJECT-ROOT, or with no project the system choice."
  (let* ((key (if project-root (scope-key project-root) :system))
         (id (cdr (assoc key (environment-store-active store) :test #'equal)))
         (environment (and id (find-environment store id))))
    (and environment (member environment (project-environments store project-root)) environment)))

(defun set-active-environment (store project-root environment)
  (let ((key (if project-root (scope-key project-root) :system)))
    (setf (environment-store-active store)
          (append (remove key (environment-store-active store) :key #'car :test #'equal)
                  (when environment (list (cons key (environment-id environment))))))
    (save-environment-store store)
    environment))

(defun add-environment (store name scope &key variables)
  (let ((name (string-trim " " name)))
    (unless (text-ok-p name 200) (fail-request "Enter an environment name of up to 200 characters"))
    (when (>= (length (environment-store-environments store)) *maximum-environments*)
      (fail-request (format nil "Xeon keeps at most ~D environments" *maximum-environments*)))
    (check-environment-variables variables)
    (let ((environment (%environment (loop for id = (format nil "e~36R" (random (expt 36 10) (make-random-state t)))
                                           unless (find-environment store id) return id)
                                     name (scope-key scope) (mapcar #'copy-list variables))))
      (setf (environment-store-environments store)
            (append (environment-store-environments store) (list environment)))
      (save-environment-store store)
      environment)))

(defun update-environment (store environment &key (name (environment-name environment))
                                                  (variables (environment-variables environment)))
  (let ((name (string-trim " " name)))
    (unless (text-ok-p name 200) (fail-request "Enter an environment name of up to 200 characters"))
    (check-environment-variables variables)
    (setf (environment-name environment) name
          (environment-variables environment) (mapcar #'copy-list variables))
    (save-environment-store store)
    environment))

(defun remove-environment (store environment)
  (setf (environment-store-environments store) (remove environment (environment-store-environments store))
        (environment-store-active store)
        (remove (environment-id environment) (environment-store-active store) :key #'cdr :test #'equal))
  (save-environment-store store)
  store)

(defun environment-bindings (environment)
  "(name . value) pairs for RESOLVE-ENTRY's :VARIABLES."
  (and environment
       (mapcar (lambda (v) (cons (first v) (second v))) (environment-variables environment))))

(defun redact-secrets (text environment)
  "TEXT with each non-empty secret value of ENVIRONMENT replaced by bullets."
  (let ((secrets (sort (loop for v in (and environment (environment-variables environment))
                             when (and (third v) (plusp (length (second v)))) collect (second v))
                       #'> :key #'length)))
    ;; Scan the original text once. Never search replacement bullets again:
    ;; a secret may itself contain bullets, and replacements must stay opaque.
    (with-output-to-string (out)
      (loop with start = 0
            while (< start (length text))
            do (let ((secret (find-if (lambda (secret)
                                       (and (<= (+ start (length secret)) (length text))
                                            (string= secret text :start2 start
                                                                :end2 (+ start (length secret)))))
                                     secrets)))
                 (if secret
                     (progn (write-string "••••••" out) (incf start (length secret)))
                     (progn (write-char (char text start) out) (incf start))))))))

;;; Stored collections: .http files in a project's .xeon/api folder or in the API
;;; folder of Xeon's configuration directory.

(defparameter *maximum-collections* 500)

(defun project-collection-directory (project-root)
  (merge-pathnames (make-pathname :directory '(:relative ".xeon" "api"))
                   (uiop:ensure-directory-pathname project-root)))

(defun system-collection-directory (configuration-directory)
  (merge-pathnames (make-pathname :directory '(:relative "api"))
                   (uiop:ensure-directory-pathname configuration-directory)))

(defun list-collections (directory)
  "The .http and .rest files in DIRECTORY, sorted by name; none when it does not exist."
  (when (and directory (uiop:directory-exists-p directory))
    (let ((files (remove-if-not (lambda (p) (member (pathname-type p) '("http" "rest") :test #'equalp))
                                (uiop:directory-files directory))))
      (subseq (sort files #'string-lessp :key #'file-namestring)
              0 (min (length files) *maximum-collections*)))))

(defun collection-file-name (name)
  "A safe file name for a collection called NAME."
  (let* ((name (string-trim " ." name))
         (clean (map 'string (lambda (c) (if (or (alphanumericp c) (find c "-_ ")) c #\-)) name)))
    (when (zerop (length clean)) (fail-request "Enter a collection name"))
    (when (> (length clean) 120) (fail-request "Collection names are limited to 120 characters"))
    (concatenate 'string clean ".http")))

(defun create-collection (directory name &key (text (default-collection-source)))
  "Creates the collection file NAME in DIRECTORY and returns its pathname. Refuses to
replace an existing file."
  (let ((path (merge-pathnames (collection-file-name name) (uiop:ensure-directory-pathname directory))))
    (when (probe-file path) (fail-request (format nil "~A already exists" (file-namestring path))))
    (ensure-directories-exist path)
    (xeon.core:save-buffer-as (xeon.core:make-buffer :text text) path :if-exists :error)
    path))
