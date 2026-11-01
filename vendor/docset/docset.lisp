;;;; docset.lisp — writes and reads docsets (see docs/format.md).
;;;;
;;;; Plain Common Lisp with no dependencies, so the spec browser, Cadre and
;;;; the generators of docsets can all share it.

(in-package #:docset)

;;; Printing and reading data

(defun write-data (form stream)
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-pretty* nil)
          (*print-readably* nil))
      (prin1 form stream))))

(defun read-data (stream &optional (eof-error-p t))
  "Reads one form of docset data, with stray symbols kept out of real packages."
  (with-standard-io-syntax
    (let ((*read-eval* nil)
          (*package* (find-package '#:docset.data)))
      (read stream eof-error-p nil))))

(defun data-file (directory name)
  (merge-pathnames name (uiop-free-directory directory)))

(defun uiop-free-directory (directory)
  "DIRECTORY as a directory pathname, whether or not it was written with a final slash."
  (let ((p (pathname directory)))
    (if (or (pathname-name p) (pathname-type p))
        (make-pathname :directory (append (or (pathname-directory p) '(:relative))
                                          (list (file-namestring p)))
                       :name nil :type nil :defaults p)
        p)))

;;; Writing

(defparameter *index-keys* '(:names :kind :kind-name :package :parent :summary))

(defun write-docset (directory manifest entries)
  "Writes a docset to DIRECTORY. MANIFEST is a plist without :entry-count; each of
ENTRIES is a plist starting (:entry \"id\" ...) as docs/docset-format.md describes."
  (let ((directory (uiop-free-directory directory))
        (index '()))
    (ensure-directories-exist directory)
    (with-open-file (out (data-file directory "entries.sexp") :direction :output
                         :if-exists :supersede :external-format :utf-8)
      (dolist (entry entries)
        (let ((id (second entry)) (plist (cddr entry)))
          (push (append (list id)
                        (loop for key in *index-keys*
                              for value = (getf plist key)
                              when value append (list key value))
                        (list :position (file-position out)))
                index)
          (write-data entry out)
          (terpri out))))
    (with-open-file (out (data-file directory "index.sexp") :direction :output
                         :if-exists :supersede :external-format :utf-8)
      (write-string "(" out)
      (dolist (record (nreverse index))
        (terpri out)
        (write-data record out))
      (format out ")~%"))
    (with-open-file (out (data-file directory "manifest.sexp") :direction :output
                         :if-exists :supersede :external-format :utf-8)
      (write-data (append (list :format 1)
                          (loop for (key value) on manifest by #'cddr
                                unless (member key '(:format :entry-count :toc))
                                  append (list key value))
                          (list :entry-count (length entries)
                                :toc (getf manifest :toc)))
                  out)
      (terpri out))
    directory))

;;; Reading

(defstruct (docset (:constructor %make-docset))
  directory manifest index
  (by-id (make-hash-table :test 'equal))
  (by-name (make-hash-table :test 'equal))   ; lowercase name -> records
  (cache (make-hash-table :test 'equal)))    ; id -> entry, once read

(defun record-id (record) (first record))
(defun record-names (record) (getf (rest record) :names))
(defun record-kind (record) (getf (rest record) :kind))
(defun record-kind-name (record) (getf (rest record) :kind-name))
(defun record-package (record) (getf (rest record) :package))
(defun record-parent (record) (getf (rest record) :parent))
(defun record-summary (record) (getf (rest record) :summary))
(defun record-position (record) (getf (rest record) :position))

(defun docset-id (docset) (getf (docset-manifest docset) :id))
(defun docset-title (docset) (getf (docset-manifest docset) :title))
(defun docset-packages (docset) (getf (docset-manifest docset) :packages))
(defun docset-toc (docset) (getf (docset-manifest docset) :toc))

(defun open-docset (directory)
  "Reads DIRECTORY's manifest and index. Entries are read when asked for."
  (let* ((directory (uiop-free-directory directory))
         (manifest (with-open-file (in (data-file directory "manifest.sexp") :external-format :utf-8)
                     (read-data in)))
         (index (with-open-file (in (data-file directory "index.sexp") :external-format :utf-8)
                  (read-data in)))
         (docset (%make-docset :directory directory :manifest manifest :index index)))
    (unless (eql (getf manifest :format) 1)
      (error "~A is docset format ~S; this reader knows format 1." directory (getf manifest :format)))
    (dolist (record index)
      (setf (gethash (record-id record) (docset-by-id docset)) record)
      (dolist (name (record-names record))
        (push record (gethash (string-downcase name) (docset-by-name docset)))))
    (maphash (lambda (name records) (setf (gethash name (docset-by-name docset)) (nreverse records)))
             (docset-by-name docset))
    docset))

(defun entry-record (docset id)
  (gethash id (docset-by-id docset)))

(defun docset-entry (docset id)
  "The entry with ID, read from disk the first time."
  (or (gethash id (docset-cache docset))
      (let ((record (entry-record docset id)))
        (when record
          (setf (gethash id (docset-cache docset))
                (with-open-file (in (data-file (docset-directory docset) "entries.sexp")
                                    :external-format :utf-8)
                  (file-position in (record-position record))
                  (read-data in)))))))

(defun entry-id (entry) (second entry))
(defun entry-names (entry) (getf (cddr entry) :names))
(defun entry-kind-name (entry) (getf (cddr entry) :kind-name))
(defun entry-sections (entry) (getf (cddr entry) :sections))

(defun entry-section (entry label)
  (cdr (assoc label (entry-sections entry) :test #'string-equal)))

;;; Finding docsets

(defvar *docset-directories* '()
  "Directories holding docsets, each in its own subdirectory, searched by FIND-DOCSETS.")

(defvar *library-systems* '("gtk4" "raylib")
  "ASDF systems whose docs/docset/ FIND-DOCSETS looks for, when ASDF can find them.")

(defun user-docset-directory ()
  "Where generated docsets of libraries go, each in its own subdirectory:
$XDG_DATA_HOME/docsets/, or ~/.local/share/docsets/."
  (let ((xdg #+sbcl (sb-ext:posix-getenv "XDG_DATA_HOME") #-sbcl nil))
    (merge-pathnames "docsets/"
                     (if (and xdg (plusp (length xdg)))
                         (uiop-free-directory xdg)
                         (merge-pathnames ".local/share/" (user-homedir-pathname))))))

(defun library-docset-directory (system-name)
  (let* ((asdf (find-package :asdf))
         (find-system (and asdf (find-symbol "FIND-SYSTEM" asdf)))
         (source-directory (and asdf (find-symbol "SYSTEM-SOURCE-DIRECTORY" asdf)))
         (system (and find-system (ignore-errors (funcall find-system system-name nil)))))
    (when system
      (let ((dir (merge-pathnames "docs/docset/" (funcall source-directory system))))
        (when (probe-file (data-file dir "manifest.sexp")) dir)))))

(defun find-docsets (&key (directories *docset-directories*) (systems *library-systems*) (user t))
  "Opens every docset found in DIRECTORIES, in the docs/docset/ of SYSTEMS and,
when USER, in USER-DOCSET-DIRECTORY. A docset id found twice is opened once."
  (let ((found '()))
    (dolist (dir (append directories (when user (list (user-docset-directory)))))
      (dolist (sub (directory (merge-pathnames "*/manifest.sexp" (uiop-free-directory dir))))
        (push (make-pathname :name nil :type nil :defaults sub) found)))
    (dolist (system systems)
      (let ((dir (library-docset-directory system)))
        (when dir (push dir found))))
    (let ((ids '()))
      (loop for dir in (nreverse found)
            for docset = (handler-case (open-docset dir)
                           (error (e) (warn "Skipping docset ~A: ~A" dir e) nil))
            when (and docset (not (member (docset-id docset) ids :test #'equal)))
              do (push (docset-id docset) ids)
              and collect docset))))

;;; Looking things up

(defun find-records (docset name)
  "The index records of DOCSET's entries named NAME, in any case."
  (gethash (string-downcase name) (docset-by-name docset)))

(defun docset-package-name (docset name)
  "The package name DOCSET's records use for NAME, which may be one of the
nicknames its manifest lists."
  (or (loop for (package . nicknames) in (getf (docset-manifest docset) :nicknames)
            when (or (string-equal name package) (member name nicknames :test #'string-equal))
              return package)
      name))

(defun lookup-symbol (docsets name package)
  "Records documenting the symbol NAME of the package named PACKAGE (a string,
or a package; a nickname will do), across DOCSETS, as (docset . record):
entries for the symbol itself first, then entries with no package (glossary terms)."
  (let ((package-name (if (packagep package) (package-name package) (string package)))
        (symbols '()) (others '()))
    (dolist (docset docsets)
      (let ((package-name (docset-package-name docset package-name)))
        (dolist (record (find-records docset name))
          (cond ((null (record-package record)) (push (cons docset record) others))
                ((string-equal package-name (record-package record)) (push (cons docset record) symbols))))))
    (append (nreverse symbols) (nreverse others))))

(defun search-names (docset query &key (limit 50))
  "Records whose names contain QUERY: names starting with it first, then shorter names first."
  (let ((query (string-downcase query)) (hits '()))
    (dolist (record (docset-index docset))
      (let ((best nil))
        (dolist (name (record-names record))
          (let ((at (search query (string-downcase name))))
            (when at
              (let ((score (+ (if (zerop at) 0 1000) (length name))))
                (when (or (null best) (< score best)) (setf best score))))))
        (when best (push (cons best record) hits))))
    (mapcar #'cdr (subseq (stable-sort hits #'< :key #'car) 0 (min limit (length hits))))))
