(in-package #:xeon.folders)

(defstruct discovered-system name file (dialect :common-lisp))

(defstruct project-run process log)

(defun project-run-command (system)
  "Return an argv list, never a shell command. CL entry points run on SBCL's
initial thread in a fresh process, not the editor or a Swank worker thread."
  (ecase (discovered-system-dialect system)
    (:common-lisp
     (let ((*print-readably* t) (*print-pretty* nil))
       (list "sbcl" "--noinform" "--non-interactive"
             "--eval" "(require :asdf)"
             "--eval" (format nil "(asdf:load-asd ~S)" (uiop:native-namestring (discovered-system-file system)))
             "--eval" (format nil "(asdf:load-system ~S)" (discovered-system-name system))
             "--eval"
             (format nil "(let ((entry (asdf/system:component-entry-point (asdf:find-system ~S)))) (unless entry (error ~S)) (uiop:call-function entry))"
                     (discovered-system-name system)
                     "No ASDF :entry-point configured for this system."))))
    (:clojure
     ;; Leiningen's :main and deps.edn's :run alias are explicit project contracts.
     (if (equal (file-namestring (discovered-system-file system)) "project.clj")
         '("lein" "run") '("clojure" "-M:run")))
    (:racket '("racket" "main.rkt"))))

(defun launch-project (system &key (launcher #'uiop:launch-program))
  "Launch independently of the REPL. Output/errors go to a unique persistent log.
The returned process handle supports UIOP status, wait and termination operations."
  (let ((command (project-run-command system))
        (directory (uiop:pathname-directory-pathname (discovered-system-file system))))
    (uiop:with-temporary-file (:stream out :pathname log :prefix "xeon-run-"
                              :type "log" :keep t)
      (declare (ignore out))
      (make-project-run :log log
        :process (funcall launcher command :directory directory :input nil
                          :output log :error-output :output
                          :if-output-exists :append)))))

(defun project-marker-dialect (path)
  (cond ((member (file-namestring path) '("deps.edn" "project.clj") :test #'equal) :clojure)
        ((equal (file-namestring path) "info.rkt") :racket)))

(defun asd-system-names (path)
  "Read static top-level definitions only. Never evaluate project code during discovery.
Complex/generated definitions fall back to the ASDF file's primary system name."
  (or (ignore-errors
        (with-open-file (input path :external-format :utf-8)
          (when (> (file-length input) (* 1024 1024)) (return-from asd-system-names nil))
          (let ((*read-eval* nil) (*package* (find-package :asdf))
                (*readtable* (copy-readtable nil)))
            (loop for form = (read input nil :eof) until (eq form :eof)
                  when (and (consp form) (symbolp (first form))
                            (string= (symbol-name (first form)) "DEFSYSTEM")
                            (consp (cdr form)) (typep (second form) '(or string symbol)))
                    collect (string-downcase (string (second form)))))))
      (list (pathname-name path))))

(defun discover-folder-systems (project &key (limit 10000) (max-depth 16))
  "Bounded discovery using Explorer's ignore and symlink policy. Returns systems and a warning."
  (let ((systems nil) (remaining limit) (warning nil))
    (labels ((walk (directory depth)
               (handler-case
                   (dolist (entry (list-folder-children project directory))
                     (when (minusp (decf remaining))
                       (setf warning "Project discovery entry limit reached.") (return))
                     (unless (entry-link-p entry)
                       (cond ((entry-directory-p entry)
                              (if (< depth max-depth) (walk (entry-path entry) (1+ depth))
                                  (setf warning "Project discovery depth limit reached.")))
                             ((project-marker-dialect (entry-path entry))
                              (let* ((path (entry-path entry))
                                     (directory (uiop:pathname-directory-pathname path))
                                     (dialect (project-marker-dialect path)))
                                ;; Prefer deps.edn when both Clojure markers exist.
                                (unless (and (equal (file-namestring path) "project.clj")
                                             (probe-file (merge-pathnames "deps.edn" directory)))
                                  (push (make-discovered-system
                                         :name (car (last (pathname-directory directory)))
                                         :file path :dialect dialect) systems))))
                             ((equalp (pathname-type (entry-path entry)) "asd")
                              (dolist (name (asd-system-names (entry-path entry)))
                                (push (make-discovered-system :name name :file (entry-path entry)) systems))))))
                 (error (condition) (setf warning (princ-to-string condition))))))
      (walk (project-root project) 0))
    (values (sort systems #'string-lessp :key #'discovered-system-name) warning)))

(defun system-action-source (system action)
  "Serialize paths and names as Lisp strings, not interpolated executable text."
  (unless (member action '(:load :test :recompile)) (error "Unknown project action: ~S" action))
  (when (eq action :recompile)
    (unless (eq (discovered-system-dialect system) :common-lisp)
      (error "Project recompilation currently requires a Common Lisp ASDF system."))
    (let ((*print-readably* t) (*print-pretty* nil))
      (return-from system-action-source
        (format nil "(progn (require :asdf) (asdf:load-asd ~S) (asdf:operate 'asdf:compile-op ~S :force t))"
          (uiop:native-namestring (discovered-system-file system)) (discovered-system-name system)))))
  (unless (eq (discovered-system-dialect system) :common-lisp)
    (return-from system-action-source (language-project-action-source system action)))
  (let ((*print-readably* t) (*print-pretty* nil))
    (format nil "(progn (require :asdf) (asdf:load-asd ~S) (asdf:~A ~S))"
            (uiop:native-namestring (discovered-system-file system))
            (ecase action (:load "load-system") (:test "test-system"))
            (discovered-system-name system))))

(defun project-source-string (string)
  "String literal shared by Clojure and Racket, including Windows path escaping."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across string do
      (case char
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (otherwise (when (find char '(#\\ #\")) (write-char #\\ out))
                   (write-char char out))))
    (write-char #\" out)))

(defun language-project-action-source (system action)
  (let* ((root (uiop:pathname-directory-pathname (discovered-system-file system)))
         (path (project-source-string (uiop:native-namestring root))))
    (ecase (discovered-system-dialect system)
      (:clojure
       (format nil "(do (require 'clojure.java.io 'clojure.test) (let [root (clojure.java.io/file ~A) files (fn [dir] (let [p (clojure.java.io/file root dir)] (if (.exists p) (sort-by str (filter #(and (.isFile %) (re-find #\"\\.(clj|cljc)$\" (.getName %))) (file-seq p))) []))) load-dir (fn [dir] (doseq [f (files dir)] (load-file (.getPath f))))] (load-dir \"src\") ~A))"
               path (if (eq action :test)
                        "(do (load-dir \"test\") (clojure.test/run-all-tests))"
                        ":loaded")))
      (:racket
       (let ((main (project-source-string (uiop:native-namestring (merge-pathnames "main.rkt" root)))))
         (if (eq action :load)
             (format nil "(dynamic-require '(file ~A) #f)" main)
             (format nil "(dynamic-require '(submod (file ~A) test) #f)" main)))))))
