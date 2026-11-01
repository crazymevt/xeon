(in-package #:xeon.tests)

(defun test-outline ()
  (dolist (declaration '("#:xeon.core" ":xeon.core" "\"xeon.core\""))
    (let ((buffer (make-buffer :name "buffer.lisp"
                   :text (format nil "; Package preamble~%(in-package ~A)~%(defun example () nil)" declaration))))
      (check (equal "xeon.core" (nth-value 1 (xeon.syntax:buffer-outline buffer))))))
  (check (equal "cl-user" (nth-value 1 (xeon.syntax:buffer-outline
                                      (make-buffer :name "plain.lisp" :text "(defun example () nil)")))))
  (let ((root (merge-pathnames (format nil "krypton-outline-~A/" (gensym)) (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (dolist (spec '(("top.lisp" "(in-package :top) (defun top () 1)")
                           ("src/deep/code.clj" "(ns demo) (defn saved [] 1)")
                           (".git/hidden.lisp" "(defun hidden () 1)")))
             (folder-test-write (merge-pathnames (first spec) root) (second spec)))
           (check (= 1 (length (xeon.syntax:folder-outline root nil))))
           (let ((buffer (open-buffer-file (merge-pathnames "src/deep/code.clj" root))))
             (insert-text buffer " (defn unsaved [] 2)" (buffer-length buffer))
             (check (equal '("saved" "top" "unsaved")
                           (mapcar #'xeon.syntax:outline-symbol-name
                                   (xeon.syntax:folder-outline root (list buffer) :recursive t)))))
           (delete-file (merge-pathnames "src/deep/code.clj" root))
           (check (= 1 (length (xeon.syntax:folder-outline root nil :recursive t)))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  (let ((entries (xeon.syntax:buffer-outline
                  (make-buffer :name "icons.lisp"
                    :text "(defconstant a 1) (defvar b 2) (defun c () 3) (defmacro d () nil) (defclass e () ())"))))
    (check (equal '(:constant :variable :function :macro :type)
                  (mapcar #'xeon.syntax:outline-symbol-category entries))))
  (let ((entries (xeon.syntax:buffer-outline
                  (make-buffer :name "icons.rkt" :text "(define a 1) (define (b x) x) (define-syntax c 1)"))))
    (check (equal '(:variable :function :macro)
                  (mapcar #'xeon.syntax:outline-symbol-category entries))))
  (flet ((names (text file)
           (mapcar #'xeon.syntax:outline-symbol-name
                   (xeon.syntax:buffer-outline (make-buffer :name file :text text)))))
    (check (equal '("alpha" "zebra")
                  (names (format nil "(in-package :demo)~%(defun zebra () \"(defun fake ())\")~%; (defun nope ())~%(defvar alpha 1)") "demo.lisp")))
    (check (equal '("alpha" "zebra") (names "(ns demo) (defn zebra [] 1) (def alpha 2)" "demo.clj")))
    (check (equal '("alpha" "zebra") (names "#lang racket
(define (zebra x) x) (define alpha 1)" "demo.rkt")))
    (check (equal '("late")
                  (names (concatenate 'string (make-string (* 1024 1024) :initial-element #\Space)
                                      "(defun late () 1)") "large.lisp"))))
  (let* ((text "(in-package :one) (defun first () 1) (in-package :two) (defun second () 2)")
         (buffer (make-buffer :name "demo.lisp" :text text)))
    (set-selection buffer (buffer-length buffer))
    (multiple-value-bind (entries namespace) (xeon.syntax:buffer-outline buffer)
      (check (equal "two" namespace))
      (check (equal '("one" "two") (mapcar #'xeon.syntax:outline-symbol-namespace entries)))
      (dolist (entry entries)
        (let ((at (xeon.syntax:outline-symbol-position entry))
              (name (xeon.syntax:outline-symbol-name entry)))
          (check (string= name (buffer-substring buffer at (+ at (length name))))))))))
