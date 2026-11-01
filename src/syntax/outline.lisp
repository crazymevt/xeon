(in-package #:xeon.syntax)

(defstruct outline-symbol namespace name kind position pathname)

(defun outline-symbol-category (symbol)
  (let ((kind (outline-symbol-kind symbol)))
    (cond ((member kind '("defconstant") :test #'equal) :constant)
          ((member kind '("defvar" "defparameter" "def" "define") :test #'equal) :variable)
          ((member kind '("defmacro" "define-syntax" "define-syntax-rule" "define-symbol-macro") :test #'equal) :macro)
          ((member kind '("defun" "defmethod" "defgeneric" "defn" "defn-" "defmulti" "define-function" "define/contract") :test #'equal) :function)
          ((member kind '("defclass" "defstruct" "deftype" "defrecord" "defprotocol" "define-condition" "struct") :test #'equal) :type)
          (t :namespace))))

(defun outline-source-files (directory &key recursive)
  (remove-if-not
   (lambda (path)
     (member (string-downcase (or (pathname-type path) ""))
             '("lisp" "lsp" "cl" "asd" "clj" "cljs" "cljc" "rkt" "scm" "ss") :test #'equal))
   (xeon.folders:folder-files directory :recursive recursive)))

(defun folder-outline (directory buffers &key recursive)
  "Index source children of DIRECTORY, preferring unsaved open buffers."
  (let ((result nil))
    (dolist (path (outline-source-files directory :recursive recursive))
        (let ((buffer (or (find (truename path) buffers :key #'xeon.core:buffer-pathname
                               :test #+win32 #'equalp #-win32 #'equal)
                          (xeon.core:open-buffer-file path))))
          (setf result (nconc result (buffer-outline buffer)))))
    (stable-sort result #'string-lessp :key #'outline-symbol-name)))

(defun buffer-outline (buffer)
  "Source definitions, without evaluating or interning source. Reads bounded chunks.
Namespaces are packages in CL, ns forms in Clojure, and modules in Racket."
  (let* ((language (detect-language buffer))
         (namespace (if (eq language :common-lisp) "cl-user"
                        (if (eq language :clojure) "user" (xeon.core:buffer-name buffer))))
         (session (make-syntax-session buffer)) (stack nil) (result nil)
         (namespaces (list (cons 0 namespace)))
         (token (make-string-output-stream)) (token-start nil))
    (unless (member language '(:common-lisp :clojure :racket))
      (return-from buffer-outline nil))
    (labels ((emit-token ()
               (when token-start
                 (let* ((name (get-output-stream-string token)) (frame (first stack))
                        (kind (first frame)))
                   (when frame
                     (cond
                       ((null kind) (setf (first frame) (string-downcase name)))
                       ((not (second frame))
                        (setf (second frame) t)
                        (cond
                          ((member kind '("in-package" "ns" "module" "module*") :test #'equal)
                           (setf namespace (string-trim "\"':#" name))
                           (push (cons token-start namespace) namespaces))
                          ((member kind '("defun" "defmacro" "defmethod" "defgeneric" "defclass"
                                          "defstruct" "deftype" "defvar" "defparameter" "defconstant"
                                          "defpackage" "define-condition" "defsetf" "define-symbol-macro"
                                          "def" "defn" "defn-" "defmacro" "defmulti" "defmethod"
                                          "defprotocol" "defrecord" "deftype" "define" "define-syntax"
                                          "define-syntax-rule" "define/contract" "define-function" "struct") :test #'equal)
                           (push (make-outline-symbol :namespace namespace :name name :kind kind
                                   :position token-start :pathname (xeon.core:buffer-pathname buffer)) result))))))
                   (setf token-start nil))))
             (open-form ()
               ;; Racket function shorthand and CL structure options.
               (if (and stack (not (second (first stack)))
                        (member (first (first stack)) '("define" "define/contract" "define-syntax-rule" "defstruct") :test #'equal))
                   (let ((kind (first (first stack))))
                     (setf (second (first stack)) t)
                     (push (list (if (equal kind "define") "define-function" kind) nil) stack))
                   (push (list nil nil) stack))))
      (loop while (syntax-pending-p session) do
        (multiple-value-bind (start bytes text styles) (next-syntax-block session 4096)
          (declare (ignore bytes))
          (loop for c across text for style across styles for at from start do
            (cond
              ((= style 3) (emit-token))
              ((and (/= style 2) (find c "([{)]}"))
               (emit-token)
               (if (find c "([{") (open-form) (pop stack)))
              ((spacep c) (emit-token))
              ((or (/= style 2)
                   (and stack (member (first (first stack)) '("in-package" "module" "module*") :test #'equal)))
               (unless token-start (setf token-start at))
               (write-char c token))
              (t (emit-token))))))
      (emit-token))
    (values (stable-sort result
                 (lambda (a b)
                   (if (string-equal (outline-symbol-namespace a) (outline-symbol-namespace b))
                       (string-lessp (outline-symbol-name a) (outline-symbol-name b))
                       (string-lessp (outline-symbol-namespace a) (outline-symbol-namespace b)))))
            ;; A newly opened file has point zero, before its package form.
            ;; Use the first declared namespace for that preamble rather than
            ;; showing unrelated CL-USER definitions from the entire project.
            (let ((entry (find-if (lambda (entry) (<= (car entry) (xeon.core:buffer-point buffer))) namespaces)))
              (cdr (if (and (zerop (car entry)) (rest namespaces))
                       (car (last namespaces 2))
                       entry)))
            (reverse namespaces))))
