(in-package #:xeon.syntax)

(defun source-extension (buffer)
  (let* ((path (xeon.core:buffer-pathname buffer))
         (name (xeon.core:buffer-name buffer)) (dot (position #\. name :from-end t)))
    (string-downcase (or (when path (pathname-type path))
                         (when (and (not path) dot) (subseq name (1+ dot))) ""))))

(defparameter *language-choices*
  '((:diff "Diff / patch") (:plain "Plain text") (:common-lisp "Common Lisp") (:clojure "Clojure")
    (:racket "Racket") (:json "JSON / JSONC") (:python "Python")
    (:javascript "JavaScript / TypeScript") (:c-like "C / C++ / Java / C#")
    (:shell "Shell") (:markdown "Markdown") (:yaml "YAML")
    (:config "TOML / INI") (:markup "HTML / XML") (:css "CSS / SCSS")))

(defun language-choices ()
  "Return supported language IDs and display labels."
  (copy-tree *language-choices*))

(defun set-buffer-language (buffer language)
  "Set a buffer-local override, or NIL to resume automatic detection.
This changes neither document text nor its modified state."
  (unless (or (null language) (assoc language *language-choices*))
    (error "Unsupported language: ~S" language))
  (setf (xeon.core:buffer-language-override buffer) language))

(defun detect-language (buffer)
  (when (xeon.core:buffer-language-override buffer)
    (return-from detect-language (xeon.core:buffer-language-override buffer)))
  (let* ((extension (source-extension buffer))
         (prefix (xeon.core:buffer-substring buffer 0 (min 256 (xeon.core:buffer-length buffer)))))
    (or (loop for (language . extensions) in
              '((:diff "diff" "patch") (:common-lisp "lisp" "lsp" "cl" "asd") (:clojure "clj" "cljs" "cljc" "edn")
                (:racket "rkt" "rktd" "rktl" "scm" "ss" "scrbl") (:json "json" "jsonc")
                (:python "py" "pyw") (:javascript "js" "mjs" "cjs" "jsx" "ts" "tsx")
                (:c-like "c" "h" "cc" "cpp" "hpp" "java" "cs") (:shell "sh" "bash" "zsh")
                (:markdown "md" "markdown") (:yaml "yml" "yaml") (:config "toml" "ini")
                (:markup "html" "htm" "xml" "svg") (:css "css" "scss"))
              when (member extension extensions :test #'equal) return language)
        (when (and (>= (length prefix) 5) (string= "#lang" prefix :end2 5)
                   (or (= (length prefix) 5) (find (char prefix 5) '(#\Space #\Tab #\Newline #\Return)))) :racket)
        (when (and (>= (length prefix) 2) (string= "#!" prefix :end2 2))
          (let ((line (subseq prefix 0 (position #\Newline prefix))))
            (cond ((search "python" line) :python) ((search "racket" line) :racket)
                  ((or (search "sbcl" line) (search "clisp" line)) :common-lisp)
                  ((search "clojure" line) :clojure) ((search "node" line) :javascript)
                  ((or (search "sh" line) (search "bash" line)) :shell))))
        :plain)))

(defparameter *keywords*
  '((:common-lisp "defun" "defmacro" "defmethod" "defgeneric" "defclass" "defstruct" "defpackage" "in-package"
     "defvar" "defparameter" "defconstant" "lambda" "let" "let*" "flet" "labels" "macrolet" "symbol-macrolet"
     "if" "when" "unless" "cond" "case" "ecase" "typecase" "and" "or" "not" "progn" "prog1" "prog2"
     "setf" "setq" "psetf" "loop" "do" "do*" "dolist" "dotimes" "block" "return" "return-from" "tagbody" "go"
     "catch" "throw" "unwind-protect" "handler-case" "handler-bind" "restart-case" "restart-bind" "declare"
     "the" "locally" "eval-when" "quote" "function" "multiple-value-bind" "multiple-value-call" "with-open-file"
     "with-output-to-string" "with-input-from-string" "assert" "check-type" "declaim" "proclaim")
    (:clojure "def" "defn" "defn-" "defmacro" "defmulti" "defmethod" "defprotocol" "defrecord" "deftype"
     "ns" "in-ns" "require" "import" "refer" "fn" "fn*" "let" "letfn" "loop" "recur" "if" "if-let" "if-not"
     "when" "when-let" "when-not" "cond" "condp" "case" "do" "doseq" "dotimes" "for" "try" "catch" "finally"
     "throw" "binding" "with-open" "quote" "var" "set!" "new" "->" "->>" "some->" "as->" "and" "or")
    (:racket "#lang" "#reader" "define" "define-values" "define-syntax" "define-syntax-rule" "define/contract"
     "define-struct" "struct" "lambda" "λ" "case-lambda" "let" "let*" "letrec" "let-values" "let*-values"
     "if" "cond" "case" "when" "unless" "begin" "begin0" "set!" "quote" "quasiquote" "unquote" "syntax"
     "syntax-case" "syntax-rules" "require" "provide" "module" "module+" "module*" "match" "match-define"
     "for" "for/list" "for/fold" "for*" "with-handlers" "parameterize" "and" "or")
    (:python "def" "class" "return" "if" "elif" "else" "for" "while" "in" "is" "not" "and" "or" "lambda"
     "try" "except" "finally" "raise" "with" "as" "import" "from" "yield" "async" "await" "pass" "break"
     "continue" "global" "nonlocal" "del" "assert" "match" "case")
    (:javascript "function" "const" "let" "var" "class" "extends" "new" "return" "if" "else" "switch" "case"
     "for" "while" "do" "break" "continue" "try" "catch" "finally" "throw" "import" "export" "from" "default"
     "async" "await" "yield" "typeof" "instanceof" "in" "of" "interface" "type" "enum" "public" "private")
    (:c-like "if" "else" "for" "while" "do" "switch" "case" "default" "return" "break" "continue" "class"
     "struct" "enum" "typedef" "const" "static" "void" "int" "char" "float" "double" "bool" "auto" "new"
     "delete" "public" "private" "protected" "virtual" "override" "namespace" "using" "template" "try" "catch"
     "throw" "import" "package" "interface" "extends" "implements")
    (:shell "if" "then" "else" "elif" "fi" "for" "in" "do" "done" "while" "case" "esac" "function" "select"
     "export" "local" "return" "break" "continue" "source" "readonly")
    (:json "true" "false" "null") (:yaml "true" "false" "null" "yes" "no") (:config "true" "false")))

(defun lisp-language-p (language) (member language '(:common-lisp :clojure :racket)))
(defparameter *keyword-tables*
  (loop for (language . words) in *keywords*
        collect (cons language (let ((table (make-hash-table :test #'equal)))
                                 (dolist (word words) (setf (gethash word table) t)) table))))
(defun keyword-token-p (token language)
  (or (and (lisp-language-p language) (plusp (length token)) (char= (char token 0) #\:))
      (let ((table (cdr (assoc language *keyword-tables*))))
        (when table (gethash (if (eq language :common-lisp) (string-downcase token) token) table)))))
(defparameter *number-pattern*
  (cl-ppcre:create-scanner "(?i)^[+-]?(?:#[box][0-9a-f]+|#[0-9]+r[0-9a-z]+|0[xob][0-9a-f]+|[0-9]+/[0-9]+|(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[esfdl][+-]?[0-9]+)?[mn]?)$"))
(defun token-style (token language)
  (cond ((keyword-token-p token language) 1)
        ((or (cl-ppcre:scan *number-pattern* token)
             (member token (if (eq language :common-lisp) '("nil" "t") '("nil" "true" "false" "null" "None" "True" "False" "#t" "#f" "#true" "#false"))
                     :test (if (eq language :common-lisp) #'string-equal #'string=))) 4)
        ((or (lisp-language-p language) (eq language :css)) 5)
        (t 0)))
