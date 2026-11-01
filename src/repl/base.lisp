;;;; backend.lisp — what Cadre asks of a running Lisp, whichever Lisp it is
;;;;
;;;; A LISP-CONNECTION is a connection to a running Common Lisp (through
;;;; Swank, swank/), and later Clojure (nREPL) or Racket (Cadre's own back
;;;; end). The editor asks for things through the BACKEND-… generic functions
;;;; below, never a protocol's own calls, except for features only one
;;;; protocol has (the Swank debugger's restarts, the inspector, tracing…),
;;;; which it checks with BACKEND-SUPPORTS-P first.
;;;;
;;;; Requests are asynchronous: ON-OK is called with the value (on the thread
;;;; the connection delivers on, the GTK thread in the editor), or ON-ABORT
;;;; with a reason. Values have Swank's shapes, which the UI already parses
;;;; (forms.lisp, tools.lisp), so each back end answers in those shapes:
;;;;   eval, describe, macroexpand   a string
;;;;   arglist                       a string, or nil
;;;;   autodoc                       (string cacheable-p)
;;;;   completions                   (((name score chunks flags) …) …), fuzzy-completions'
;;;;   definitions                   ((label location) …), locations as PARSE-LOCATION reads
;;;;   xref                          as swank:xref (one kind) or swank:xrefs (several)
;;;;   compile-form, compile-file    a compilation result, as PARSE-COMPILATION-RESULT reads

(in-package #:xeon.repl)

(defclass lisp-connection ()
  ((host :initarg :host :initform nil :reader connection-host)
   (port :initarg :port :initform nil :reader connection-port)
   (state :initform :open :accessor connection-state :documentation ":open or :closed.")
   (package :initarg :package :initform nil :accessor connection-package
            :documentation "The package (or namespace, or module) code is read in by default.")
   (prompt :initarg :prompt :initform nil :accessor connection-prompt)
   (process :initform nil :accessor connection-process
            :documentation "The process running the Lisp, if Cadre started it."))
  (:documentation "A connection to a running Lisp. Subclasses speak a protocol."))

(defun connection-open-p (connection)
  (and connection (eq (connection-state connection) :open)))

(defgeneric connection-language (connection)
  (:documentation "The dialect name of the Lisp CONNECTION runs: :common-lisp, :clojure or :racket."))

(defgeneric connection-implementation (connection)
  (:documentation "A short name for the Lisp, such as \"SBCL 2.6.9\"."))

(defgeneric backend-supports-p (connection feature)
  (:documentation "True if CONNECTION can do FEATURE: :eval :compile :compile-file :load-file
:describe :arglist :autodoc :completions :definitions :xref :macroexpand :repl
:interrupt, and features only some protocols have: :debugger :inspector
:stepper :trace :systems :presentations :image-faces :apropos.")
  (:method (connection feature) (declare (ignore connection feature)) nil))

(defun backend-unsupported (connection what)
  (editor-error "~a can't ~a" (connection-implementation connection) what))

(defmacro define-backend-request (name lambda-list what documentation)
  "Define the generic function NAME, whose default method says the Lisp can't do WHAT."
  (let ((connection (first lambda-list)))
    `(defgeneric ,name (,@lambda-list &key package on-ok on-abort &allow-other-keys)
       (:documentation ,documentation)
       (:method (,@lambda-list &key &allow-other-keys)
         (declare (ignore ,@(rest lambda-list)))
         (backend-unsupported ,connection ,what)))))

(define-backend-request backend-eval (connection string) "evaluate code"
  "Evaluate the forms in STRING; ON-OK gets their values as text.")

(define-backend-request backend-eval-region (connection string) "evaluate code"
  "Evaluate the forms in STRING, a region of a file; ON-OK gets the last value as text.")

(define-backend-request backend-compile-form (connection string) "compile code"
  "Compile STRING, a top-level form from a file. Keys: :buffer-name, :position
(of the form in the file, from 1), :line and :column (from 1), :file, and
:policy (an alist such as ((cl:debug . 3))). ON-OK gets a compilation result.")

(define-backend-request backend-compile-file (connection path) "compile files"
  "Compile the file at PATH (a native namestring), and load it if it
compiled. ON-OK gets the compilation result as soon as it is compiled, and
:on-loaded (a function of no arguments) is called once it is loaded.")

(define-backend-request backend-load-file (connection path) "load files"
  "Load the source file at PATH (a native namestring).")

(define-backend-request backend-describe (connection name) "describe symbols"
  "A description of the symbol NAME, as text.")

(define-backend-request backend-arglist (connection name) "show arglists"
  "The arglist of the operator NAME as text, or nil if it isn't one.")

(define-backend-request backend-autodoc (connection form) "show arglists"
  "The arglist for the call FORM (as RAW-FORM-AT gives it, with the cursor
marked), with the current argument highlighted.")

(define-backend-request backend-completions (connection prefix) "complete symbols"
  "Completions of PREFIX.")

(define-backend-request backend-find-definitions (connection name) "find definitions"
  "Where NAME is defined, as (label location) lists.")

(define-backend-request backend-xref (connection kinds name) "find cross-references"
  "The references of KINDS (a list such as (:calls)) to NAME.")

(define-backend-request backend-macroexpand (connection string) "expand macros"
  "The expansion of the form in STRING: one step, or with :all every macro in it.")

(define-backend-request backend-repl-eval (connection string) "run a REPL"
  "Evaluate STRING as input typed at the REPL. Its output and values are
shown through the connection's events.")

(define-backend-request backend-set-package (connection name) "change packages"
  "Make the package (or namespace) NAME the REPL's. ON-OK gets (name prompt).")

(defgeneric backend-send-input (connection thread tag string)
  (:documentation "Send STRING, a line typed at the REPL, to the program that
asked for input with a (:read-string THREAD TAG) event.")
  (:method (connection thread tag string)
    (declare (ignore thread tag string))
    (backend-unsupported connection "read input")))

(defgeneric backend-interrupt (connection)
  (:documentation "Interrupt what the Lisp is doing for the REPL.")
  (:method (connection) (backend-unsupported connection "be interrupted")))

(defgeneric backend-disconnect (connection)
  (:documentation "Close CONNECTION."))
