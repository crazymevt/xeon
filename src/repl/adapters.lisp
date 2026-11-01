(in-package #:xeon.repl)

(defun swank-request (connection form package on-ok on-abort &key (thread t))
  (swank-rex connection form
             :package (or package (connection-package connection)) :thread thread
             :on-ok (or on-ok #'identity) :on-abort (or on-abort #'default-abort)))

(defmethod backend-supports-p ((connection swank-connection) feature)
  (member feature '(:repl :interrupt :debugger :inspector :compile)))

(defparameter *repl-source-chunk-size* 262144)

(defmethod backend-compile-form ((connection swank-connection) string
                                &key package on-ok on-abort buffer-name (position 1) (line 1) (column 1) file policy)
  (flet ((request (source)
           (let ((form (swank-call "swank:compile-string-for-emacs" "" (or buffer-name "untitled")
                         (list (list :position position) (list :line line column)) file policy)))
             (setf (second form) source) form)))
    (if (> (length string) *repl-source-chunk-size*)
        (swank-upload-source connection string package on-ok on-abort #'request)
        (swank-request connection (request string) package on-ok on-abort :thread :repl-thread))))

(defun swank-diagnostic-evaluation (form)
  "Report warnings during listener evaluation, including LOAD/ASDF operations.
Do not swallow conditions or interfere with the interactive debugger."
  (let ((condition (remote-symbol "krypton-condition")))
    (list (remote-symbol "cl:handler-bind")
      (list
        (list (remote-symbol "swank/backend:compiler-condition")
          (list (remote-symbol "cl:lambda") (list condition)
            (list (remote-symbol "swank::send-to-emacs")
              ;; Swank forwards BACKGROUND-MESSAGE; arbitrary event names are
              ;; rejected by its control thread, even for connected clients.
              (list (remote-symbol "cl:list") :background-message :krypton-problem
                (list (remote-symbol "swank::make-compiler-note") condition))))))
      (list (remote-symbol "swank/backend:with-compilation-hooks") nil form))))

(defun swank-upload-source (connection source package on-ok on-abort build-request)
  ;; Invoke one compile/eval operation on the complete source. Each upload
  ;; request returns NIL rather than echoing the growing string into a frame.
  (let ((symbol (remote-symbol (format nil "CL-USER::~A" (format nil "KRYPTON-SOURCE-~X-~X-~A" (get-universal-time) (random most-positive-fixnum) (gensym)))))
        (offset 0) (abort (or on-abort #'default-abort)))
    (labels ((cleanup ()
               (when (connection-open-p connection)
                 (swank-rex connection
                   (list (remote-symbol "cl:progn") (swank-call "cl:makunbound" symbol)
                         (swank-call "cl:unintern" symbol "COMMON-LISP-USER") nil)
                   :on-ok #'identity :on-abort #'identity)))
             (failed (reason) (cleanup) (funcall abort reason))
             (next (ignored)
               (declare (ignore ignored))
               (if (= offset (length source))
                   (swank-rex connection
                     (funcall build-request (swank-call "cl:symbol-value" symbol))
                     :package (or package (connection-package connection)) :thread :repl-thread
                     :on-ok (lambda (value) (cleanup) (when on-ok (funcall on-ok value))) :on-abort #'failed)
                   (let* ((end (min (length source) (+ offset *repl-source-chunk-size*)))
                          (form (list (remote-symbol "cl:progn")
                                  (list (remote-symbol "cl:replace") (swank-call "cl:symbol-value" symbol)
                                        (subseq source offset end) :start1 offset) nil)))
                     (setf offset end)
                     (swank-rex connection form :thread :repl-thread :on-ok #'next :on-abort #'failed)))))
      (swank-rex connection
        (list (remote-symbol "cl:progn")
          (list (remote-symbol "cl:set") (list 'quote symbol) (swank-call "cl:make-string" (length source))) nil)
        :thread :repl-thread :on-ok #'next :on-abort #'failed))))

(defmethod backend-repl-eval ((connection swank-connection) string &key package on-ok on-abort)
  (if (> (length string) *repl-source-chunk-size*)
      (swank-upload-source connection string package on-ok on-abort
        (lambda (source)
          (swank-diagnostic-evaluation (list (remote-symbol "swank-repl:listener-eval") source))))
      (swank-request connection (swank-diagnostic-evaluation (swank-call "swank-repl:listener-eval" string)) package on-ok on-abort
                     :thread :repl-thread)))

(defmethod backend-set-package ((connection swank-connection) name &key on-ok on-abort)
  (swank-request connection (swank-call "swank:set-package" name) nil on-ok on-abort :thread :repl-thread))

(defmethod backend-send-input ((connection framed-connection) thread tag string)
  ;; Swank's answer to :read-string, which Cadre's Racket back end takes too.
  (swank-send connection (list :emacs-return-string thread tag string)))

(defmethod backend-interrupt ((connection swank-connection))
  (swank-interrupt connection))

(defmethod backend-disconnect ((connection swank-connection))
  (swank-disconnect connection))
(in-package #:xeon.repl)

(defun nrepl-eval-request (connection code &key ns file line column repl on-done on-abort)
  "Evaluate CODE. Output goes to the handler as it comes; with REPL, values
and namespace changes do too. ON-DONE is called with the values (strings),
the error output, and whether it failed."
  (let ((values '()) (err (make-string-output-stream)) (error-size 0) (presentation nil) (failed nil) (id nil))
    (setf id (nrepl-send connection
                         (append (list (cons "op" "eval") (cons "code" code))
                                 (when (and repl (nrepl-eval-function connection)) (list (cons "eval" (nrepl-eval-function connection))))
                                 (and ns (list (cons "ns" ns)))
                                 (and file (list (cons "file" file)))
                                 (and line (list (cons "line" line)))
                                 (and column (list (cons "column" column))))
                         :on-abort on-abort
                         :on-message
                         (lambda (message)
                           (when (dict-get message "krypton-presentation")
                             (setf presentation (dict-get message "krypton-presentation")))
                           (let ((out (dict-get message "out"))
                                 (error-output (dict-get message "err"))
                                 (value (dict-get message "value"))
                                 (new-ns (dict-get message "ns")))
                             (when out (nrepl-event connection (list :write-string out nil)))
                             (when error-output
                               (when (< error-size 65536)
                                 (write-string error-output err :end (min (length error-output) (- 65536 error-size)))
                                 (incf error-size (length error-output)))
                               (nrepl-event connection (list :write-string error-output nil)))
                             (when value
                               (when (< (length values) 100) (push (subseq value 0 (min 65536 (length value))) values))
                               (when repl
                                 (when presentation (nrepl-event connection (list :presentation-start presentation :repl-result)))
                                 (nrepl-event connection (list :write-string (format nil "~a~%" value) :repl-result))
                                 (when presentation (nrepl-event connection (list :presentation-end presentation :repl-result)))
                                 (setf presentation nil)))
                             (when (and repl new-ns (string/= new-ns (connection-package connection)))
                               (setf (connection-package connection) new-ns (connection-prompt connection) new-ns)
                               (nrepl-event connection (list :new-package new-ns new-ns)))
                             (when (nrepl-status-p message "eval-error") (setf failed t))
                             ;; The code reads *in*: ask the REPL for a line.
                             (when (nrepl-status-p message "need-input")
                               (nrepl-event connection (list :read-string nil nil)))
                             (when (nrepl-status-p message "namespace-not-found")
                               (setf failed t)
                               (format err "Namespace ~a isn't loaded: evaluate its ns form, or load the file." ns))))
                         :on-done (lambda (responses)
                                    (declare (ignore responses))
                                    (funcall on-done (reverse values) (get-output-stream-string err) failed))))
    (push id (nrepl-eval-ids connection))
    id))

(defun eval-error-reason (error-output)
  (if (plusp (length error-output)) (string-trim '(#\Newline #\Space) error-output) "Evaluation failed"))

(defun stacktrace-frame (frame)
  "A cider-nrepl stack frame as (name file line column), or nil for its
own tooling frames and repeats."
  (let ((flags (dict-get frame "flags")))
    (unless (intersection '("tooling" "dup") flags :test #'equal)
      (let ((url (dict-get frame "file-url")))
        (list (or (dict-get frame "name") (dict-get frame "method") "?")
              (and (stringp url) (> (length url) 5) (string= "file:" url :end2 5) (subseq url 5))
              (dict-get frame "line")
              nil)))))

(defun nrepl-report-stacktrace (connection)
  "Ask cider-nrepl for the last error's stack (its root cause's) and send it
to the handler as (:post-mortem message class frames), as Cadre's Racket
back end does."
  (let ((op (cond ((nrepl-op-p connection "analyze-last-stacktrace") "analyze-last-stacktrace")
                  ((nrepl-op-p connection "stacktrace") "stacktrace"))))
    (when op
      (nrepl-send connection (list (cons "op" op))
                  :on-abort (lambda (reason) (declare (ignore reason)))
                  :on-done (lambda (responses)
                             (let ((root (car (last (remove-if-not (lambda (m) (dict-get m "class")) responses)))))
                               (when root
                                 (nrepl-event connection
                                              (list :post-mortem
                                                    (or (dict-get root "message") "")
                                                    (dict-get root "class")
                                                    (remove nil (mapcar #'stacktrace-frame
                                                                        (dict-get root "stacktrace"))))))))))))

(defmethod backend-repl-eval ((connection nrepl-connection) string &key on-ok on-abort &allow-other-keys)
  ;; Errors were already shown as the REPL's error output.
  (nrepl-eval-request connection string :ns (connection-package connection) :repl t :on-abort on-abort
                      :on-done (lambda (values error-output failed)
                                 (declare (ignore values))
                                 (when failed
                                   (nrepl-event connection (list :krypton-problem
                                     (list :severity :error :message (eval-error-reason error-output))))
                                   (nrepl-report-stacktrace connection))
                                 (when on-ok (funcall on-ok nil)))))

(defmethod backend-send-input ((connection nrepl-connection) thread tag string)
  (declare (ignore thread tag))
  (nrepl-send connection (list (cons "op" "stdin") (cons "stdin" string))))

(defmethod backend-interrupt ((connection nrepl-connection))
  (dolist (id (nrepl-eval-ids connection))
    (nrepl-send connection (list (cons "op" "interrupt") (cons "interrupt-id" id)))))

(defmethod backend-disconnect ((connection nrepl-connection))
  (nrepl-disconnect connection))

;;; Looking things up

(defun clojure-printed-sexp (text)
  "TEXT, a list Clojure printed, read as data: strings' \\n and \\t become
the characters they stand for."
  (read-sexp
   (with-output-to-string (out)
     (loop with in-string = nil
           for i from 0 below (length text)
           for c = (char text i)
           do (cond ((and in-string (char= c #\\) (< (1+ i) (length text)))
                     (let ((next (char text (1+ i))))
                       (case next
                         (#\n (write-char #\Newline out))
                         (#\t (write-char #\Tab out))
                         (t (write-char c out) (write-char next out))))
                     (incf i))
                    (t (when (char= c #\") (setf in-string (not in-string)))
                       (write-char c out)))))))

(defun cider-inspection (value)
  "cider-nrepl's rendered inspector VALUE as a Swank inspector reply."
  (let* ((parts (loop for part in (clojure-printed-sexp value)
                      collect (if (equal part '(:newline)) (string #\Newline) part)))
         (class (second (find-if (lambda (p) (and (consp p) (eq (first p) :value))) parts))))
    (list :title (or class "") :id 0 :content (list parts (length parts) 0 (length parts)))))

(defun nrepl-inspect-request (connection message on-ok on-abort)
  (nrepl-send connection message
              :on-abort on-abort
              :on-done (lambda (responses)
                         (let ((value (some (lambda (m) (dict-get m "value")) responses))
                               (error-output (some (lambda (m) (dict-get m "err")) responses)))
                           (cond (value (when on-ok (funcall on-ok (cider-inspection value))))
                                 (on-abort (funcall on-abort (eval-error-reason (or error-output "")))))))))

(defun nrepl-inspect (connection code ns &key on-ok on-abort)
  "Evaluate CODE in NS and inspect its value: ON-OK gets a Swank inspector reply."
  (nrepl-inspect-request connection (list (cons "op" "eval") (cons "code" code) (cons "ns" (or ns "user"))
                                          (cons "inspect" "true"))
                         on-ok on-abort))

(defun nrepl-inspect-op (connection op &key index on-ok on-abort)
  "Do the inspector's OP (\"inspect-push\" with INDEX, \"inspect-pop\",
\"inspect-refresh\"): ON-OK gets the object then inspected."
  (nrepl-inspect-request connection (append (list (cons "op" op)) (and index (list (cons "idx" index))))
                         on-ok on-abort))

;;; Tests (cider-nrepl's test-var-query)

(in-package #:xeon.repl)

(defclass racket-connection (framed-connection)
  ((implementation :initform "Racket" :accessor racket-implementation)
   (inspector-p :initform nil :accessor racket-inspector-p))
  (:default-initargs :package "racket" :prompt "racket")
  (:documentation "A connection to Cadre's Racket back end."))

(defmethod connection-language ((connection racket-connection)) :racket)

(defmethod connection-implementation ((connection racket-connection))
  (racket-implementation connection))

(defmethod backend-supports-p ((connection racket-connection) feature)
  (member feature '(:repl :interrupt :post-mortem)))

(defun racket-connect (host port &key (deliver #'funcall) handler on-ready on-failure)
  "Connect to the Racket back end at HOST:PORT; call ON-READY with the
connection once it has said what it is."
  (let ((connection (swank-connect host port :deliver deliver :handler handler :class 'racket-connection)))
    (swank-rex connection '(:connection-info)
               :on-abort (or on-failure #'default-abort)
               :on-ok (lambda (info)
                        (setf (racket-inspector-p connection) (getf info :inspector)
                              (racket-implementation connection) (getf info :implementation)
                              (connection-package connection) (getf info :package)
                              (connection-prompt connection) (getf info :prompt))
                        (when on-ready (funcall on-ready connection))))
    connection))

;;; Requests

(defun racket-request (connection op arguments package on-ok on-abort)
  "Send (OP . ARGUMENTS) to be done with PACKAGE's namespace (nil: the REPL's)."
  (swank-rex connection (cons op arguments) :package package :thread t
                                            :on-ok (or on-ok #'identity) :on-abort (or on-abort #'default-abort)))

(defmethod backend-repl-eval ((connection racket-connection) string &key on-ok on-abort &allow-other-keys)
  (let ((offset 0) (abort (or on-abort #'default-abort)))
    (labels ((done (value) (declare (ignore value)) (when on-ok (funcall on-ok nil)))
             (next (ignored)
               (declare (ignore ignored))
               (if (= offset (length string))
                   (swank-rex connection (list :eval-source) :package nil :thread :repl-thread :on-ok #'done :on-abort abort)
                   (let ((end (min (length string) (+ offset *repl-source-chunk-size*))))
                     (prog1
                       (swank-rex connection (list :put-source offset (subseq string offset end))
                         :package nil :on-ok #'next :on-abort abort)
                       (setf offset end))))))
      (if (> (length string) *repl-source-chunk-size*)
          (swank-rex connection (list :begin-source (length string)) :package nil :on-ok #'next :on-abort abort)
          (swank-rex connection (list :repl-eval string) :package nil :thread :repl-thread :on-ok #'done :on-abort abort)))))

(defmethod backend-interrupt ((connection racket-connection))
  (swank-interrupt connection))

(defmethod backend-disconnect ((connection racket-connection))
  (swank-disconnect connection))

;;; Looking things up

