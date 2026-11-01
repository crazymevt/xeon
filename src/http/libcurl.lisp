(in-package #:xeon.http)

;; All native ABI and SBCL threading details are confined to this adapter.
;; CURLOPT long arguments MUST use :long (32 bits on Windows, 64 on Unix).
(cffi:define-foreign-library libcurl
  (:darwin "libcurl.4.dylib")
  (:windows (:or "libcurl.dll" "libcurl-x64.dll"))
  (:unix "libcurl.so.4"))
(defvar *library-path* nil "Absolute path to the packaged libcurl; set before initialization.")
(defvar *initialized* nil)
(defvar *ca-file* nil "Packaged CA bundle used unless a request supplies its own CA file.")
(defvar *bundle-checked* nil)
(defvar *initialization-lock* (sb-thread:make-mutex :name "HTTP transport initialization"))
(cffi:defcfun ("curl_global_init" %global-init) :int (flags :long))
(cffi:defcfun ("curl_easy_init" %easy-init) :pointer)
(cffi:defcfun ("curl_easy_cleanup" %easy-cleanup) :void (handle :pointer))
(cffi:defcfun ("curl_easy_perform" %perform) :int (handle :pointer))
(cffi:defcfun ("curl_easy_strerror" %strerror) :string (code :int))
(cffi:defcfun ("curl_slist_append" %append-header) :pointer (list :pointer) (text :string))
(cffi:defcfun ("curl_slist_free_all" %free-headers) :void (list :pointer))

(defun check-code (code)
  (unless (zerop code) (fail-request (%strerror code) code)))

(defun configure-bundle (&optional directory)
  "Select an application-private libcurl + CA bundle, never a CWD-relative library.
An explicit or environment-configured bundle must exist; missing default bundles
fall back to the OS library for source-tree development only."
  (let* ((configured (or directory (uiop:getenv "XEON_HTTP_BUNDLE")))
         (root (if configured (uiop:ensure-directory-pathname configured)
                   (asdf:system-relative-pathname "xeon/http" "native/http/")))
         (filename #+darwin "libcurl.4.dylib" #+win32 "libcurl.dll"
                   #-(or darwin win32) "libcurl.so.4")
         (library (merge-pathnames filename root)) (ca (merge-pathnames "cacert.pem" root)))
    (unless (uiop:absolute-pathname-p root) (fail-request "HTTP bundle path must be absolute"))
    (when (or configured (probe-file library))
      (unless (and (probe-file library) (probe-file ca))
        (fail-request "HTTP bundle is incomplete (libcurl or CA bundle is missing)"))
      (when (and *initialized* (not (equal *library-path* (namestring (truename library)))))
        (fail-request "Cannot replace libcurl after initialization"))
      (setf *library-path* (namestring (truename library)) *ca-file* (namestring (truename ca))))
    (setf *bundle-checked* t)
    *library-path*))

(defun initialize-transport ()
  "Initialize once before starting HTTP workers. No native library is loaded at ASDF load time.
Release packages should set *LIBRARY-PATH* to an absolute, trusted app-private path.
Keep global state alive until process exit; cleanup could invalidate other callers."
  (sb-thread:with-mutex (*initialization-lock*)
    (unless *initialized*
      (unless (or *library-path* *bundle-checked*) (configure-bundle))
      (if *library-path*
          (progn
            (unless (uiop:absolute-pathname-p (pathname *library-path*))
              (fail-request "Packaged libcurl path must be absolute"))
            (cffi:load-foreign-library *library-path*))
          (cffi:use-foreign-library libcurl))
      (check-code (%global-init 3)) ; CURL_GLOBAL_ALL
      (setf *initialized* t)))
  t)

(defmacro set-option (handle option type value)
  `(let ((code (cffi:foreign-funcall-varargs "curl_easy_setopt" (:pointer ,handle :int ,option)
                                   ,type ,value :int)))
     (unless (zerop code)
       (fail-request (format nil "libcurl option ~D: ~A" ,option (%strerror code)) code))))

(defstruct transfer
  (body (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (headers (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  body-limit header-limit failure cancelled-p)
(defvar *transfer* nil) ; dynamically bound per synchronous perform, including worker threads

(defun transfer-aborted-p ()
  (and (transfer-cancelled-p *transfer*) (funcall (transfer-cancelled-p *transfer*))))

(defun receive-bytes (pointer count headers-p)
  ;; Never unwind a Lisp condition through a C stack frame.
  (handler-case
      (let* ((out (if headers-p (transfer-headers *transfer*) (transfer-body *transfer*)))
             (limit (if headers-p (transfer-header-limit *transfer*) (transfer-body-limit *transfer*))))
        (when (transfer-aborted-p) (return-from receive-bytes 0))
        (when (> (+ (length out) count) limit)
          (setf (transfer-failure *transfer*) "Response exceeds configured byte limit")
          (return-from receive-bytes 0))
        (loop for i below count do (vector-push-extend (cffi:mem-aref pointer :unsigned-char i) out))
        count)
    (error () (setf (transfer-failure *transfer*) "Cannot receive response bytes") 0)))

(cffi:defcallback receive-body :size ((data :pointer) (size :size) (count :size) (user :pointer))
  (declare (ignore user)) (receive-bytes data (* size count) nil))
(cffi:defcallback receive-headers :size ((data :pointer) (size :size) (count :size) (user :pointer))
  (declare (ignore user)) (receive-bytes data (* size count) t))

(cffi:defcallback transfer-progress :int ((user :pointer) (total-down :int64) (down :int64)
                                        (total-up :int64) (up :int64))
  (declare (ignore user total-down down total-up up))
  (handler-case (if (transfer-aborted-p) 1 0) (error () 1)))

(defun send-request (request &key cancelled-p)
  "Perform a bounded, synchronous HTTP(S) request. Returns 4xx/5xx as responses.
No redirects, persistent cookies, netrc credentials, or external processes.
TLS peer and hostname verification are mandatory. Call off the GUI thread."
  (validate-request request)
  (initialize-transport)
  (let* ((handle (%easy-init)) (headers (cffi:null-pointer)) (payload (cffi:null-pointer))
         (*transfer* (make-transfer :cancelled-p cancelled-p :body-limit (request-max-body-bytes request)
                                   :header-limit (request-max-header-bytes request))))
    (when (cffi:null-pointer-p handle) (fail-request "Cannot allocate request handle"))
    (unwind-protect
         (progn
           (set-option handle 10002 :string (request-url request)) ; URL
           (set-option handle 181 :long 3) ; PROTOCOLS: HTTP | HTTPS (also older system libcurl)
           (set-option handle 182 :long 3) ; REDIR_PROTOCOLS, defense in depth
           (set-option handle 52 :long 0) ; FOLLOWLOCATION: no credential-bearing redirects
           (set-option handle 64 :long 1) ; SSL_VERIFYPEER
           (set-option handle 81 :long 2) ; SSL_VERIFYHOST
           (set-option handle 51 :long 0) ; NETRC_IGNORED
           (set-option handle 45 :long 0) ; FAILONERROR disabled: preserve error bodies
           (set-option handle 41 :long 0) ; VERBOSE disabled: do not log secrets
           (set-option handle 99 :long 1) ; NOSIGNAL for threaded callers
           (set-option handle 155 :long (request-timeout-ms request))
           (set-option handle 156 :long (request-connect-timeout-ms request))
           (set-option handle 20011 :pointer (cffi:callback receive-body))
           (set-option handle 20079 :pointer (cffi:callback receive-headers))
           (set-option handle 43 :long 0) ; NOPROGRESS off for cooperative cancellation
           (set-option handle 20219 :pointer (cffi:callback transfer-progress))
           (when (or (request-ca-file request) *ca-file*)
             (set-option handle 10065 :string (or (request-ca-file request) *ca-file*)))
           (dolist (header (request-headers request))
             (let ((next (%append-header headers
                                         (if (zerop (length (cdr header)))
                                             (format nil "~A;" (car header)) ; libcurl's empty-header syntax
                                             (format nil "~A: ~A" (car header) (cdr header))))))
               (when (cffi:null-pointer-p next) (fail-request "Cannot allocate request headers"))
               (setf headers next)))
           (unless (cffi:null-pointer-p headers) (set-option handle 10023 :pointer headers))
           (when (request-body request)
             (let* ((body (request-body request))
                    (bytes (if (stringp body) (babel:string-to-octets body :encoding :utf-8) body)))
               (when (> (length bytes) 2147483647) (fail-request "Request body is too large"))
               (setf payload (cffi:foreign-alloc :unsigned-char :count (max 1 (length bytes))))
               (loop for byte across bytes for i from 0 do (setf (cffi:mem-aref payload :unsigned-char i) byte))
               (set-option handle 60 :long (length bytes)) ; POSTFIELDSIZE, binary-safe
               (set-option handle 10015 :pointer payload)))
           (when (string-equal "HEAD" (request-method request)) (set-option handle 44 :long 1))
           ;; Set last: POSTFIELDS changes the implicit method.
           (set-option handle 10036 :string (request-method request))
           (let ((code (%perform handle)))
             (when (transfer-failure *transfer*) (fail-request (transfer-failure *transfer*) code))
             (check-code code))
           (cffi:with-foreign-objects ((status :long) (elapsed :double))
             (check-code (cffi:foreign-funcall-varargs "curl_easy_getinfo" (:pointer handle :int #x200002)
                                              :pointer status :int))
             (check-code (cffi:foreign-funcall-varargs "curl_easy_getinfo" (:pointer handle :int #x300003)
                                              :pointer elapsed :int))
             (%make-response :status (cffi:mem-ref status :long)
                             :elapsed-seconds (cffi:mem-ref elapsed :double)
                             :body (transfer-body *transfer*) :headers (transfer-headers *transfer*))))
      (%easy-cleanup handle)
      (unless (cffi:null-pointer-p headers) (%free-headers headers))
      (unless (cffi:null-pointer-p payload) (cffi:foreign-free payload)))))
