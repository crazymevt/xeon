(in-package #:xeon.terminal)

;; Terminal sessions have identity and own a native PTY/ConPTY lifetime. They are
;; independent of documents and GUI objects. Calls are serialized by their owner;
;; native workers only move bounded byte queues, never enter Lisp or libvterm.
(defvar *next-id* 0)
(defclass terminal-session ()
  ((id :initform (incf *next-id*) :reader terminal-id)
   (handle :initform nil :accessor handle)
   (directory :initarg :directory :reader terminal-directory)
   (shell :initarg :shell :reader terminal-shell)
   (rows :initarg :rows :accessor rows)
   (columns :initarg :columns :accessor columns)
   (status :initform :new :accessor terminal-status)
   (revision :initform 0 :accessor terminal-revision)
   (exit-code :initform nil :accessor exit-code)))

(defun make-terminal (&key (directory (user-homedir-pathname)) shell (rows 24) (columns 80))
  (unless (and (integerp rows) (<= 2 rows 100) (integerp columns) (<= 10 columns 300))
    (error 'terminal-error :detail "Terminal size must be 2..100 rows and 10..300 columns."))
  (when (and shell (or (not (stringp shell)) (find #\Null shell) (find #\" shell)))
    (error 'terminal-error :detail "Shell must be an executable path, not a command line."))
  (let ((directory (uiop:ensure-directory-pathname (truename directory))))
    (unless (uiop:directory-exists-p directory)
      (error 'terminal-error :detail "Terminal working directory does not exist."))
    (make-instance 'terminal-session :directory directory :shell shell :rows rows :columns columns)))

(defun start-terminal (session)
  (unless (eq :new (terminal-status session))
    (error 'terminal-error :detail "Terminal has already been started or closed."))
  (ensure-library)
  (let ((pointer (%create (rows session) (columns session))))
    (when (cffi:null-pointer-p pointer) (error 'terminal-error :detail (%error)))
    (unwind-protect
         (progn
           (when (zerop (%start pointer (uiop:native-namestring (terminal-directory session))
                                     (or (terminal-shell session) "")))
             (error 'terminal-error :detail (%error)))
           (setf (handle session) pointer (terminal-status session) :running pointer nil)
           (incf (terminal-revision session)))
      (when pointer (%destroy pointer))))
  session)

(defun close-terminal (session)
  "Idempotently terminate the owned terminal and release its native resources."
  (when (handle session) (%destroy (handle session)) (setf (handle session) nil))
  (unless (eq :closed (terminal-status session))
    (setf (terminal-status session) :closed) (incf (terminal-revision session)))
  session)

(defun poll-terminal (session)
  "Consume at most 64 KiB of output. Revision is the explicit change notification."
  (when (handle session)
    (let ((count (%poll (handle session))) (exit (%status (handle session))))
      (when (plusp count) (incf (terminal-revision session)))
      (when (and (/= exit -1) (eq :running (terminal-status session)))
        (setf (terminal-status session) :exited (exit-code session) exit)
        (incf (terminal-revision session)))))
  (terminal-revision session))

(defun terminal-command-running-p (session)
  "Whether closing needs confirmation. Idle shells do not need confirmation.
POSIX checks the foreground process group; Windows checks shell job children.
OS query failures conservatively require confirmation."
  (when (handle session)
    (poll-terminal session)
    (and (eq :running (terminal-status session))
         (plusp (%command-running (handle session))))))

(defun resize-terminal (session rows columns)
  (when (and (handle session) (or (/= rows (rows session)) (/= columns (columns session))))
    (unless (= 1 (%resize (handle session) rows columns))
      (error 'terminal-error :detail "Could not resize terminal."))
    (setf (rows session) rows (columns session) columns)
    (incf (terminal-revision session)))
  session)

(defun send-key (session &key (key 0) (unicode 0) (modifiers 0))
  "KEY is a libvterm key ID; MODIFIERS are Shift=1, Alt=2, Control=4."
  (unless (and (eq :running (terminal-status session)) (handle session)
               (= 1 (%key (handle session) key unicode modifiers)))
    (error 'terminal-error :detail "Terminal is not running or its input queue is full.")))

(defun paste-text (session text)
  "Send UTF-8 text with negotiated bracketed-paste framing. Never truncate input."
  ;; Prevent pasted ESC/control payloads from breaking out of bracketed paste.
  (let* ((text (remove-if (lambda (c) (and (or (< (char-code c) 32) (<= 127 (char-code c) 159))
                                          (not (find c '(#\Tab #\Newline #\Return))))) text))
         (bytes (babel:string-to-octets text :encoding :utf-8)))
    (unless (<= (length bytes) 65536)
      (error 'terminal-error :detail "Paste exceeds the terminal's 64 KiB limit."))
    (unless (eq :running (terminal-status session))
      (error 'terminal-error :detail "Terminal is not running."))
    (cffi:with-pointer-to-vector-data (pointer bytes)
      (unless (= 1 (%paste (handle session) pointer (length bytes)))
        (error 'terminal-error :detail "Terminal input queue is full; paste was not sent.")))))

(defun set-terminal-colours (session foreground background)
  (when (handle session)
    (%colours (handle session) foreground background)
    (incf (terminal-revision session))))

(defun send-mouse (session row column &key (button 0) pressed (modifiers 0))
  (when (eq :running (terminal-status session))
    (%mouse (handle session) row column button (if pressed 1 0) modifiers)))

(defun terminal-snapshot (session)
  "Copied bounded screen/scrollback projection; styles and cursor use UTF-8 bytes."
  (when (handle session)
    (let* ((pointer (handle session)) (text (%snapshot pointer))
           (length (%length pointer)) (style-pointer (%styles pointer))
           (styles (make-array length :element-type '(unsigned-byte 8))))
      (dotimes (i length) (setf (aref styles i) (cffi:mem-aref style-pointer :uint8 i)))
      (list :text (cffi:foreign-string-to-lisp text :count length :encoding :utf-8)
            :styles styles :cursor (%cursor pointer) :cursor-visible (plusp (%cursor-visible pointer))
            :mouse (plusp (%mouse-mode pointer)) :history-lines (%history-lines pointer)
            :palette (loop for i below (%palette-count pointer)
                           collect (list (if (< i 32) i (+ i 8))
                                         (%palette pointer i 0) (%palette pointer i 1) (%palette pointer i 2)))
            :revision (terminal-revision session) :status (terminal-status session)
            :exit-code (exit-code session) :rows (rows session) :columns (columns session)))))
