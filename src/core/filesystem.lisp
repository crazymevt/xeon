(in-package #:xeon.core)

;; This is the only implementation/OS-specific file adapter. Do not substitute
;; UIOP's delete-then-rename Windows fallback: a failed save must not delete the
;; original before attempting replacement.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(and sbcl (not win32)) (require :sb-posix))

(defun existing-file-mode (path)
  #+(and sbcl (not win32))
  (progn
    (sb-posix:access (sb-ext:native-namestring path) sb-posix:w-ok)
    (logand #o777 (sb-posix:stat-mode (sb-posix:stat (sb-ext:native-namestring path)))))
  #+(and sbcl win32)
  (progn
    (check-windows-file-writable path)
    ;; Request write access without truncation or data writes, so replacement
    ;; cannot bypass an existing file's ACL just through directory permissions.
    (with-open-file (stream path :direction :io :if-exists :overwrite
                                :if-does-not-exist :error :element-type '(unsigned-byte 8))
      (declare (ignore stream)))
    nil)
  #-sbcl (declare (ignore path))
  #-sbcl nil)

(defun set-file-mode (path mode)
  #+(and sbcl (not win32))
  (when mode (sb-posix:chmod (sb-ext:native-namestring path) mode))
  #-(and sbcl (not win32)) (declare (ignore path mode)))

(defun utf16-code-units (string)
  "NUL-terminated UTF-16 for the Windows wide-character APIs."
  (let ((units (make-array 0 :element-type '(unsigned-byte 16)
                            :fill-pointer 0 :adjustable t)))
    (loop for char across string for code = (char-code char) do
      (when (or (zerop code) (<= #xd800 code #xdfff) (> code #x10ffff))
        (error "Invalid character in Windows pathname."))
      (if (< code #x10000) (vector-push-extend code units)
          (let ((offset (- code #x10000)))
            (vector-push-extend (+ #xd800 (ash offset -10)) units)
            (vector-push-extend (+ #xdc00 (logand offset #x3ff)) units))))
    (vector-push-extend 0 units)
    (coerce units '(simple-array (unsigned-byte 16) (*)))))

#+(and sbcl win32)
(progn
  (eval-when (:load-toplevel :execute)
    (sb-alien:load-shared-object "kernel32.dll"))
  (sb-alien:define-alien-routine ("MoveFileExW" %move-file-ex-wide) sb-alien:int
    (source (* sb-alien:unsigned-short)) (destination (* sb-alien:unsigned-short))
    (flags sb-alien:unsigned-int))
  (sb-alien:define-alien-routine ("GetFileAttributesW" %file-attributes-wide)
      sb-alien:unsigned-int (path (* sb-alien:unsigned-short)))
  (sb-alien:define-alien-routine ("GetLastError" %windows-last-error) sb-alien:unsigned-int)

  (defun windows-path-units (path)
    (utf16-code-units (sb-ext:native-namestring path)))

  (defun check-windows-file-writable (path)
    (let ((units (windows-path-units path)))
      (sb-sys:with-pinned-objects (units)
        (let ((attributes (%file-attributes-wide
                           (sb-alien:sap-alien (sb-sys:vector-sap units)
                                               (* sb-alien:unsigned-short)))))
          (when (= attributes #xffffffff)
            (error "GetFileAttributesW failed for ~S (Windows error ~D)."
                   path (%windows-last-error)))
          (when (logbitp 0 attributes)
            (error "File ~S has the Windows read-only attribute." path)))))))

(defun replace-file-on-disk (source destination existing-p)
  #-(and sbcl win32) (declare (ignore existing-p))
  #+(and sbcl win32)
  (let ((from (windows-path-units source)) (to (windows-path-units destination)))
    (sb-sys:with-pinned-objects (from to)
      ;; MOVEFILE_WRITE_THROUGH, plus REPLACE_EXISTING only for an observed file.
      ;; No COPY_ALLOWED: staging and destination are on the same volume.
      (when (zerop (%move-file-ex-wide
                    (sb-alien:sap-alien (sb-sys:vector-sap from) (* sb-alien:unsigned-short))
                    (sb-alien:sap-alien (sb-sys:vector-sap to) (* sb-alien:unsigned-short))
                    (if existing-p #x9 #x8)))
        (error "MoveFileExW failed for ~S (Windows error ~D)."
               destination (%windows-last-error)))))
  #-(and sbcl win32)
  (progn
    ;; RENAME-FILE merges missing components, including a source .tmp extension.
    (uiop:rename-file-overwriting-target
     source (make-pathname :type (or (pathname-type destination) :unspecific)
                           :defaults destination))))
