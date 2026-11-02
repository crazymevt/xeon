(in-package #:xeon.assistants)

;; Local-only storage, deliberately outside the project's source tree. Never
;; persist approval callbacks, pending requests, credentials or runtime handles.
(defvar *history-lock* (sb-thread:make-mutex :name "Chat history files"))
(defun history-directory-key (directory)
  (uiop:native-namestring (uiop:ensure-directory-pathname (truename directory))))
(defun read-chat-record (path)
  (handler-case
      (with-open-file (in path :external-format :utf-8)
        (when (> (file-length in) (* 16 1024 1024)) (error "Chat record too large."))
        (let ((*read-eval* nil) (*package* (find-package :cl-user)))
          (let ((record (read in nil nil)))
            (when (and (listp record) (eql (getf record :version) 1)) record))))
    (error () nil)))
(defun write-chat-record (path record)
  (ensure-directories-exist path)
  (let ((temporary (make-pathname :type "tmp" :defaults path)))
    (unwind-protect
         (progn
           (with-open-file (out temporary :direction :output :if-exists :supersede
                                :external-format :utf-8)
             (let ((*package* (find-package :cl-user)) (*print-readably* t) (*print-pretty* nil)) (write record :stream out)))
           (uiop:rename-file-overwriting-target temporary path))
      (when (probe-file temporary) (delete-file temporary)))))
(defun preferred-provider (store)
  "Return the last explicitly selected provider, or NIL for missing/invalid settings."
  (sb-thread:with-mutex (*history-lock*)
    (let ((provider (getf (read-chat-record (merge-pathnames "preferences.sexp" store)) :provider)))
      (when (member provider '(:codex :claude)) provider))))

(defun remember-provider (store provider)
  "Persist a selection independently of conversation saves and background updates."
  (unless (member provider '(:codex :claude)) (error "Invalid assistant provider."))
  (sb-thread:with-mutex (*history-lock*)
    (write-chat-record (merge-pathnames "preferences.sexp" store)
                       (list :version 1 :provider provider)))
  provider)

(defun chat-history (store directory &optional provider)
  (let ((key (history-directory-key directory)))
    (sb-thread:with-mutex (*history-lock*)
      (sort (loop for path in (when (probe-file store) (uiop:directory-files store "*.sexp"))
                  for record = (read-chat-record path)
                  when (and record (getf record :id) (equal key (getf record :directory))
                            (or (null provider) (eq provider (getf record :provider))))
                    collect record)
            #'> :key (lambda (record) (getf record :updated 0))))))
(defun restore-session (session history)
  (let* ((store (session-store session))
         (preferences (read-chat-record (merge-pathnames
                       (format nil "preferences-~(~A~).sexp" (session-provider session)) store))))
    (when history
      (unless (and (eq (getf history :provider) (session-provider session))
                   (equal (getf history :directory) (history-directory-key (session-directory session))))
        (error "Chat belongs to another provider or folder.")))
    (let ((settings (or history preferences)))
      (when settings
        (setf (session-mode session) (getf settings :mode :agent)
              (session-model session) (getf settings :model)
              (session-effort session) (getf settings :effort))))
    (setf (session-history-id session) (or (getf history :id)
              (format nil "~D-~36R" (get-universal-time) (random (expt 36 16)))))
    (when history
      (setf (session-conversation session) (getf history :conversation)
            (session-messages session) (copy-tree (getf history :messages))
            (session-text session) (getf history :text "")
            (session-next-message session) (getf history :next-message 0)
            (session-message-epoch session) (getf history :message-epoch 0)))))
(defun save-session (session)
  (when (session-store session)
    (let ((record (sb-thread:with-mutex ((session-lock session))
                    (list :version 1 :id (session-history-id session)
                          :directory (history-directory-key (session-directory session))
                          :provider (session-provider session) :mode (session-mode session)
                          :model (session-model session) :effort (session-effort session)
                          :conversation (session-conversation session) :updated (get-universal-time)
                          :messages (copy-tree (session-messages session)) :text (session-text session)
                          :next-message (session-next-message session) :message-epoch (session-message-epoch session)))))
      (sb-thread:with-mutex (*history-lock*)
        (write-chat-record (merge-pathnames (format nil "~A.sexp" (session-history-id session)) (session-store session)) record)
        (write-chat-record (merge-pathnames (format nil "preferences-~(~A~).sexp" (session-provider session)) (session-store session))
                           (list :version 1 :mode (getf record :mode) :model (getf record :model) :effort (getf record :effort)))))))
(defun change-session-directory (session directory)
  "Retire the old conversation before launching an agent in another folder."
  (let ((directory (truename (uiop:ensure-directory-pathname directory))))
    (enqueue session
      (lambda ()
        (unless (equal (history-directory-key directory) (history-directory-key (session-directory session)))
          ;; Failure to persist history must not leave an agent running in the
          ;; previous project after the editor has opened another folder.
          (let ((save-error (handler-case (progn (save-session session) nil)
                              (error (c) (princ-to-string c)))))
          (retire-process session)
          (setf (session-auto-approve session) nil
                (session-directory session) directory (session-conversation session) nil
                (session-messages session) nil (session-text session) "" (session-next-message session) 0
                (session-usage session) nil (session-context session) nil)
          (when (session-store session) (restore-session session nil))
          (start-provider session nil)
          (when save-error
            (session-update session :text (format nil "~%[Previous chat history could not be saved: ~A]~%" save-error)))))))))
