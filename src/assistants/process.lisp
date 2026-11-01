(in-package #:xeon.assistants)

(defun assistant-available-p (provider)
  "Whether the CLI discovery adapter can resolve PROVIDER; never starts a process."
  (handler-case (not (null (assistant-program provider)))
    (error () nil)))

;; Process and executable discovery are the OS adapter. No shell interpolation.
(defun assistant-program (provider)
  (let* ((name (ecase provider (:codex "codex") (:claude "claude")))
         (home (user-homedir-pathname))
         (paths (uiop:split-string (or (uiop:getenv "PATH") "")
                                  :separator (if (uiop:os-windows-p) ";" ":")))
         (candidates (append
           (loop for path in paths unless (zerop (length path)) append
             (loop for suffix in (if (uiop:os-windows-p) '(".exe") '(""))
                   collect (merge-pathnames (concatenate 'string name suffix) (uiop:ensure-directory-pathname path))))
           (list (merge-pathnames (format nil ".local/bin/~A~A" name (if (uiop:os-windows-p) ".exe" "")) home))
           (when (and (eq provider :codex) (uiop:os-macosx-p))
             (list #P"/Applications/ChatGPT.app/Contents/Resources/codex-cli/CodexCLI.app/Contents/MacOS/codex")))))
    (or (find-if #'probe-file candidates)
        (error "Install ~A and sign in with its CLI before opening this assistant." name))))

(defun launch-assistant-process (command directory)
  (uiop:launch-program command :directory directory :input :stream :output :stream
                       :error-output :stream :external-format '(:utf-8 :replacement #\?)))

(defun stop-assistant-process (process &key (grace-seconds 5) (kill-seconds 2))
  "Release the owned client's persistence writer before replacing it.
Closing stdin requests normal CLI shutdown, including SessionEnd hooks. Signal
delivery alone does not mean the process has exited or its writer lease is free."
  (labels ((await-exit (seconds)
             (loop with deadline = (+ (get-internal-real-time) (* seconds internal-time-units-per-second))
                   while (uiop:process-alive-p process)
                   when (>= (get-internal-real-time) deadline) return nil
                   do (sleep .02)
                   finally (return t))))
    (let ((input (uiop:process-info-input process)))
      (when input (ignore-errors (close input))))
    (unless (await-exit grace-seconds)
      (uiop:terminate-process process :urgent t)
      (unless (await-exit kill-seconds)
        (error "The previous assistant client has not stopped; the conversation was not reopened.")))
    (uiop:wait-process process)))

(defun bounded-protocol-line (stream)
  (let ((text (make-array 256 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for c = (read-char stream nil) do
      (when (or (null c) (eql c #\Newline))
        (return (unless (and (null c) (zerop (length text))) (coerce text 'simple-string))))
      (when (>= (length text) (* 16 1024 1024)) (error "Assistant protocol line exceeds 16 Mi characters."))
      (vector-push-extend c text))))

(defun open-assistant-url (url)
  "Open an explicit user-clicked authentication/elicitation link."
  (unless (and (stringp url) (or (uiop:string-prefix-p "https://" url) (uiop:string-prefix-p "http://" url)))
    (error "Only HTTP and HTTPS assistant links may be opened."))
  (uiop:launch-program
    (cond ((uiop:os-windows-p) (list "rundll32.exe" "url.dll,FileProtocolHandler" url))
          ((uiop:os-macosx-p) (list "open" url))
          (t (list "xdg-open" url)))
    :output nil :error-output nil))
