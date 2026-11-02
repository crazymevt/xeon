(defpackage #:xeon.assistants
  (:use #:cl)
  (:export #:assistant-buffer #:make-assistant-buffer #:buffer-session #:sync-assistant-buffer
           #:make-session #:session-snapshot #:start-session #:stop-session #:send-message
           #:set-auto-approve #:configure-session #:clear-context #:interrupt-session #:answer-request
           #:session-provider #:session-program #:session-directory #:session-mode #:session-model #:session-effort
           #:chat-history #:preferred-provider #:remember-provider #:change-session-directory #:session-store #:session-history-id
           #:session-efforts #:runtime-tools #:set-tool-handler #:assistant-available-p))
