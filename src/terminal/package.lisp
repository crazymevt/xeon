(defpackage #:xeon.terminal
  (:use #:cl)
  (:export #:terminal-session #:make-terminal #:start-terminal #:close-terminal
           #:poll-terminal #:resize-terminal #:terminal-snapshot #:terminal-status
           #:terminal-directory #:terminal-id #:terminal-shell #:terminal-revision #:terminal-command-running-p
           #:send-key #:send-mouse #:paste-text #:set-terminal-colours #:terminal-error
           #:terminal-library-path #:terminal-available-p))
