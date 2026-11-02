;;;; The LGT editor shell. Widgets, layout and input translation live here;
;;;; documents, editing rules, language behavior and runtimes stay in the backend.

(defpackage #:xeon.ui
  (:use #:cl #:lgt)
  (:export #:start #:make-workbench #:workbench #:workbench-window #:workbench-manager
           #:workbench-tabs #:workbench-explorer #:workbench-tools #:workbench-repl
           #:active-editor #:active-document #:document-buffer #:document-text
           #:new-document #:open-document #:save-document #:save-document-as #:close-document
           #:open-folder #:refresh-explorer #:toggle-sidebar #:toggle-tools
           #:select-tool #:show-settings #:apply-theme))
