(defpackage #:xeon.syntax
  (:use #:cl)
  (:export #:outline-source-files #:language-choices #:set-buffer-language)
  (:export #:outline-symbol-category #:folder-outline #:buffer-outline #:outline-symbol #:outline-symbol-namespace #:outline-symbol-name #:outline-symbol-kind #:outline-symbol-position #:outline-symbol-pathname #:function-help-at #:buffer-completions #:current-form-range #:detect-language #:make-syntax-session #:syntax-language #:syntax-position
           #:syntax-byte-position #:syntax-pending-p #:invalidate-syntax #:refresh-syntax-language
           #:format-selection #:make-selection-format-job #:format-job-selection-p #:format-job-context-position #:format-job-context-target #:format-document #:format-document-supported-p #:make-format-job #:step-format-job #:apply-format-job
           #:format-job-output-growth #:format-job-layout-left #:format-job-phase #:format-job-position #:format-job-buffer #:format-job-state #:format-job-line #:format-job-total #:formatting-error #:set-lisp-indentation-rule #:matching-brackets #:insert-indented-newline #:newline-indentation #:next-syntax-block))
