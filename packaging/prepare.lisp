(require :asdf)
(load (merge-pathnames "quicklisp.lisp" (uiop:getcwd)))
(uiop:symbol-call :quicklisp-quickstart :install
                  :path (merge-pathnames "quicklisp/" (uiop:getcwd)))
(asdf:load-asd (merge-pathnames "xeon.asd" (uiop:getcwd)))
(uiop:symbol-call :ql :quickload
                  #+(and unix (not darwin)) '("xeon/ui" "lgt/gtk/native")
                  #-(and unix (not darwin)) "xeon/ui")
