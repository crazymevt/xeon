;;; Standalone headless suite. Fixture runner supplies a loopback server URL.
(load (merge-pathnames "../tools/bootstrap.lisp" *load-truename*))
(asdf:initialize-output-translations
 `(:output-translations (t (,(merge-pathnames "../build/http-cache/"
                                            (uiop:pathname-directory-pathname *load-truename*))
                            :implementation :**/ :*.*.*))
                        :ignore-inherited-configuration))
(let ((dependencies (merge-pathnames "../build/http-test-deps/"
                                     (uiop:pathname-directory-pathname *load-truename*))))
  (if (probe-file dependencies)
      (asdf:initialize-source-registry `(:source-registry (:tree ,dependencies) :inherit-configuration))
      (let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
        (when (probe-file setup) (load setup)))))
(asdf:load-asd (merge-pathnames "../xeon.asd" *load-truename*))
(asdf:load-system "xeon/http")
(let ((library (uiop:getenv "XEON_LIBCURL")))
  (when (and library (plusp (length library)))
    (setf xeon.http:*library-path* library)))
(load (merge-pathnames "http.lisp" *load-truename*))
(assert (not (find-package "WXLAMBDA")))
