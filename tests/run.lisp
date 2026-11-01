(load (merge-pathnames "../tools/bootstrap.lisp" *load-truename*))
(asdf:test-system "xeon")
(dolist (name '("WXLAMBDA" "XEON.UI" "LUT" "LGT" "RAYLIB"))
  (assert (not (find-package name))))
