#lang racket/base
;;; pkgs.rkt — which of a package's dependencies aren't installed
;;;
;;; Copyright (c) 2026 Jessie Hughart. MIT License (see LICENSE beside this file).
;;;
;;; `racket pkgs.rkt missing DEP ...` prints each DEP (a package name or
;;; source, as info.rkt's deps give them) that no installed package
;;; provides, one per line, for Cadre's Project page.

(require pkg/lib pkg/name)

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (when (and (pair? args) (equal? (car args) "missing"))
    (for ([dep (cdr args)])
      (define name (or (package-source->name dep) dep))
      (unless (pkg-directory name)
        (displayln dep)))))
