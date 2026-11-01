#lang racket/base
;;; docs.rkt — the signatures in the installed Racket documentation, for
;;; Cadre's Racket docset
;;;
;;; Copyright (c) 2026 Jessie Hughart. MIT License (see LICENSE beside this file).
;;;
;;; `racket docs.rkt` prints one line per documented export, as Common Lisp
;;; data: (name (library ...) kind (signature-line ...) url). Kind is what
;;; the docs call it ("procedure", "syntax", "value", "parameter", ...), and
;;; the signature lines are the docs' blue boxes: the call and its contracts.
;;;
;;; `racket docs.rkt project ROOT FILE ...` prints the same for what a
;;; package's modules provide, read from their source without running it:
;;; (name (module) kind (header [contract]) #f description), the header being
;;; the definition's, the contract contract-out's, and the description the
;;; ; comment lines just above the definition.
;;; Nothing here is copied from the documentation into Cadre: Cadre makes the
;;; docset on the user's machine, from the Racket they have installed.

(require setup/xref
         scribble/xref
         scribble/manual-struct
         scribble/blueboxes
         racket/string
         racket/list
         racket/file
         racket/path
         syntax/modread
         "cl-data.rkt")

;;; A package's own modules, read without running them

(define (read-module path)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (parameterize ([current-load-relative-directory (path-only path)])
      (with-module-reading-parameterization
        (λ () (call-with-input-file path (λ (in) (port-count-lines! in) (read-syntax path in))))))))

(define (body-forms stx)
  ;; (module name lang form ...), or with #%module-begin around the forms.
  (define parts (syntax->list stx))
  (define forms (if (and parts (>= (length parts) 3)) (cdddr parts) '()))
  (if (and (= (length forms) 1)
           (pair? (syntax-e (car forms)))
           (eq? (syntax-e (car (syntax-e (car forms)))) '#%module-begin))
      (cdr (syntax->list (car forms)))
      forms))

(define (head stx) (let ([e (syntax-e stx)]) (and (pair? e) (syntax-e (car e)))))

(define (comment-above lines line)
  ;; The ; comment lines just above LINE (from 1), without their semicolons.
  (let loop ([i (- line 2)] [found '()])
    (if (and (>= i 0) (regexp-match? #px"^\\s*;" (list-ref lines i)))
        (loop (sub1 i) (cons (string-trim (regexp-replace #px"^\\s*;+\\s?" (list-ref lines i) "")) found))
        (string-join found "\n"))))

(define (definitions forms lines)
  ;; name → (kind header description) for the module's definitions.
  (define table (make-hash))
  (for ([f forms])
    (define d (syntax->datum f))
    (define (add! name kind header)
      (when (symbol? name)
        (hash-set! table name (list kind (format "~s" header)
                                    (if (syntax-line f) (comment-above lines (syntax-line f)) "")))))
    (when (pair? d)
      (case (car d)
        [(define define/contract)
         (define target (and (pair? (cdr d)) (cadr d)))
         (cond [(pair? target)
                (let loop ([t target]) (if (pair? (car t)) (loop (car t)) (add! (car t) "procedure" target)))]
               [else (add! target "value" target)])]
        [(define-syntax-rule)
         (when (and (pair? (cdr d)) (pair? (cadr d))) (add! (car (cadr d)) "syntax" (cadr d)))]
        [(define-syntax)
         (define target (and (pair? (cdr d)) (cadr d)))
         (add! (if (pair? target) (car target) target) "syntax" (if (pair? target) target (list target '...)))]
        [(struct define-struct)
         (when (pair? (cdr d))
           (define name (if (pair? (cadr d)) (car (cadr d)) (cadr d)))
           (add! name "struct" (list* 'struct name (if (pair? (cddr d)) (list (caddr d)) '()))))])))
  table)

(define (provided forms table)
  ;; (name . contract-or-#f) for what the module provides.
  (append*
   (for/list ([f forms] #:when (eq? (head f) 'provide))
     (append*
      (for/list ([spec (cdr (syntax->datum f))])
        (cond [(symbol? spec) (list (cons spec #f))]
              [(and (pair? spec) (eq? (car spec) 'struct-out) (pair? (cdr spec))) (list (cons (cadr spec) #f))]
              [(and (pair? spec) (eq? (car spec) 'all-defined-out))
               (for/list ([name (hash-keys table)]) (cons name #f))]
              [(and (pair? spec) (eq? (car spec) 'contract-out))
               (for/list ([c (cdr spec)] #:when (and (pair? c) (symbol? (car c))))
                 (cons (car c) (and (pair? (cdr c)) (format "~s" (cadr c)))))]
              [else '()]))))))

(define (project-docs root files)
  (define out (current-output-port))
  (for ([file files])
    (define stx (read-module file))
    (when stx
      (define forms (body-forms stx))
      (define lines (file->lines file))
      (define table (definitions forms lines))
      (define module (path->string (find-relative-path (simple-form-path root) (simple-form-path file))))
      (for ([entry (remove-duplicates (provided forms table) #:key car)])
        (define name (car entry))
        (define def (hash-ref table name #f))
        (write-cl (list (symbol->string name)
                        (list module)
                        (if def (first def) "")
                        (append (if def (list (second def)) '())
                                (if (cdr entry) (list (format "contract: ~a" (cdr entry))) '()))
                        #f
                        (if def (third def) ""))
                  out)
        (newline out)))))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (when (and (pair? args) (equal? (car args) "project"))
    (project-docs (cadr args) (cddr args))
    (exit 0))
  (define xref (load-collections-xref))
  (define cache (make-blueboxes-cache #t))
  (define out (current-output-port))
  (for ([e (xref-index xref)]
        #:when (exported-index-desc? (entry-desc e)))
    (define desc (entry-desc e))
    (define tag (entry-tag e))
    (define lines
      (map (λ (l) (string-replace l "\u00A0" " "))
           (or (with-handlers ([exn:fail? (λ (_) #f)])
                 (fetch-blueboxes-strs tag #:blueboxes-cache cache))
               '())))
    (define url
      (with-handlers ([exn:fail? (λ (_) #f)])
        (define-values (path anchor) (xref-tag->path+anchor xref tag))
        (and path (format "file://~a~a" (path->string path) (if anchor (format "#~a" anchor) "")))))
    (write-cl (list (format "~a" (exported-index-desc-name desc))
                    (for/list ([lib (exported-index-desc-from-libs desc)]) (format "~s" lib))
                    (if (pair? lines) (car lines) "")
                    (if (pair? lines) (cdr lines) '())
                    url)
              out)
    (newline out)))
