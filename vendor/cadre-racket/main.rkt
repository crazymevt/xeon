#lang racket/base
;;; cadre-racket — Cadre's back end for Racket
;;;
;;; Copyright (c) 2026 Jessie Hughart. MIT License (see LICENSE beside this file).
;;;
;;; Started as `racket main.rkt --port-file FILE`, it listens on a free port
;;; on 127.0.0.1, writes the port to FILE and serves one connection from
;;; Cadre. It exits when that connection closes, or when its standard input
;;; does (Cadre quit).
;;;
;;; Messages both ways are framed as Swank's are: six hex digits giving the
;;; length in UTF-8 octets, then an s-expression. Cadre sends
;;;   (:emacs-rex (:op arg ...) package thread id)   a request
;;;   (:emacs-interrupt thread)                       break running evaluations
;;; and gets back
;;;   (:return (:ok value) id) or (:return (:abort reason) id)
;;;   (:write-string text target)    output (target nil) or a REPL value (:repl-result)
;;;   (:new-package name prompt)     the REPL moved to another module
;;; Values are written so Common Lisp's reader reads them: #t is t, #f and
;;; '() are nil, and strings escape only " and \.
;;;
;;; A "package" is the path of a module file, whose namespace code is
;;; evaluated in (the module is run first if it hasn't been), "racket" for
;;; the plain namespace the REPL starts in, or nil for the REPL's current
;;; one. Running a file makes the REPL enter its module, as racket-mode's
;;; and DrRacket's Run do.

(require racket/list
         racket/string
         racket/port
         racket/tcp
         racket/lazy-require
         syntax/modread
         "cl-data.rkt")

(lazy-require [racket/pretty (pretty-format)]
              [drracket/check-syntax (show-content)]
              [setup/xref (load-collections-xref)]
              [scribble/xref (xref-binding->definition-tag xref-tag->path+anchor)]
              [scribble/blueboxes (fetch-blueboxes-strs)])

;;; Framing

(define connection-out #f)
(define (set-connection-out! out) (set! connection-out out))
(define write-lock (make-semaphore 1))

(define (send message)
  (define text (let ([o (open-output-string)]) (write-cl message o) (get-output-string o)))
  (define octets (string->bytes/utf-8 text))
  (call-with-semaphore
   write-lock
   (λ ()
     (write-bytes (string->bytes/utf-8 (string-pad-hex (bytes-length octets))) connection-out)
     (write-bytes octets connection-out)
     (flush-output connection-out))))

(define (string-pad-hex n)
  (define s (number->string n 16))
  (string-append (make-string (max 0 (- 6 (string-length s))) #\0) s))

(define (read-message in)
  (define header (read-bytes 6 in))
  (cond [(or (eof-object? header) (< (bytes-length header) 6)) eof]
        [else
         (define n (string->number (bytes->string/utf-8 header) 16))
         (define body (read-bytes n in))
         (if (or (eof-object? body) (< (bytes-length body) n))
             eof
             (parameterize ([read-accept-reader #f] [read-accept-lang #f] [read-accept-compiled #f])
               (read (open-input-string (bytes->string/utf-8 body #\?)))))]))

;;; Output: each request's output goes to Cadre as it is written.

(define (utf-8-complete-length b)
  ;; How many of B's bytes end on a whole UTF-8 character.
  (define n (bytes-length b))
  (let loop ([i (sub1 n)] [back 0])
    (cond [(or (< i 0) (= back 4)) n]
          [else
           (define c (bytes-ref b i))
           (cond [(< c #x80) n]
                 [(= (bitwise-and c #xC0) #x80) (loop (sub1 i) (add1 back))]
                 [else (define need (cond [(>= c #xF0) 4] [(>= c #xE0) 3] [else 2]))
                       (if (> need (- n i)) i n)])])))

(define (remote-output-port)
  ;; Whole lines go to Cadre as they are written; the rest when the port is
  ;; flushed (at the end of each request), so a value and its newline,
  ;; written separately, arrive together.
  (define pending #"")
  (define (send-text! bs)
    (when (> (bytes-length bs) 0)
      (send `(:write-string ,(bytes->string/utf-8 bs #\?) nil))))
  (make-output-port
   'cadre
   always-evt
   (λ (bs start end non-block? breakable?)
     (cond
       [(= start end)                   ; a flush
        (define cut (utf-8-complete-length pending))
        (send-text! (subbytes pending 0 cut))
        (set! pending (subbytes pending cut))]
       [else
        (set! pending (bytes-append pending (subbytes bs start end)))
        (define newline (let loop ([i (sub1 (bytes-length pending))])
                          (cond [(< i 0) #f]
                                [(= (bytes-ref pending i) 10) i]
                                [else (loop (sub1 i))])))
        (when newline
          (send-text! (subbytes pending 0 (add1 newline)))
          (set! pending (subbytes pending (add1 newline))))])
     (- end start))
   void))

;;; Namespaces

(define base-namespace (make-base-namespace))
(parameterize ([current-namespace base-namespace])
  (namespace-require 'racket))

(define modules (make-hash))           ; path string → its module's namespace
(define repl-namespace base-namespace)
(define repl-package "racket")

(define (package-prompt package)
  (if (equal? package "racket")
      "racket"
      (let-values ([(dir name must-be-dir?) (split-path package)]) (path->string name))))

(define (file-directory path)
  (let-values ([(dir name must-be-dir?) (split-path path)]) dir))

(define (module-spec path) `(file ,path))

(define (run-module path)
  ;; Run the module in the file at PATH, from its source, in a fresh
  ;; namespace (its main submodule too, as `racket` does), and return the
  ;; module's namespace. Its definitions can be redefined afterwards.
  (define target (simplify-path (path->complete-path path)))
  (define ns (make-base-empty-namespace))
  (define load/use-compiled (current-load/use-compiled))
  (parameterize ([current-namespace ns]
                 [compile-enforce-module-constants #f]
                 [current-load-relative-directory (file-directory target)]
                 [current-command-line-arguments (vector)]
                 ;; The file itself from source, so its namespace has every
                 ;; definition; the libraries it uses from their .zo files.
                 [current-load/use-compiled
                  (λ (p name)
                    (if (equal? (simplify-path (path->complete-path p)) target)
                        ((current-load) p name)
                        (load/use-compiled p name)))])
    (namespace-attach-module base-namespace 'racket ns)
    (dynamic-require (module-spec path) #f)
    (define main `(submod ,(module-spec path) main))
    (when (module-declared? main #t)
      (dynamic-require main #f))
    (define module-ns (module->namespace (module-spec path)))
    (hash-set! modules path module-ns)
    module-ns))

(define (package-namespace package)
  (cond [(or (not package) (eq? package 'nil)) repl-namespace]
        [(equal? package "racket") base-namespace]
        [(hash-ref modules package #f)]
        [(file-exists? package)
         (send `(:write-string ,(format "; Running ~a\n" (package-prompt package)) nil))
         (run-module package)]
        [else base-namespace]))

(define (enter-package! package ns)
  (set! repl-namespace ns)
  (set! repl-package package)
  (send `(:new-package ,package ,(package-prompt package))))

;;; Reading code

(define (read-all-syntax text source #:line [line #f] #:column [column #f] #:position [position #f])
  (define in (open-input-string text))
  (port-count-lines! in)
  (when line (set-port-next-location! in line (or column 0) (or position 1)))
  (parameterize ([read-accept-reader #t] [read-accept-lang #f])
    (let loop ([forms '()])
      (define stx (read-syntax source in))
      (if (eof-object? stx) (reverse forms) (loop (cons stx forms))))))

(define (eval-in ns stx #:interaction? [interaction? #f])
  (parameterize ([current-namespace ns])
    (call-with-values
     (λ () (eval (namespace-syntax-introduce
                  (if interaction? (datum->syntax #f (cons '#%top-interaction stx) stx) stx))))
     list)))

(define (value->string v)
  (parameterize ([print-as-expression #t])
    (let ([o (open-output-string)]) (print v o) (get-output-string o))))

(define result-objects (make-hash))
(define result-counter 0)
(define inspected #f)
(define inspection-history '())
(define inspection-parts '())
(define (remember-result v)
  (set! result-counter (add1 result-counter))
  (hash-set! result-objects result-counter v)
  (hash-remove! result-objects (- result-counter 128))
  result-counter)
(define (inspection-view)
  (set! inspection-parts
    (cond [(pair? inspected) (list (car inspected) (cdr inspected))]
          [(vector? inspected) (take (vector->list inspected) (min 64 (vector-length inspected)))]
          [(hash? inspected) (take (hash->list inspected) (min 64 (hash-count inspected)))]
          [else '()]))
  (define parts
    (for/list ([v inspection-parts] [i (in-naturals)])
      `(:value ,(value->string v) ,i)))
  `(:title ,(value->string inspected) :content (,parts ,(length parts) 0 ,(length parts))))
(define (inspect-value v [push? #t])
  (when push? (set! inspection-history (take (cons inspected inspection-history) (min 64 (add1 (length inspection-history))))))
  (set! inspected v)
  (inspection-view))

(define (printed-values vs)
  (map value->string (filter (λ (v) (not (void? v))) vs)))

;;; Errors as (:error message file line column), for Cadre's notes

(define (error-place e)
  (define locs (if (exn:srclocs? e) ((exn:srclocs-accessor e) e) '()))
  (define loc (findf srcloc-line locs))
  (if loc
      (list (let ([s (srcloc-source loc)]) (if (path? s) (path->string s) (and (string? s) s)))
            (srcloc-line loc)
            (or (srcloc-column loc) 0))
      (list #f #f #f)))

(define (exn-text e)
  (if (exn? e) (exn-message e) (format "~a" e)))

(define (error-result e)
  (cons ':error (cons (exn-text e) (error-place e))))

;; A run-time error's stack goes to Cadre's Debugger page as
;; (:post-mortem message type frames), frames being (name file line column)
;; from the exception's context (errortrace isn't used: it slows everything).
;; Syntax and read errors are notes in Problems instead.

(define this-file (variable-reference->module-source (#%variable-reference)))

(define (exn-type-name e)
  (define name (format "~a" (vector-ref (struct->vector e) 0)))
  (if (string-prefix? name "struct:") (substring name 7) name))

(define (exn-frames e)
  ;; Frames with a source or a procedure's name (not "body of …" alone, which
  ;; is the module running), not this back end's, repeats collapsed.
  (define frames
    (for/list ([c (continuation-mark-set->context (exn-continuation-marks e))]
               #:unless (let ([loc (cdr c)]) (and loc (equal? (srcloc-source loc) this-file)))
               #:when (or (cdr c) (and (car c) (not (regexp-match? #rx"^body of " (format "~a" (car c)))))))
      (define loc (cdr c))
      (define source (and loc (srcloc-source loc)))
      (list (format "~a" (or (car c) "(anonymous)"))
            (cond [(path? source) (path->string source)] [(string? source) source] [else #f])
            (and loc (srcloc-line loc))
            (and loc (srcloc-column loc)))))
  (define collapsed
    (for/list ([f frames] [i (in-naturals)]
               #:unless (and (> i 0) (equal? f (list-ref frames (sub1 i)))))
      f))
  (if (> (length collapsed) 80) (take collapsed 80) collapsed))

(define (report-error! e)
  (unless (or (exn:fail:syntax? e) (exn:fail:read? e) (not (exn? e)))
    (send `(:post-mortem ,(exn-text e) ,(exn-type-name e) ,(exn-frames e)))))

;;; Identifiers, documentation and definitions

;;; Requests run on threads of their own, and the xref and blueboxes aren't
;;; safe to share: a lookup made while another is loading the index fails.
;;; So one lookup at a time, and the index starts loading at startup (on a
;;; slow machine it takes a while).
(define docs-lock (make-semaphore 1))
(define (with-docs thunk) (call-with-semaphore docs-lock thunk))

(define the-xref #f)
(define (xref)
  (unless the-xref (set! the-xref (load-collections-xref)))
  the-xref)

(define (warm-docs!)
  (void (thread (λ () (with-handlers ([(λ (e) #t) void]) (with-docs xref))))))

(define (identifier-in ns name)
  (parameterize ([current-namespace ns])
    (namespace-symbol->identifier (string->symbol name))))

(define (bound? ns name)
  (parameterize ([current-namespace ns])
    (and (identifier-binding (identifier-in ns name)) #t)))

(define (doc-tag ns name)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (define id (identifier-in ns name))
    (with-docs (λ () (xref-binding->definition-tag (xref) id 0)))))

(define (tag-url tag)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (define-values (path anchor) (with-docs (λ () (xref-tag->path+anchor (xref) tag))))
    (and path (format "file://~a~a" (path->string path) (if anchor (format "#~a" anchor) "")))))

(define (doc-signature-lines tag)
  (with-handlers ([exn:fail? (λ (e) #f)])
    ;; The docs space signatures with non-breaking spaces.
    (let ([lines (with-docs (λ () (fetch-blueboxes-strs tag)))])
      (and lines (map (λ (l) (string-replace l "\u00A0" " ")) lines)))))

(define (binding-source ns name)
  ;; The file NAME's binding in NS comes from and its name there, or #f.
  (with-handlers ([exn:fail? (λ (e) #f)])
    (define b (identifier-binding (identifier-in ns name)))
    (and (list? b)
         (let* ([resolved (resolved-module-path-name (module-path-index-resolve (first b)))]
                [path (if (pair? resolved) (cadr resolved) resolved)])
           (and (path? path) (cons (path->string path) (second b)))))))

(define definers
  '(define define-values define-syntax define-syntaxes define-syntax-rule define-struct struct
    define/contract define-for-syntax define-logger define-generics define-match-expander
    define-simple-macro define-syntax-parser define-type define-runtime-path))

(define (definition-names datum)
  ;; The names a definition DATUM introduces.
  (define (head-name x) (cond [(symbol? x) x] [(pair? x) (head-name (car x))] [else #f]))
  (cond [(and (pair? datum) (pair? (cdr datum)) (memq (car datum) definers))
         (define target (cadr datum))
         (cond [(memq (car datum) '(define-values define-syntaxes))
                (if (list? target) (filter symbol? target) '())]
               [(memq (car datum) '(struct define-struct))
                (list (head-name target))]
               [else (list (head-name target))])]
        [else '()]))

(define (read-module-syntax path)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (parameterize ([current-load-relative-directory (file-directory path)])
      (with-module-reading-parameterization
        (λ () (call-with-input-file path
                (λ (in) (port-count-lines! in) (read-syntax path in))))))))

(define (find-definition stx sym)
  ;; The syntax of the definition of SYM in STX, searching depth first.
  (let loop ([stx stx])
    (define d (syntax-e stx))
    (cond [(and (pair? d) (memq sym (definition-names (syntax->datum stx)))) stx]
          [(list? d) (for/or ([s d]) (loop s))]
          [(pair? d) (let ([l (syntax->list stx)]) (and l (for/or ([s l]) (loop s))))]
          [else #f])))

(define (definition-in-file path sym)
  ;; Where SYM is defined in the file at PATH: (line column header), or #f.
  (define stx (read-module-syntax path))
  (define def (and stx (find-definition stx sym)))
  (and def
       (let* ([d (syntax->datum def)]
              [header (cadr d)])
         (list (syntax-line def) (syntax-column def)
               (and (pair? header) (memq (car d) '(define define/contract)) header)))))

(define (arity-arglist name arity)
  (define (args n) (for/list ([i n]) (string->symbol (format "a~a" (add1 i)))))
  (cond [(exact-nonnegative-integer? arity) (cons name (args arity))]
        [(arity-at-least? arity) (append (cons name (args (arity-at-least-value arity))) '(...))]
        [(and (list? arity) (pair? arity)) (arity-arglist name (last arity))]
        [else #f]))

(define (value-of ns name)
  (with-handlers ([exn:fail? (λ (e) (void))])
    (parameterize ([current-namespace ns])
      (namespace-variable-value (string->symbol name) #t))))

(define (arglist ns name)
  ;; NAME's arglist as text, such as "(map proc lst ...+)", or #f.
  (define tag (doc-tag ns name))
  (define lines (and tag (doc-signature-lines tag)))
  (define documented (and lines (findf (λ (l) (string-prefix? l "(")) lines)))
  (cond [documented (string-trim (regexp-replace #px"\\s+->\\s.*$" documented ""))]
        [else
         (define source (binding-source ns name))
         (define found (and source (definition-in-file (car source) (cdr source))))
         (cond [(and found (third found))
                (format "~s" (cons (string->symbol name) (cdr (flatten-header (third found)))))]
               [else
                (define v (value-of ns name))
                (and (procedure? v)
                     (let ([a (arity-arglist (string->symbol name) (procedure-arity v))])
                       (and a (format "~s" a))))])]))

(define (flatten-header header)
  ;; (define ((f a) b) …) → (f a b)
  (if (pair? (car header))
      (let ([inner (flatten-header (car header))]) (append inner (cdr header)))
      header))

(define (describe ns name)
  (cond
    [(not (bound? ns name)) (format "~a is not defined here." name)]
    [else
     (define tag (doc-tag ns name))
     (define lines (and tag (doc-signature-lines tag)))
     (define source (binding-source ns name))
     (define v (value-of ns name))
     (string-join
      (filter values
              (list (format "~a: ~a~a" name
                            (cond [(and lines (pair? lines)) (car lines)]
                                  [(procedure? v) "procedure"]
                                  [(void? v) "syntax"]
                                  [else "value"])
                            (if source (format " from ~a" (car source)) ""))
                    (and lines (pair? lines) (string-join (cdr lines) "\n"))
                    (and (not lines) (arglist ns name))
                    (and (not lines) (not (procedure? v)) (not (void? v))
                         (format "Value: ~a" (value->string v)))
                    (let ([url (and tag (tag-url tag))]) (and url (format "Documentation: ~a" url)))))
      "\n\n")]))

(define (completions ns prefix)
  (define names
    (parameterize ([current-namespace ns])
      (for*/list ([s (namespace-mapped-symbols ns)]
                  [n (in-value (symbol->string s))]
                  #:when (string-prefix? n prefix))
        n)))
  (for/list ([n (take (sort (remove-duplicates names) string<?) (min 300 (length names)))])
    (define v (value-of ns n))
    (list n (cond [(procedure? v) ':function] [(void? v) ':macro] [else ':variable]))))

;;; Check-syntax on a buffer's text, which needn't be saved or run

(define (check-syntax path text)
  ;; What drracket/check-syntax finds in TEXT, the source of the module in the
  ;; file at PATH: a hash with 'arrows ((def-start def-end use-start use-end) …,
  ;; only in this file), 'jumps ((start end id file) …, imported names),
  ;; 'docs ((start end url) …), 'unused ((start end) …, unused requires) and
  ;; 'error (an error-result) or #f.
  (define arrows '()) (define jumps '()) (define docs '()) (define unused '()) (define targets '())
  (define err #f)
  (with-handlers ([(λ (e) #t) (λ (e) (set! err (error-result e)))])
    (define p (string->path path))
    (define stx
      (parameterize ([current-load-relative-directory (file-directory p)])
        (with-module-reading-parameterization
          (λ () (let ([in (open-input-string text)]) (port-count-lines! in) (read-syntax p in))))))
    (parameterize ([current-load-relative-directory (file-directory p)]
                   [current-namespace (make-base-namespace)])
      (for ([v (show-content stx)])
        (case (vector-ref v 0)
          [(syncheck:add-arrow/name-dup/pxpy)
           (unless (vector-ref v 11)       ; not from a require
             (set! arrows (cons (list (vector-ref v 1) (vector-ref v 2) (vector-ref v 5) (vector-ref v 6)) arrows)))]
          [(syncheck:add-definition-target/phase-level+space)
           (set! targets (cons (list (vector-ref v 1) (vector-ref v 2) (vector-ref v 3)) targets))]
          [(syncheck:add-jump-to-definition/phase-level+space)
           (set! jumps (cons (list (vector-ref v 1) (vector-ref v 2) (vector-ref v 3) (vector-ref v 4)) jumps))]
          [(syncheck:add-docs-menu)
           (set! docs (cons (list (vector-ref v 1) (vector-ref v 2)
                                  (format "file://~a#~a" (path->string (vector-ref v 5)) (vector-ref v 7)))
                            docs))]
          [(syncheck:add-unused-require)
           (set! unused (cons (list (vector-ref v 1) (vector-ref v 2)) unused))]
          [else (void)]))))
  (hash 'arrows arrows 'jumps jumps 'docs docs 'unused unused 'targets targets 'error err))

(define (covers? span offset)
  (and (<= (first span) offset) (<= offset (second span))))

(define (offset-line-column text offset)
  ;; TEXT's line (from 1) and column (from 0) at OFFSET.
  (for/fold ([line 1] [column 0]) ([c (in-string text 0 (min offset (string-length text)))])
    (if (char=? c #\newline) (values (add1 line) 0) (values line (add1 column)))))

(define (line-text text offset)
  (define start (let loop ([i (min offset (string-length text))])
                  (if (and (> i 0) (not (char=? (string-ref text (sub1 i)) #\newline))) (loop (sub1 i)) i)))
  (define end (let loop ([i start])
                (if (and (< i (string-length text)) (not (char=? (string-ref text i) #\newline))) (loop (add1 i)) i)))
  (string-trim (substring text start end)))

(define (binder-at found offset)
  ;; The (start end) of the binding the identifier at OFFSET refers to, in this file, or #f.
  (define arrows (hash-ref found 'arrows))
  (cond [(findf (λ (a) (covers? (list (third a) (fourth a)) offset)) arrows)
         => (λ (a) (list (first a) (second a)))]
        [(findf (λ (a) (covers? (list (first a) (second a)) offset)) arrows)
         => (λ (a) (list (first a) (second a)))]
        [(findf (λ (t) (covers? t offset)) (hash-ref found 'targets))
         => (λ (t) (list (first t) (second t)))]
        [else #f]))

(define (location-at path text offset)
  (define-values (line column) (offset-line-column text offset))
  (list path line column))

(define (find-definitions ns name path text offset)
  ;; Where NAME (at OFFSET in TEXT, the source of PATH, when they are given) is
  ;; defined: a list of (label file line column), or (label :error message).
  (define found (and path text offset (check-syntax path text)))
  (define binder (and found (binder-at found offset)))
  (define jump (and found (findf (λ (j) (covers? j offset)) (hash-ref found 'jumps))))
  (cond
    [binder (list (cons (format "~a" name) (location-at path text (first binder))))]
    [jump
     (define file (path->string (fourth jump)))
     (define place (definition-in-file file (third jump)))
     (list (list (format "~a" (third jump)) file (if place (first place) 1) (if place (second place) 0)))]
    [else
     (define source (binding-source ns name))
     (define place (and source (definition-in-file (car source) (cdr source))))
     (cond [place (list (list name (car source) (first place) (second place)))]
           [source (list (list name (car source) 1 0))]
           [else '()])]))

(define (references path text offset)
  ;; The uses in TEXT of the binding at OFFSET: ((line-text path line column) …).
  (define found (check-syntax path text))
  (define binder (binder-at found offset))
  (if (not binder)
      '()
      (for/list ([a (sort (hash-ref found 'arrows) < #:key third)]
                 #:when (and (= (first a) (first binder)) (= (second a) (second binder))))
        (cons (line-text text (third a)) (location-at path text (third a))))))

(define (binding-spans path text offset)
  ;; For renaming: the (start end) of the binding the identifier at OFFSET
  ;; refers to and of each use of it in TEXT, last first; or an error if it
  ;; is imported (it's defined, and renamed, elsewhere).
  (define found (check-syntax path text))
  (define binder (binder-at found offset))
  (cond
    [binder
     (sort (remove-duplicates
            (cons binder
                  (for/list ([a (hash-ref found 'arrows)]
                             #:when (and (= (first a) (first binder)) (= (second a) (second binder))))
                    (list (third a) (fourth a)))))
           > #:key first)]
    [(findf (λ (j) (covers? j offset)) (hash-ref found 'jumps))
     => (λ (j) (error 'rename "~a is imported from ~a: rename it there" (third j) (fourth j)))]
    [(hash-ref found 'error)
     => (λ (e) (error 'rename "the file doesn't expand: ~a" (second e)))]
    [else (error 'rename "that isn't defined in this file (it's imported, or a primitive)")]))

(define (documentation-url ns name path text offset)
  (define found (and path text offset (check-syntax path text)))
  (define doc (and found (findf (λ (d) (covers? d offset)) (hash-ref found 'docs))))
  (cond [doc (third doc)]
        [else (let ([tag (doc-tag ns name)]) (and tag (tag-url tag)))]))

(define (problems path text)
  ;; Errors and unused requires in TEXT, as ((severity message line column) …).
  (define found (check-syntax path text))
  (append
   (let ([e (hash-ref found 'error)])
     (if e
         (list (list ':error (second e) (or (fourth e) 1) (or (fifth e) 0)))
         '()))
   (for/list ([u (hash-ref found 'unused)])
     (define-values (line column) (offset-line-column text (first u)))
     (list ':warning (format "Unused require: ~a" (substring text (first u) (min (second u) (string-length text))))
           line column))))

;;; Macros

(define (macroexpand ns code all?)
  (define stx (car (read-all-syntax code 'macroexpand)))
  (parameterize ([current-namespace ns])
    (define expanded ((if all? expand expand-once) (namespace-syntax-introduce stx)))
    (pretty-format (syntax->datum expanded) 78 #:mode 'write)))

;;; Requests

(define running (make-hash))            ; threads evaluating code → #t

;; Input: code that reads from its standard input asks Cadre for a line,
;; (:read-string t tag), and waits for (:emacs-return-string t tag text).
(define input-waiting (make-hash))      ; tag → a function taking the text
(define next-input-tag 0)

(define (asking-input-port)
  (define-values (in out) (make-pipe))
  (define asked? #f)
  (make-input-port
   'cadre-input
   (λ (bs)
     (define n (read-bytes-avail!* bs in))
     (cond [(and (number? n) (> n 0)) n]
           [else
            (unless asked?
              (set! asked? #t)
              (set! next-input-tag (add1 next-input-tag))
              (define tag next-input-tag)
              (hash-set! input-waiting tag (λ (text) (set! asked? #f) (write-string text out)))
              (send `(:read-string t ,tag)))
            (wrap-evt in (λ (_) 0))]))
   #f
   void))

(define (call-evaluating thunk)
  ;; Call THUNK with output going to Cadre, and input asked of it, so it can be interrupted.
  (define out (remote-output-port))
  (hash-set! running (current-thread) #t)
  (dynamic-wind
   void
   (λ ()
     (parameterize ([current-output-port out]
                    [current-error-port out]
                    [current-input-port (asking-input-port)])
       (thunk)))
   (λ ()
     (flush-output out)
     (hash-remove! running (current-thread)))))

(define (eval-text ns text source)
  ;; The printed values of the last form.
  (for/fold ([printed '()]) ([stx (read-all-syntax text source)])
    (printed-values (eval-in ns stx))))

(define (nil->false v) (if (eq? v 'nil) #f v))

(define pending-source #f)

(define (handle op args package)
  (define (ns) (package-namespace (nil->false package)))
  (case op
    [(:connection-info)
     `(:implementation ,(format "Racket ~a" (version)) :package ,repl-package :prompt ,(package-prompt repl-package) :inspector #t)]
    [(:inspect-result) (inspect-value (hash-ref result-objects (first args)))]
    [(:inspect-expression) (inspect-value (car (eval-in repl-namespace (car (read-all-syntax (first args) 'inspect)))))]
    [(:inspect-part) (inspect-value (list-ref inspection-parts (first args)))]
    [(:inspect-back)
     (unless (null? inspection-history)
       (set! inspected (car inspection-history))
       (set! inspection-history (cdr inspection-history)))
     (inspection-view)]
    [(:inspect-refresh) (inspection-view)]
    [(:eval)
     (call-evaluating
      (λ ()
        (with-handlers ([exn:fail? (λ (e) (report-error! e) (raise e))])
          (eval-text (ns) (first args) 'eval))))]
    [(:begin-source) (set! pending-source (make-string (first args))) #t]
    [(:put-source) (string-copy! pending-source (first args) (second args)) #t]
    [(:eval-source)
     (define source pending-source)
     (set! pending-source #f)
     (handle ':repl-eval (list source) package)]
    [(:repl-eval)
     (call-evaluating
      (λ ()
        (with-handlers ([exn:fail? (λ (e)
                                     (send `(:write-string ,(format "~a\n" (exn-text e)) nil))
                                     (report-error! e))]
                        [exn:break? (λ (e) (send `(:write-string "; Interrupted\n" nil)))])
          (for ([stx (read-all-syntax (first args) 'repl)])
            (for ([v (eval-in repl-namespace stx #:interaction? #t)] #:unless (void? v))
              (define id (remember-result v))
              (send `(:presentation-start ,id))
              (send `(:write-string ,(format "~a\n" (value->string v)) :repl-result))
              (send `(:presentation-end ,id)))))
        #t))]
    [(:compile-form)
     ;; (:compile-form code file line column position)
     (define-values (code file line column position) (apply values (map nil->false args)))
     (call-evaluating
      (λ ()
        (with-handlers ([exn:fail? error-result])
          (define target (ns))
          (for ([stx (read-all-syntax code (if file (string->path file) 'buffer)
                                      #:line line #:column column #:position position)])
            (eval-in target stx))
          '(:ok))))]
    [(:run-file)
     (define path (first args))
     (call-evaluating
      (λ ()
        (with-handlers ([exn:fail? (λ (e) (report-error! e) (error-result e))])
          (enter-package! path (run-module path))
          '(:ok))))]
    [(:set-package)
     (define name (first args))
     (call-evaluating
      (λ ()
        (enter-package! name (package-namespace (if (equal? name "") "racket" name)))
        (list repl-package (package-prompt repl-package))))]
    [(:describe) (describe (ns) (first args))]
    [(:arglist) (arglist (ns) (first args))]
    [(:completions) (completions (ns) (first args))]
    [(:find-definitions)
     (apply find-definitions (ns) (map nil->false args))]
    [(:references) (apply references args)]
    [(:binding-spans) (apply binding-spans args)]
    [(:documentation) (apply documentation-url (ns) (map nil->false args))]
    [(:problems) (apply problems args)]
    [(:macroexpand) (macroexpand (ns) (first args) (eq? (second args) 't))]
    [else (error 'cadre-racket "unknown request ~a" op)]))

(define (serve in)
  (let loop ()
    (define message (read-message in))
    (unless (eof-object? message)
      (when (pair? message)
        (case (car message)
          [(:emacs-rex)
           (define form (second message))
           (define package (third message))
           (define id (fifth message))
           (thread
            (λ ()
              (define reply
                (with-handlers ([exn:break? (λ (e) `(:abort "Interrupted"))]
                                [(λ (e) #t) (λ (e) `(:abort ,(exn-text e)))])
                  `(:ok ,(handle (car form) (cdr form) package))))
              (send `(:return ,reply ,id))))]
          [(:emacs-interrupt)
           (for ([t (hash-keys running)]) (break-thread t))]
          [(:emacs-return-string)
           (define give (hash-ref input-waiting (third message) #f))
           (when give
             (hash-remove! input-waiting (third message))
             (give (fourth message)))]
          [else (void)]))
      (loop))))

(module+ main
  (define port-file
    (let ([args (vector->list (current-command-line-arguments))])
      (cond [(member "--port-file" args) => cadr]
            [else (error 'cadre-racket "usage: racket main.rkt --port-file FILE")])))
  (define listener (tcp-listen 0 4 #t "127.0.0.1"))
  (define-values (host port remote-host remote-port) (tcp-addresses listener #t))
  (call-with-output-file port-file #:exists 'truncate (λ (out) (write port out)))
  ;; Exit when Cadre does (its end of our standard input closes).
  (define stdin (current-input-port))
  (void (thread (λ () (let loop () (unless (eof-object? (read-char stdin)) (loop))) (exit 0))))
  (define-values (in out) (tcp-accept listener))
  (tcp-close listener)
  (set-connection-out! out)
  (warm-docs!)
  (serve in)
  (exit 0))
