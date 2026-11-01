#lang racket/base
;;; Data written so Common Lisp's reader (Cadre's safe reader) reads it.
;;;
;;; Copyright (c) 2026 Jessie Hughart. MIT License (see LICENSE beside this file).

(provide write-cl write-cl-string)

(define (write-cl v out)
  (cond [(eq? v #t) (write-string "t" out)]
        [(or (eq? v #f) (null? v) (void? v)) (write-string "nil" out)]
        [(string? v) (write-cl-string v out)]
        [(symbol? v) (write-string (symbol->string v) out)]
        [(exact-integer? v) (write-string (number->string v) out)]
        [(and (real? v) (rational? v)) (write-string (real->decimal-string v 3) out)]
        [(path? v) (write-cl-string (path->string v) out)]
        [(pair? v)
         (write-string "(" out)
         (let loop ([v v] [first? #t])
           (cond [(null? v)]
                 [(pair? v)
                  (unless first? (write-string " " out))
                  (write-cl (car v) out)
                  (loop (cdr v) #f)]
                 [else (write-string " . " out) (write-cl v out)]))
         (write-string ")" out)]
        [else (write-cl-string (format "~a" v) out)]))

(define (write-cl-string s out)
  (write-string "\"" out)
  (for ([c (in-string s)])
    (when (memv c '(#\" #\\)) (write-char #\\ out))
    (write-char c out))
  (write-string "\"" out))

