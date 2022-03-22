#lang racket/base

(provide todo/log/compile-time
         todo/log/run-time
         todo/error/compile-time
         todo/error/run-time)

(require (for-syntax syntax/parse
                     racket/base))

; Log execution of TODO code.
(define-syntax (todo/log/run-time stx)
  (syntax-parse stx
    [(_ e)
     (with-syntax ([origin (syntax-source stx)])
       #'(begin
           (unless (string=? (or (getenv "ALLOW_TODO") "")
                             "1")
             (log-error "TODO at ~a" origin))
           e))]))

; Allow TODOs, but emit error message at compile time.
(define-syntax (todo/log/compile-time stx)
  (syntax-parse stx
    [(_ e)
     (unless (string=? (or (getenv "ALLOW_TODO") "")
                       "1")
       (log-error "TODO at ~a" (syntax-source stx)))
     #'e]))

; Disallow TODOs (compile-time check errors out).
(define-syntax (todo/error/compile-time stx)
  (syntax-parse stx
    [(_ e)
     (unless (string=? (or (getenv "ALLOW_TODO") "")
                       "1")
       (error (format "TODO at ~a" (syntax-source stx))))
     #'e]))

; Error out at run-time when executing TODO code.
(define-syntax (todo/error/run-time stx)
  (with-syntax ([origin (syntax-source stx)])
    (syntax-parse stx
      [(_ e)
       #'(begin
           (unless (string=? (or (getenv "ALLOW_TODO") "")
                             "1")
             (error (format "TODO at ~a" origin)))
           e)])))
