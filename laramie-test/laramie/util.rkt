#lang racket/base

(provide check-length)

(require syntax/parse/define
         rackunit)

(define-simple-macro (check-length lst len)
  (check-= (length lst)
           len
           0))
