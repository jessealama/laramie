#lang racket/base

(provide check-length
         descendants)

(require syntax/parse/define
         rackunit)

(define-simple-macro (check-length lst len)
  (check-= (length lst)
           len
           0))
