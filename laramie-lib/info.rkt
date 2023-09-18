#lang info

(define collection 'multi)
(define version "0.3.1")
(define description "Implementation for Laramie")
(define authors '("jesse@serverracket.com"))
(define deps '(("base" #:version "8.7")
               "typed-racket-lib"
               "typed-racket-more"
               "txexpr"
               "http-easy"))
(define build-deps '("rackunit-lib"
                     "rackunit-typed"
                     "http-easy"))
