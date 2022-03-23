#lang info

(define version "0.2")
(define collection "laramie")
(define description "Implementation for Laramie")
(define authors '("jesse@serverracket.com"))
(define deps '("base"
               "typed-racket-lib"
               "txexpr"
               "http-easy"))
(define build-deps '("rackunit-lib"
                     "rackunit-typed"))
