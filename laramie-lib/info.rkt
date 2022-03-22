#lang info

(define version "0.2")
(define collection 'multi)
(define description "Implementation for Laramie")
(define authors '("jesse@serverracket.com"))
(define setup-collects '("laramie"))
(define deps '("base"
               "typed-racket-lib"
               "txexpr"
               "http-easy"))
(define build-deps '("rackunit-lib"
                     "rackunit-typed"))
