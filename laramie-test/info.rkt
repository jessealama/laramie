#lang info

(define collection "tests")

(define deps '())

(define build-deps '("base"
                     "rackunit-lib"
                     "typed-racket-lib"
                     "laramie-lib"))

(define update-implies '("laramie-lib"))
