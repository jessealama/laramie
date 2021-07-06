#lang info

(define collection "laramie")

(define build-deps '("scribble-lib"
                     "laramie-lib"
                     "xml"
                     "racket-doc"))

(define deps '("base"))

(define update-implies '("laramie-lib"))

(define pkg-desc "Documentation for Laramie")

(define scribblings '(("laramie.scrbl")))

(define authors '("jesse@serverracket.com"))
