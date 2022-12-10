#lang typed/racket/base/optional

(provide token?
         character-token?
         noncharacter-code-point?
         control-code-point?
         ascii-whitespace-code-point?
         hexadecimal-digit?
         decimal-digit?
         ascii-alpha?
         ascii-uppercase?
         ascii-lowercase?
         ascii-digit?
         html5-whitespace?
         ascii-alphanumeric?)

(require (file "types.rkt"))

(module+ test
  (require typed/rackunit))

(define-predicate token? (U Char
                            (Pairof Char (Option Char))
                            (Pairof (Listof Char) (U Char (List Char Char))) ; character references replaced by a character
                            tag-token
                            comment-token
                            doctype-token))

(define-predicate noncharacter-code-point? NonCharacterCodepoint)

(module+ test
  (check-true (noncharacter-code-point? #x6ffff)))

(define-predicate control-code-point? ControlCodepoint)

(module+ test
  (check-true (control-code-point? #x000e))
  (check-true (control-code-point? 14))
  (check-false (html5-whitespace? 14)))

(define-predicate ascii-whitespace-code-point? ASCIIWhitespaceCodepoint)

(module+ test
  (check-false (ascii-whitespace-code-point? 14)))

(define-predicate hexadecimal-digit? HexadecimalDigit)

(define-predicate decimal-digit? DecimalDigit)

(define-predicate ascii-alpha? (U ASCIILowercase ASCIIUppercase))

(define-predicate ascii-uppercase? ASCIIUppercase)

(define-predicate ascii-lowercase? ASCIILowercase)

(define-predicate ascii-digit? ASCIIDigit)

(define-predicate html5-whitespace? HTML5Whitespace)

(define-predicate ascii-alphanumeric? ASCIIAlphanumeric)

(module+ test
  (check-false (ascii-lowercase? (integer->char 96)))
  (for ([n (in-range 97 (+ 97 26))])
    (let ([c (integer->char n)])
      (check-true (ascii-lowercase? c)
                  (format "~a should be lowercase, but it isn't!" c))))
  (check-false (ascii-lowercase? (integer->char (add1 (+ 96 26)))))
  (for ([n (in-range 65 (+ 65 26))])
    (let ([c (integer->char n)])
      (check-false (ascii-lowercase? c)
                   (format "~a shouldn't be lowercase, but it is!" c))))
  (check-false (ascii-lowercase? #\nul))
  (check-false (ascii-lowercase? #\!)))
