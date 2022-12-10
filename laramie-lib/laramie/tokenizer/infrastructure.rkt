#lang typed/racket/base/optional

(provide whitespace-codepoint?
         surrogate-char?
         surrogate-code-point?
         noncharacter-char?
         noncharacter-code-point?
         control-char?
         control-character-codepoint?
         hexadecimal-digit->integer
         digit->integer)

(require (file "types.rkt")
         (file "predicates.rkt"))

(module+ test
  (require typed/rackunit))

; https://infra.spec.whatwg.org/#ascii-whitespace
(: whitespace-codepoints (Listof Byte))
(define whitespace-codepoints
  (list #x0009 ; tab
        #x000a ; linefeed
        #x000c ; formfeed
        #x000d ; carraige return
        #x0020 ; space
        ))

(: whitespace-codepoint? (-> Exact-Nonnegative-Integer Boolean))
(define (whitespace-codepoint? b)
  (and (member b whitespace-codepoints =)
       #t))

(: surrogate-char? (-> Char Boolean))
(define (surrogate-char? c)
  (surrogate-code-point? (char->integer c)))

(: surrogate-code-point? (-> Exact-Nonnegative-Integer Boolean))
(define (surrogate-code-point? c)
  (<= #xd800 c #xdfff))

(: noncharacter-char? (-> Char Boolean))
(define (noncharacter-char? c)
  (noncharacter-code-point? (char->integer c)))

(: control-char? (-> Char Boolean))
(define (control-char? c)
  (or (char=? c #\u0000)
      (char=? c #\u001f)
      (and (char<=? #\u007f c)
           (char<=? c #\u009f))))

(: control-character-codepoint? (-> Exact-Nonnegative-Integer Boolean))
(define (control-character-codepoint? c)
  (or (= c #x0000)
      (= c #x001f)
      (and (<= #x007f c)
           (<= c #x009f))))

(: digit->integer (-> ASCIIDigit ZeroToNine))
(define (digit->integer d)
  (cond
    [(char=? d #\0) 0]
    [(char=? d #\1) 1]
    [(char=? d #\2) 2]
    [(char=? d #\3) 3]
    [(char=? d #\4) 4]
    [(char=? d #\5) 5]
    [(char=? d #\6) 6]
    [(char=? d #\7) 7]
    [(char=? d #\8) 8]
    [else 9]))

(: hexadecimal-digit->integer (-> HexadecimalDigit ZeroToFifteen))
(define (hexadecimal-digit->integer d)
  (cond
    [(char=? d #\0) 0]
    [(char=? d #\1) 1]
    [(char=? d #\2) 2]
    [(char=? d #\3) 3]
    [(char=? d #\4) 4]
    [(char=? d #\5) 5]
    [(char=? d #\6) 6]
    [(char=? d #\7) 7]
    [(char=? d #\8) 8]
    [(char=? d #\9) 9]
    [(char=? d #\a) 10]
    [(char=? d #\A) 10]
    [(char=? d #\b) 11]
    [(char=? d #\B) 11]
    [(char=? d #\c) 12]
    [(char=? d #\C) 12]
    [(char=? d #\d) 13]
    [(char=? d #\D) 13]
    [(char=? d #\e) 14]
    [(char=? d #\E) 14]
    [(char=? d #\f) 15]
    [else 15]))
