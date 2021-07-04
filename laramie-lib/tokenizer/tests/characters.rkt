#lang racket/base

(require racket/require
         (multi-in ".."
                   ("types.rkt"
                    "tokenize.rkt"
                    "tokens.rkt"
                    "characters.rkt")))

(module+ test
  (require rackunit
           racket/format
           syntax/parse/define))

(module+ test
  (define-simple-macro (check-it test-name subject value)
    (let ([tokens (tokenize subject
                            #:include-dropped? #t
                            #:include-errors? #t)])
      (test-case
          (format "~a [value]" test-name)
        (check-equal? tokens
                      value))
      (test-case
          (format "~a [length]" test-name)
        (check-= (length (enumerate-input-characters tokens))
                 (string-length subject)
                 0)))))

(module+ test
  (check-it
   "Null character in toplevel"
   (~a #\u0000)
   (list
    (unexpected-null-character (location 1 0 1) #f)
    (character-token (location 1 0 1) (location 1 1 2) '(#\nul . #f))))
  (check-it
   "Character reference EOFs"
   "&"
   (list (character-token (location 1 0 1) (location 1 1 2) #\&)))
  (check-it
   "Numeric character reference EOFs"
   "&#"
   (list
    (absence-of-digits-in-numeric-character-reference (location 1 0 1) #f)
    (character-token (location 1 0 1) (location 1 1 2) #\&)
    (character-token (location 1 1 2) (location 1 2 3) #\#)))
  (check-it
   "Decimal character reference EOFs"
   "&#1"
   (list
    (missing-semicolon-after-character-reference (location 1 0 1) #f)
    (control-character-reference
     (location 1 0 1)
     (list
      (character-token (location 1 0 1) (location 1 1 2) #\&)
      (character-token (location 1 1 2) (location 1 2 3) #\#)
      (character-token (location 1 2 3) (location 1 3 4) #\1)))))
  (check-it
   "Hexadecimal character reference EOFs"
   "&#x"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\&)
    (character-token (location 1 1 2) (location 1 2 3) #\#)
    (character-token (location 1 2 3) (location 1 3 4) #\x)))
  (check-it
   "Invalid named character reference"
   "&screwthis;"
   (list
    (unknown-named-character-reference
     (location 1 11 12)
     (list
      (character-token (location 1 0 1) (location 1 1 2) #\&)
      (character-token (location 1 1 2) (location 1 2 3) #\s)
      (character-token (location 1 2 3) (location 1 3 4) #\c)
      (character-token (location 1 3 4) (location 1 4 5) #\r)
      (character-token (location 1 4 5) (location 1 5 6) #\e)
      (character-token (location 1 5 6) (location 1 6 7) #\w)
      (character-token (location 1 6 7) (location 1 7 8) #\t)
      (character-token (location 1 7 8) (location 1 8 9) #\h)
      (character-token (location 1 8 9) (location 1 9 10) #\i)
      (character-token (location 1 9 10) (location 1 10 11) #\s)
      (character-token (location 1 10 11) (location 1 11 12) #\;)))))
  (check-it
   "Weird character in unknown named character reference"
   "&screwthis"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\&)
    (character-token (location 1 1 2) (location 1 2 3) #\s)
    (character-token (location 1 2 3) (location 1 3 4) #\c)
    (character-token (location 1 3 4) (location 1 4 5) #\r)
    (character-token (location 1 4 5) (location 1 5 6) #\e)
    (character-token (location 1 5 6) (location 1 6 7) #\w)
    (character-token (location 1 6 7) (location 1 7 8) #\t)
    (character-token (location 1 7 8) (location 1 8 9) #\h)
    (character-token (location 1 8 9) (location 1 9 10) #\i)
    (character-token (location 1 9 10) (location 1 10 11) #\s)))
  (check-it
   "Decimal character reference out of bounds"
   "&#99999999999999999999;"
   (list
    (character-reference-outside-unicode-range
     (location 1 0 1)
     (list
      (character-token (location 1 0 1) (location 1 1 2) #\&)
      (character-token (location 1 1 2) (location 1 2 3) #\#)
      (character-token (location 1 2 3) (location 1 3 4) #\9)
      (character-token (location 1 3 4) (location 1 4 5) #\9)
      (character-token (location 1 4 5) (location 1 5 6) #\9)
      (character-token (location 1 5 6) (location 1 6 7) #\9)
      (character-token (location 1 6 7) (location 1 7 8) #\9)
      (character-token (location 1 7 8) (location 1 8 9) #\9)
      (character-token (location 1 8 9) (location 1 9 10) #\9)
      (character-token (location 1 9 10) (location 1 10 11) #\9)
      (character-token (location 1 10 11) (location 1 11 12) #\9)
      (character-token (location 1 11 12) (location 1 12 13) #\9)
      (character-token (location 1 12 13) (location 1 13 14) #\9)
      (character-token (location 1 13 14) (location 1 14 15) #\9)
      (character-token (location 1 14 15) (location 1 15 16) #\9)
      (character-token (location 1 15 16) (location 1 16 17) #\9)
      (character-token (location 1 16 17) (location 1 17 18) #\9)
      (character-token (location 1 17 18) (location 1 18 19) #\9)
      (character-token (location 1 18 19) (location 1 19 20) #\9)
      (character-token (location 1 19 20) (location 1 20 21) #\9)
      (character-token (location 1 20 21) (location 1 21 22) #\9)
      (character-token (location 1 21 22) (location 1 22 23) #\9)
      (character-token (location 1 22 23) (location 1 23 24) #\;)))
    (character-token
     (location 1 0 1)
     (location 1 23 24)
     '((#\&
        #\#
        #\9
        #\9
        #\9
        #\9
        #\9
        #\9
        #\9
        #\9
        #\9
        #\9
        #\9
        #\9
        #\9
        #\9
        #\9
        #\9
        #\9
        #\9
        #\9
        #\9
        #\;)
       .
       #\�))))
  (check-it
   "OK decimal reference (P sign)"
   "&#80;"
   (list
    (character-reference-token
     (location 1 0 1)
     (location 1 0 0)
     (list
      (character-token (location 1 0 1) (location 1 1 2) #\&)
      (character-token (location 1 1 2) (location 1 2 3) #\#)
      (character-token (location 1 2 3) (location 1 3 4) #\8)
      (character-token (location 1 3 4) (location 1 4 5) #\0)
      (character-token (location 1 4 5) (location 1 5 6) #\;))
     #\P
     #f)))
  (check-it
   "OK hexadecimal reference (U sign)"
   "&#x55;"
   (list
    (character-reference-token
     (location 1 0 1)
     (location 1 0 0)
     (list
      (character-token (location 1 0 1) (location 1 1 2) #\&)
      (character-token (location 1 1 2) (location 1 2 3) #\#)
      (character-token (location 1 2 3) (location 1 3 4) #\x)
      (character-token (location 1 3 4) (location 1 4 5) #\5)
      (character-token (location 1 4 5) (location 1 5 6) #\5)
      (character-token (location 1 5 6) (location 1 6 7) #\;))
     #\U
     #f)))
  (check-it
   "Decimal character reference bails out"
   "&#123x;"
   (list
    (missing-semicolon-after-character-reference (location 1 0 1) #f)
    (character-reference-token
     (location 1 0 1)
     (location 1 0 0)
     (list
      (character-token (location 1 0 1) (location 1 1 2) #\&)
      (character-token (location 1 1 2) (location 1 2 3) #\#)
      (character-token (location 1 2 3) (location 1 3 4) #\1)
      (character-token (location 1 3 4) (location 1 4 5) #\2)
      (character-token (location 1 4 5) (location 1 5 6) #\3))
     #\{
     #f)
    (character-token (location 1 5 6) (location 1 6 7) #\x)
    (character-token (location 1 6 7) (location 1 7 8) #\;)))
  (check-it
   "Hexadecimal character reference includes non-hexadecimal character"
   "&#xabcdefg;"
   (list
    (missing-semicolon-after-character-reference (location 1 0 1) #f)
    (character-reference-outside-unicode-range
     (location 1 0 1)
     (list
      (character-token (location 1 0 1) (location 1 1 2) #\&)
      (character-token (location 1 1 2) (location 1 2 3) #\#)
      (character-token (location 1 2 3) (location 1 3 4) #\x)
      (character-token (location 1 3 4) (location 1 4 5) #\a)
      (character-token (location 1 4 5) (location 1 5 6) #\b)
      (character-token (location 1 5 6) (location 1 6 7) #\c)
      (character-token (location 1 6 7) (location 1 7 8) #\d)
      (character-token (location 1 7 8) (location 1 8 9) #\e)
      (character-token (location 1 8 9) (location 1 9 10) #\f)))
    (character-token
     (location 1 0 1)
     (location 1 9 10)
     '((#\& #\# #\x #\a #\b #\c #\d #\e #\f) . #\�))
    (character-token (location 1 9 10) (location 1 10 11) #\g)
    (character-token (location 1 10 11) (location 1 11 12) #\;)))
  (check-it
   "Illegal start to hexadecimal reference"
   "&#xx;"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\&)
    (character-token (location 1 1 2) (location 1 2 3) #\#)
    (character-token (location 1 2 3) (location 1 3 4) #\x)
    (character-token (location 1 3 4) (location 1 4 5) #\x)
    (character-token (location 1 4 5) (location 1 5 6) #\;)))
  (check-it
   "EOF in hexadecimal reference"
   "&#xa"
   (list
    (missing-semicolon-after-character-reference (location 1 0 1) #f)
    (character-reference-token
     (location 1 0 1)
     (location 1 0 0)
     (list
      (character-token (location 1 0 1) (location 1 1 2) #\&)
      (character-token (location 1 1 2) (location 1 2 3) #\#)
      (character-token (location 1 2 3) (location 1 3 4) #\x)
      (character-token (location 1 3 4) (location 1 4 5) #\a))
     #\newline
     #f)))
  (check-it
   "Upgrade hexadecimal character reference to something else (€)"
   "&#x80;"
   (list
    (character-reference-token
     (location 1 0 1)
     (location 1 0 0)
     (list
      (character-token (location 1 0 1) (location 1 1 2) #\&)
      (character-token (location 1 1 2) (location 1 2 3) #\#)
      (character-token (location 1 2 3) (location 1 3 4) #\x)
      (character-token (location 1 3 4) (location 1 4 5) #\8)
      (character-token (location 1 4 5) (location 1 5 6) #\0)
      (character-token (location 1 5 6) (location 1 6 7) #\;))
     #\€
     #t)))
  (check-it
   "Upgrade decimal character reference to something else (…)"
   "&#133;"
   (list
    (character-reference-token
     (location 1 0 1)
     (location 1 0 0)
     (list
      (character-token (location 1 0 1) (location 1 1 2) #\&)
      (character-token (location 1 1 2) (location 1 2 3) #\#)
      (character-token (location 1 2 3) (location 1 3 4) #\1)
      (character-token (location 1 3 4) (location 1 4 5) #\3)
      (character-token (location 1 4 5) (location 1 5 6) #\3)
      (character-token (location 1 5 6) (location 1 6 7) #\;))
     #\…
     #t)))
  (check-it
   "Surrogate character encoded as a character reference"
   "&#xd800;"
   (list
    (surrogate-character-reference
     (location 1 0 1)
     (list
      (character-token (location 1 0 1) (location 1 1 2) #\&)
      (character-token (location 1 1 2) (location 1 2 3) #\#)
      (character-token (location 1 2 3) (location 1 3 4) #\x)
      (character-token (location 1 3 4) (location 1 4 5) #\d)
      (character-token (location 1 4 5) (location 1 5 6) #\8)
      (character-token (location 1 5 6) (location 1 6 7) #\0)
      (character-token (location 1 6 7) (location 1 7 8) #\0)
      (character-token (location 1 7 8) (location 1 8 9) #\;)))
    (character-token
     (location 1 0 1)
     (location 1 8 9)
     '((#\& #\# #\x #\d #\8 #\0 #\0 #\;) . #\�))))
  (check-it
   "Non-character encoded as a character reference"
   "&#x6ffff;"
   (list
    (noncharacter-character-reference
     (location 1 0 1)
     (list
      (character-token (location 1 0 1) (location 1 1 2) #\&)
      (character-token (location 1 1 2) (location 1 2 3) #\#)
      (character-token (location 1 2 3) (location 1 3 4) #\x)
      (character-token (location 1 3 4) (location 1 4 5) #\6)
      (character-token (location 1 4 5) (location 1 5 6) #\f)
      (character-token (location 1 5 6) (location 1 6 7) #\f)
      (character-token (location 1 6 7) (location 1 7 8) #\f)
      (character-token (location 1 7 8) (location 1 8 9) #\f)
      (character-token (location 1 8 9) (location 1 9 10) #\;)))))
  (check-it
   "Control character encoded as a character reference"
   "&#x000e;"
   (list
    (control-character-reference
     (location 1 0 1)
     (list
      (character-token (location 1 0 1) (location 1 1 2) #\&)
      (character-token (location 1 1 2) (location 1 2 3) #\#)
      (character-token (location 1 2 3) (location 1 3 4) #\x)
      (character-token (location 1 3 4) (location 1 4 5) #\0)
      (character-token (location 1 4 5) (location 1 5 6) #\0)
      (character-token (location 1 5 6) (location 1 6 7) #\0)
      (character-token (location 1 6 7) (location 1 7 8) #\e)
      (character-token (location 1 7 8) (location 1 8 9) #\;)))))
  (check-it
   "Particular control character (explicitly singled out in the HTML5 spec)"
   "&#x0d;"
   (list
    (control-character-reference
     (location 1 0 1)
     (list
      (character-token (location 1 0 1) (location 1 1 2) #\&)
      (character-token (location 1 1 2) (location 1 2 3) #\#)
      (character-token (location 1 2 3) (location 1 3 4) #\x)
      (character-token (location 1 3 4) (location 1 4 5) #\0)
      (character-token (location 1 4 5) (location 1 5 6) #\d)
      (character-token (location 1 5 6) (location 1 6 7) #\;)))))
  (check-it
   "Illegal start to decimal character reference"
   "&#p;"
   (list
    (absence-of-digits-in-numeric-character-reference (location 1 0 1) #f)
    (character-token (location 1 0 1) (location 1 1 2) #\&)
    (character-token (location 1 1 2) (location 1 2 3) #\#)
    (character-token (location 1 2 3) (location 1 3 4) #\p)
    (character-token (location 1 3 4) (location 1 4 5) #\;))))
