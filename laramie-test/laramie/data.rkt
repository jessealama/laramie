#lang racket/base

(require (file "util.rkt")
         racket/require
         (multi-in "../src"
                   ("types.rkt"
                    "tokenize.rkt"
                    "tokens.rkt")))

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
   "No data at all"
   ""
   (list))
  (check-it
   "Null character in toplevel"
   (~a #\u0000)
   (list
    (unexpected-null-character (location 1 0 1) #f)
    (character-token (location 1 0 1) (location 1 1 2) '(#\nul . #f))))
  (check-it
   "EOF in bogus comment"
   "<!"
   (list
    (incorrectly-opened-comment (location 1 2 3) #f)
    (comment-token
     (location 1 0 1)
     (location 1 2 3)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     #f
     '()
     #t
     #f
     #f)))
  (check-it
   "Null character in bogus comment"
   (format "<?~af>" #\nul)
   (list
    (unexpected-question-mark-instead-of-tag-name (location 1 1 2) #f)
    (comment-token
     (location 1 0 1)
     (location 1 4 5)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\?)
     #f
     (list
      (character-token (location 1 2 3) (location 1 3 4) '(#\nul . #\�))
      (character-token (location 1 3 4) (location 1 4 5) #\f))
     #t
     #f
     #f)
    (character-token (location 1 4 5) (location 1 5 6) #\>)))
  (check-it
   "Capitalized DOCTYPE"
   "<!DOCTYPE html>"
   (list
    (doctype-token
     (location 1 0 1)
     (location 1 15 16)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\D)
      (character-token (location 1 3 4) (location 1 4 5) #\O)
      (character-token (location 1 4 5) (location 1 5 6) #\C)
      (character-token (location 1 5 6) (location 1 6 7) #\T)
      (character-token (location 1 6 7) (location 1 7 8) #\Y)
      (character-token (location 1 7 8) (location 1 8 9) #\P)
      (character-token (location 1 8 9) (location 1 9 10) #\E))
     (list
      (character-token (location 1 10 11) (location 1 11 12) #\h)
      (character-token (location 1 11 12) (location 1 12 13) #\t)
      (character-token (location 1 12 13) (location 1 13 14) #\m)
      (character-token (location 1 13 14) (location 1 14 15) #\l))
     #f
     #f
     #f
     #f
     (character-token (location 1 14 15) (location 1 15 16) #\>)
     #f
     (list (character-token (location 1 9 10) (location 1 10 11) #\space)))))
  (check-it
   "Simple CDATA"
   "<![CDATA[wat]]>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) '(#\< . #f))
    (character-token (location 1 1 2) (location 1 2 3) '(#\! . #f))
    (character-token (location 1 2 3) (location 1 3 4) '(#\[ . #f))
    (character-token (location 1 3 4) (location 1 4 5) '(#\C . #f))
    (character-token (location 1 4 5) (location 1 5 6) '(#\D . #f))
    (character-token (location 1 5 6) (location 1 6 7) '(#\A . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\T . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\A . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\[ . #f))
    (string-token (location 1 9 10) (location 1 12 13) "wat")
    (character-token (location 1 12 13) (location 1 13 14) '(#\] . #f))
    (character-token (location 1 13 14) (location 1 14 15) '(#\] . #f))
    (character-token (location 1 14 15) (location 1 15 16) '(#\> . #f))))
  (check-it
   "Empty tag"
   "<p>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 3 4)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     '()
     #f
     (character-token (location 1 2 3) (location 1 3 4) #\>)
     '())))
  (check-it
   "Weird character reference in attribute value"
   "<p ex=\"I'm &notit; I tell you\">" ; string intact, no error
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 31 32)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 31 32)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\e)
        (character-token (location 1 4 5) (location 1 5 6) #\x))
       (character-token (location 1 5 6) (location 1 6 7) #\=)
       (quoted-attr-value
        (character-token (location 1 6 7) (location 1 7 8) #\")
        (list
         (character-token (location 1 7 8) (location 1 8 9) #\I)
         (character-token (location 1 8 9) (location 1 9 10) #\')
         (character-token (location 1 9 10) (location 1 10 11) #\m)
         (character-token (location 1 10 11) (location 1 11 12) #\space)
         (character-token (location 1 11 12) (location 1 12 13) #\&)
         (character-token (location 1 12 13) (location 1 13 14) #\n)
         (character-token (location 1 13 14) (location 1 14 15) #\o)
         (character-token (location 1 14 15) (location 1 15 16) #\t)
         (character-token (location 1 15 16) (location 1 16 17) #\i)
         (character-token (location 1 16 17) (location 1 17 18) #\t)
         (character-token (location 1 17 18) (location 1 18 19) #\;)
         (character-token (location 1 18 19) (location 1 19 20) #\space)
         (character-token (location 1 19 20) (location 1 20 21) #\I)
         (character-token (location 1 20 21) (location 1 21 22) #\space)
         (character-token (location 1 21 22) (location 1 22 23) #\t)
         (character-token (location 1 22 23) (location 1 23 24) #\e)
         (character-token (location 1 23 24) (location 1 24 25) #\l)
         (character-token (location 1 24 25) (location 1 25 26) #\l)
         (character-token (location 1 25 26) (location 1 26 27) #\space)
         (character-token (location 1 26 27) (location 1 27 28) #\y)
         (character-token (location 1 27 28) (location 1 28 29) #\o)
         (character-token (location 1 28 29) (location 1 29 30) #\u))
        (character-token (location 1 29 30) (location 1 30 31) #\"))))
     #f
     (character-token (location 1 30 31) (location 1 31 32) #\>)
     (list (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "Character reference at the top level"
   "<p>I'm &notit; I tell you</p>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 3 4)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     '()
     #f
     (character-token (location 1 2 3) (location 1 3 4) #\>)
     '())
    (character-token (location 1 3 4) (location 1 4 5) #\I)
    (character-token (location 1 4 5) (location 1 5 6) #\')
    (character-token (location 1 5 6) (location 1 6 7) #\m)
    (character-token (location 1 6 7) (location 1 7 8) #\space)
    (missing-semicolon-after-character-reference (location 1 11 12) #f)
    (character-reference-token
     (location 1 7 8)
     (location 1 11 12)
     (list
      (character-token (location 1 7 8) (location 1 8 9) #\&)
      (character-token (location 1 8 9) (location 1 9 10) #\n)
      (character-token (location 1 9 10) (location 1 10 11) #\o)
      (character-token (location 1 10 11) (location 1 11 12) #\t))
     #\¬
     #f)
    (character-token (location 1 11 12) (location 1 12 13) #\i)
    (character-token (location 1 12 13) (location 1 13 14) #\t)
    (character-token (location 1 13 14) (location 1 14 15) #\;)
    (character-token (location 1 14 15) (location 1 15 16) #\space)
    (character-token (location 1 15 16) (location 1 16 17) #\I)
    (character-token (location 1 16 17) (location 1 17 18) #\space)
    (character-token (location 1 17 18) (location 1 18 19) #\t)
    (character-token (location 1 18 19) (location 1 19 20) #\e)
    (character-token (location 1 19 20) (location 1 20 21) #\l)
    (character-token (location 1 20 21) (location 1 21 22) #\l)
    (character-token (location 1 21 22) (location 1 22 23) #\space)
    (character-token (location 1 22 23) (location 1 23 24) #\y)
    (character-token (location 1 23 24) (location 1 24 25) #\o)
    (character-token (location 1 24 25) (location 1 25 26) #\u)
    (end-tag-token
     (location 1 25 26)
     (location 1 29 30)
     (character-token (location 1 25 26) (location 1 26 27) #\<)
     (list (character-token (location 1 27 28) (location 1 28 29) #\p))
     '()
     #f
     (character-token (location 1 28 29) (location 1 29 30) #\>)
     '()
     (character-token (location 1 26 27) (location 1 27 28) #\/))))
  (check-it
   "Another character reference at the top level"
   "<p>I'm &notin; I tell you</p>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 3 4)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     '()
     #f
     (character-token (location 1 2 3) (location 1 3 4) #\>)
     '())
    (character-token (location 1 3 4) (location 1 4 5) #\I)
    (character-token (location 1 4 5) (location 1 5 6) #\')
    (character-token (location 1 5 6) (location 1 6 7) #\m)
    (character-token (location 1 6 7) (location 1 7 8) #\space)
    (character-reference-token
     (location 1 7 8)
     (location 1 14 15)
     (list
      (character-token (location 1 7 8) (location 1 8 9) #\&)
      (character-token (location 1 8 9) (location 1 9 10) #\n)
      (character-token (location 1 9 10) (location 1 10 11) #\o)
      (character-token (location 1 10 11) (location 1 11 12) #\t)
      (character-token (location 1 11 12) (location 1 12 13) #\i)
      (character-token (location 1 12 13) (location 1 13 14) #\n)
      (character-token (location 1 13 14) (location 1 14 15) #\;))
     #\∉
     #f)
    (character-token (location 1 14 15) (location 1 15 16) #\space)
    (character-token (location 1 15 16) (location 1 16 17) #\I)
    (character-token (location 1 16 17) (location 1 17 18) #\space)
    (character-token (location 1 17 18) (location 1 18 19) #\t)
    (character-token (location 1 18 19) (location 1 19 20) #\e)
    (character-token (location 1 19 20) (location 1 20 21) #\l)
    (character-token (location 1 20 21) (location 1 21 22) #\l)
    (character-token (location 1 21 22) (location 1 22 23) #\space)
    (character-token (location 1 22 23) (location 1 23 24) #\y)
    (character-token (location 1 23 24) (location 1 24 25) #\o)
    (character-token (location 1 24 25) (location 1 25 26) #\u)
    (end-tag-token
     (location 1 25 26)
     (location 1 29 30)
     (character-token (location 1 25 26) (location 1 26 27) #\<)
     (list (character-token (location 1 27 28) (location 1 28 29) #\p))
     '()
     #f
     (character-token (location 1 28 29) (location 1 29 30) #\>)
     '()
     (character-token (location 1 26 27) (location 1 27 28) #\/))))
  (check-it
   "Ampersand space"
   "& "
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\&)
    (character-token (location 1 1 2) (location 1 2 3) #\space))))
