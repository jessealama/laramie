#lang racket/base

(require laramie/tokenize
         (file "util.rkt"))

(module+ test
  (require rackunit
           syntax/parse/define))

(module+ test
  (define-simple-macro (check-it test-name subject quirky? value)
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
                 0))
      (let ([doctype (findf doctype-token? tokens)])
        (cond [(eq? #f doctype)
               (error "No doctype token found!")]
              [else
               (test-case
                   (format "~a [quirks]" test-name)
                 (check-equal? (doctype-token-quirky? doctype)
                               quirky?))])))))

(module+ test
  (check-it
   "Unquoted system identifier following public identifier"
   "<!DOCTYPE q PUBLIC \"wat\"this>"
   #t
   (list
    (missing-quote-before-doctype-system-identifier (location 1 24 25) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 29 30)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\q))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\P)
      (character-token (location 1 13 14) (location 1 14 15) #\U)
      (character-token (location 1 14 15) (location 1 15 16) #\B)
      (character-token (location 1 15 16) (location 1 16 17) #\L)
      (character-token (location 1 16 17) (location 1 17 18) #\I)
      (character-token (location 1 17 18) (location 1 18 19) #\C))
     (list
      (character-token (location 1 20 21) (location 1 21 22) #\w)
      (character-token (location 1 21 22) (location 1 22 23) #\a)
      (character-token (location 1 22 23) (location 1 23 24) #\t))
     #f
     #f
     (character-token (location 1 28 29) (location 1 29 30) #\>)
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) #\")
      (character-token (location 1 23 24) (location 1 24 25) #\")
      (character-token (location 1 24 25) (location 1 25 26) '(#\t . #f))
      (character-token (location 1 25 26) (location 1 26 27) '(#\h . #f))
      (character-token (location 1 26 27) (location 1 27 28) '(#\i . #f))
      (character-token (location 1 27 28) (location 1 28 29) '(#\s . #f))))))
  (check-it
   "No space separating PUBLIC and SYSTEM identifiers"
   "<!DOCTYPE t PUBLIC \"hi\"'bye'>"
   #f
   (list
    (missing-whitespace-between-doctype-public-and-system-identifiers
     (location 1 23 24)
     #f)
    (doctype-token
     (location 1 0 1)
     (location 1 29 30)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\t))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\P)
      (character-token (location 1 13 14) (location 1 14 15) #\U)
      (character-token (location 1 14 15) (location 1 15 16) #\B)
      (character-token (location 1 15 16) (location 1 16 17) #\L)
      (character-token (location 1 16 17) (location 1 17 18) #\I)
      (character-token (location 1 17 18) (location 1 18 19) #\C))
     (list
      (character-token (location 1 20 21) (location 1 21 22) #\h)
      (character-token (location 1 21 22) (location 1 22 23) #\i))
     #f
     (list
      (character-token (location 1 24 25) (location 1 25 26) #\b)
      (character-token (location 1 25 26) (location 1 26 27) #\y)
      (character-token (location 1 26 27) (location 1 27 28) #\e))
     (character-token (location 1 28 29) (location 1 29 30) #\>)
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) #\")
      (character-token (location 1 22 23) (location 1 23 24) #\")
      (character-token (location 1 23 24) (location 1 24 25) #\')
      (character-token (location 1 27 28) (location 1 28 29) #\')))))
  (check-it
   "Non-quote after public keyword"
   "<!DOCTYPE e PUBLIC juice"
   #t
   (list
    (missing-quote-before-doctype-public-identifier (location 1 19 20) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 24 25)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\e))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\P)
      (character-token (location 1 13 14) (location 1 14 15) #\U)
      (character-token (location 1 14 15) (location 1 15 16) #\B)
      (character-token (location 1 15 16) (location 1 16 17) #\L)
      (character-token (location 1 16 17) (location 1 17 18) #\I)
      (character-token (location 1 17 18) (location 1 18 19) #\C))
     #f
     #f
     #f
     #f
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) '(#\j . #f))
      (character-token (location 1 20 21) (location 1 21 22) '(#\u . #f))
      (character-token (location 1 21 22) (location 1 22 23) '(#\i . #f))
      (character-token (location 1 22 23) (location 1 23 24) '(#\c . #f))
      (character-token (location 1 23 24) (location 1 24 25) '(#\e . #f))))))
  (check-it
   "Closing \">\" after public keyword"
   "<!DOCTYPE r PUBLIC>"
   #t
   (list
    (missing-doctype-public-identifier (location 1 18 19) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 19 20)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\r))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\P)
      (character-token (location 1 13 14) (location 1 14 15) #\U)
      (character-token (location 1 14 15) (location 1 15 16) #\B)
      (character-token (location 1 15 16) (location 1 16 17) #\L)
      (character-token (location 1 16 17) (location 1 17 18) #\I)
      (character-token (location 1 17 18) (location 1 18 19) #\C))
     #f
     #f
     #f
     (character-token (location 1 18 19) (location 1 19 20) #\>)
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)))))
  (check-it
   "EOF immediately after PUBLIC keyword"
   "<!DOCTYPE k PUBLIC "
   #t
   (list
    (eof-in-doctype (location 1 19 20) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 19 20)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\k))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\P)
      (character-token (location 1 13 14) (location 1 14 15) #\U)
      (character-token (location 1 14 15) (location 1 15 16) #\B)
      (character-token (location 1 15 16) (location 1 16 17) #\L)
      (character-token (location 1 16 17) (location 1 17 18) #\I)
      (character-token (location 1 17 18) (location 1 18 19) #\C))
     #f
     #f
     #f
     #f
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)))))
  (check-it
   "Unexpected content after DOCTYPE system identifier"
   "<!DOCTYPE t PUBLIC \"foo\" \"bar\" \"jazz\""
   #f
   (list
    (unexpected-character-after-doctype-system-identifier
     (location 1 31 32)
     (character-token (location 1 31 32) (location 1 32 33) #\"))
    (doctype-token
     (location 1 0 1)
     (location 1 37 38)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\t))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\P)
      (character-token (location 1 13 14) (location 1 14 15) #\U)
      (character-token (location 1 14 15) (location 1 15 16) #\B)
      (character-token (location 1 15 16) (location 1 16 17) #\L)
      (character-token (location 1 16 17) (location 1 17 18) #\I)
      (character-token (location 1 17 18) (location 1 18 19) #\C))
     (list
      (character-token (location 1 20 21) (location 1 21 22) #\f)
      (character-token (location 1 21 22) (location 1 22 23) #\o)
      (character-token (location 1 22 23) (location 1 23 24) #\o))
     #f
     (list
      (character-token (location 1 26 27) (location 1 27 28) #\b)
      (character-token (location 1 27 28) (location 1 28 29) #\a)
      (character-token (location 1 28 29) (location 1 29 30) #\r))
     #f
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) #\")
      (character-token (location 1 23 24) (location 1 24 25) #\")
      (character-token (location 1 24 25) (location 1 25 26) #\space)
      (character-token (location 1 25 26) (location 1 26 27) #\")
      (character-token (location 1 29 30) (location 1 30 31) #\")
      (character-token (location 1 30 31) (location 1 31 32) #\space)
      (character-token (location 1 31 32) (location 1 32 33) '(#\" . #f))
      (character-token (location 1 32 33) (location 1 33 34) '(#\j . #f))
      (character-token (location 1 33 34) (location 1 34 35) '(#\a . #f))
      (character-token (location 1 34 35) (location 1 35 36) '(#\z . #f))
      (character-token (location 1 35 36) (location 1 36 37) '(#\z . #f))
      (character-token (location 1 36 37) (location 1 37 38) '(#\" . #f))))))
  (check-it
   "EOF after DOCTYPE system identifier"
   "<!DOCTYPE r SYSTEM"
   #t
   (list
    (eof-in-doctype (location 1 18 19) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 18 19)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\r))
     #f
     #f
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\S)
      (character-token (location 1 13 14) (location 1 14 15) #\Y)
      (character-token (location 1 14 15) (location 1 15 16) #\S)
      (character-token (location 1 15 16) (location 1 16 17) #\T)
      (character-token (location 1 16 17) (location 1 17 18) #\E)
      (character-token (location 1 17 18) (location 1 18 19) #\M))
     #f
     #f
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)))))
  (check-it
   "Abrupt closing \">\" in DOCTYPE system identifier (single-quoted)"
   "<!DOCTYPE r SYSTEM 'jok>"
   #t
   (list
    (abrupt-doctype-system-identifier (location 1 23 24) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 24 25)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\r))
     #f
     #f
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\S)
      (character-token (location 1 13 14) (location 1 14 15) #\Y)
      (character-token (location 1 14 15) (location 1 15 16) #\S)
      (character-token (location 1 15 16) (location 1 16 17) #\T)
      (character-token (location 1 16 17) (location 1 17 18) #\E)
      (character-token (location 1 17 18) (location 1 18 19) #\M))
     (list
      (character-token (location 1 20 21) (location 1 21 22) #\j)
      (character-token (location 1 21 22) (location 1 22 23) #\o)
      (character-token (location 1 22 23) (location 1 23 24) #\k))
     (character-token (location 1 23 24) (location 1 24 25) #\>)
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) #\')))))
  (check-it
   "Abrupt closing \">\" in DOCTYPE system identifier (double-quoted)"
   "<!DOCTYPE r SYSTEM \"jok>"
   #t
   (list
    (abrupt-doctype-public-identifier (location 1 23 24) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 24 25)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\r))
     #f
     #f
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\S)
      (character-token (location 1 13 14) (location 1 14 15) #\Y)
      (character-token (location 1 14 15) (location 1 15 16) #\S)
      (character-token (location 1 15 16) (location 1 16 17) #\T)
      (character-token (location 1 16 17) (location 1 17 18) #\E)
      (character-token (location 1 17 18) (location 1 18 19) #\M))
     (list
      (character-token (location 1 20 21) (location 1 21 22) #\j)
      (character-token (location 1 21 22) (location 1 22 23) #\o)
      (character-token (location 1 22 23) (location 1 23 24) #\k))
     (character-token (location 1 23 24) (location 1 24 25) #\>)
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) #\")))))
  (check-it
   "Null in single-quoted DOCTYPE system identifier"
   (format "<!DOCTYPE j PUBLIC \"foo\" 'b~ar'>" #\nul)
   #f
   (list
    (unexpected-null-character (location 1 27 28) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 31 32)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\j))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\P)
      (character-token (location 1 13 14) (location 1 14 15) #\U)
      (character-token (location 1 14 15) (location 1 15 16) #\B)
      (character-token (location 1 15 16) (location 1 16 17) #\L)
      (character-token (location 1 16 17) (location 1 17 18) #\I)
      (character-token (location 1 17 18) (location 1 18 19) #\C))
     (list
      (character-token (location 1 20 21) (location 1 21 22) #\f)
      (character-token (location 1 21 22) (location 1 22 23) #\o)
      (character-token (location 1 22 23) (location 1 23 24) #\o))
     #f
     (list
      (character-token (location 1 26 27) (location 1 27 28) #\b)
      (character-token (location 1 27 28) (location 1 28 29) '(#\nul . #\�))
      (character-token (location 1 28 29) (location 1 29 30) #\r))
     (character-token (location 1 30 31) (location 1 31 32) #\>)
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) #\")
      (character-token (location 1 23 24) (location 1 24 25) #\")
      (character-token (location 1 24 25) (location 1 25 26) #\space)
      (character-token (location 1 25 26) (location 1 26 27) #\')
      (character-token (location 1 29 30) (location 1 30 31) #\')))))
  (check-it
   "Null in double-quoted DOCTYPE system identifier"
   (format "<!DOCTYPE j PUBLIC \"foo\" \"b~ar\">" #\nul)
   #f
   (list
    (unexpected-null-character (location 1 27 28) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 31 32)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\j))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\P)
      (character-token (location 1 13 14) (location 1 14 15) #\U)
      (character-token (location 1 14 15) (location 1 15 16) #\B)
      (character-token (location 1 15 16) (location 1 16 17) #\L)
      (character-token (location 1 16 17) (location 1 17 18) #\I)
      (character-token (location 1 17 18) (location 1 18 19) #\C))
     (list
      (character-token (location 1 20 21) (location 1 21 22) #\f)
      (character-token (location 1 21 22) (location 1 22 23) #\o)
      (character-token (location 1 22 23) (location 1 23 24) #\o))
     #f
     (list
      (character-token (location 1 26 27) (location 1 27 28) #\b)
      (character-token (location 1 27 28) (location 1 28 29) '(#\nul . #\�))
      (character-token (location 1 28 29) (location 1 29 30) #\r))
     (character-token (location 1 30 31) (location 1 31 32) #\>)
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) #\")
      (character-token (location 1 23 24) (location 1 24 25) #\")
      (character-token (location 1 24 25) (location 1 25 26) #\space)
      (character-token (location 1 25 26) (location 1 26 27) #\")
      (character-token (location 1 29 30) (location 1 30 31) #\")))))
  (check-it
   "EOF in single-quoted DOCTYPE system identifier"
   "<!DOCTYPE y SYSTEM 'abc"
   #t
   (list
    (eof-in-doctype (location 1 23 24) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 23 24)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\y))
     #f
     #f
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\S)
      (character-token (location 1 13 14) (location 1 14 15) #\Y)
      (character-token (location 1 14 15) (location 1 15 16) #\S)
      (character-token (location 1 15 16) (location 1 16 17) #\T)
      (character-token (location 1 16 17) (location 1 17 18) #\E)
      (character-token (location 1 17 18) (location 1 18 19) #\M))
     (list
      (character-token (location 1 20 21) (location 1 21 22) #\a)
      (character-token (location 1 21 22) (location 1 22 23) #\b)
      (character-token (location 1 22 23) (location 1 23 24) #\c))
     #f
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) #\')))))
  (check-it
   "EOF in double-quoted DOCTYPE system identifier"
   "<!DOCTYPE y SYSTEM \"abc"
   #t
   (list
    (eof-in-doctype (location 1 23 24) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 23 24)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\y))
     #f
     #f
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\S)
      (character-token (location 1 13 14) (location 1 14 15) #\Y)
      (character-token (location 1 14 15) (location 1 15 16) #\S)
      (character-token (location 1 15 16) (location 1 16 17) #\T)
      (character-token (location 1 16 17) (location 1 17 18) #\E)
      (character-token (location 1 17 18) (location 1 18 19) #\M))
     (list
      (character-token (location 1 20 21) (location 1 21 22) #\a)
      (character-token (location 1 21 22) (location 1 22 23) #\b)
      (character-token (location 1 22 23) (location 1 23 24) #\c))
     #f
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) #\")))))
  (check-it
   "Abrupt end of single-quoted DOCTYPE public identifier"
   "<!DOCTYPE q PUBLIC 'what>"
   #t
   (list
    (abrupt-doctype-public-identifier (location 1 24 25) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 25 26)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\q))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\P)
      (character-token (location 1 13 14) (location 1 14 15) #\U)
      (character-token (location 1 14 15) (location 1 15 16) #\B)
      (character-token (location 1 15 16) (location 1 16 17) #\L)
      (character-token (location 1 16 17) (location 1 17 18) #\I)
      (character-token (location 1 17 18) (location 1 18 19) #\C))
     (list
      (character-token (location 1 20 21) (location 1 21 22) #\w)
      (character-token (location 1 21 22) (location 1 22 23) #\h)
      (character-token (location 1 22 23) (location 1 23 24) #\a)
      (character-token (location 1 23 24) (location 1 24 25) #\t))
     #f
     #f
     (character-token (location 1 24 25) (location 1 25 26) #\>)
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) #\')))))
  (check-it
   "Null character in single-quoted DOCTYPE public identifier"
   (format "<!DOCTYPE h PUBLIC '~a'>" #\nul)
   #f
   (list
    (unexpected-null-character (location 1 20 21) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 23 24)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\h))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\P)
      (character-token (location 1 13 14) (location 1 14 15) #\U)
      (character-token (location 1 14 15) (location 1 15 16) #\B)
      (character-token (location 1 15 16) (location 1 16 17) #\L)
      (character-token (location 1 16 17) (location 1 17 18) #\I)
      (character-token (location 1 17 18) (location 1 18 19) #\C))
     (list (character-token (location 1 20 21) (location 1 21 22) '(#\nul . #\�)))
     #f
     #f
     (character-token (location 1 22 23) (location 1 23 24) #\>)
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) #\')
      (character-token (location 1 21 22) (location 1 22 23) #\')))))
  (check-it
   "EOF in single-quoted DOCTYPE public identifier"
   "<!DOCTYPE l PUBLIC 'g"
   #t
   (list
    (eof-in-doctype (location 1 21 22) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 21 22)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\l))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\P)
      (character-token (location 1 13 14) (location 1 14 15) #\U)
      (character-token (location 1 14 15) (location 1 15 16) #\B)
      (character-token (location 1 15 16) (location 1 16 17) #\L)
      (character-token (location 1 16 17) (location 1 17 18) #\I)
      (character-token (location 1 17 18) (location 1 18 19) #\C))
     (list (character-token (location 1 20 21) (location 1 21 22) #\g))
     #f
     #f
     #f
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) #\')))))
  (check-it
   "Premature closing \">\" in double-quoted DOCTYPE public identifier"
   "<!DOCTYPE h PUBLIC \"ab>"
   #t
   (list
    (abrupt-doctype-public-identifier (location 1 22 23) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 23 24)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\h))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\P)
      (character-token (location 1 13 14) (location 1 14 15) #\U)
      (character-token (location 1 14 15) (location 1 15 16) #\B)
      (character-token (location 1 15 16) (location 1 16 17) #\L)
      (character-token (location 1 16 17) (location 1 17 18) #\I)
      (character-token (location 1 17 18) (location 1 18 19) #\C))
     (list
      (character-token (location 1 20 21) (location 1 21 22) #\a)
      (character-token (location 1 21 22) (location 1 22 23) #\b))
     #f
     #f
     (character-token (location 1 22 23) (location 1 23 24) #\>)
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) #\")))))
  (check-it
   "Null character in double-quoted DOCTYPE public identifier"
   (format "<!DOCTYPE z PUBLIC \"~a\"" #\nul)
   #t
   (list
    (unexpected-null-character (location 1 20 21) #f)
    (eof-in-doctype (location 1 22 23) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 22 23)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\z))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\P)
      (character-token (location 1 13 14) (location 1 14 15) #\U)
      (character-token (location 1 14 15) (location 1 15 16) #\B)
      (character-token (location 1 15 16) (location 1 16 17) #\L)
      (character-token (location 1 16 17) (location 1 17 18) #\I)
      (character-token (location 1 17 18) (location 1 18 19) #\C))
     (list (character-token (location 1 20 21) (location 1 21 22) '(#\nul . #\�)))
     #f
     #f
     #f
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) #\")
      (character-token (location 1 21 22) (location 1 22 23) #\")))))
  (check-it
   "EOF after double-quoted DOCTYPE public identifier"
   "<!DOCTYPE z PUBLIC \""
   #t
   (list
    (eof-in-doctype (location 1 20 21) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 20 21)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\z))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\P)
      (character-token (location 1 13 14) (location 1 14 15) #\U)
      (character-token (location 1 14 15) (location 1 15 16) #\B)
      (character-token (location 1 15 16) (location 1 16 17) #\L)
      (character-token (location 1 16 17) (location 1 17 18) #\I)
      (character-token (location 1 17 18) (location 1 18 19) #\C))
     '()
     #f
     #f
     #f
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) #\")))))
  (check-it
   "DOCTYPE goes bogus after public keyword"
   "<!DOCTYPE l publicly>"
   #t
   (list
    (missing-quote-before-doctype-public-identifier (location 1 18 19) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 21 22)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\l))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\p)
      (character-token (location 1 13 14) (location 1 14 15) #\u)
      (character-token (location 1 14 15) (location 1 15 16) #\b)
      (character-token (location 1 15 16) (location 1 16 17) #\l)
      (character-token (location 1 16 17) (location 1 17 18) #\i)
      (character-token (location 1 17 18) (location 1 18 19) #\c))
     #f
     #f
     #f
     (character-token (location 1 20 21) (location 1 21 22) #\>)
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) '(#\l . #f))
      (character-token (location 1 19 20) (location 1 20 21) '(#\y . #f))))))
  (check-it
   "DOCTYPE ends immediately after public keyword"
   "<!DOCTYPE l public>"
   #t
   (list
    (missing-doctype-public-identifier (location 1 18 19) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 19 20)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\l))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\p)
      (character-token (location 1 13 14) (location 1 14 15) #\u)
      (character-token (location 1 14 15) (location 1 15 16) #\b)
      (character-token (location 1 15 16) (location 1 16 17) #\l)
      (character-token (location 1 16 17) (location 1 17 18) #\i)
      (character-token (location 1 17 18) (location 1 18 19) #\c))
     #f
     #f
     #f
     (character-token (location 1 18 19) (location 1 19 20) #\>)
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)))))
  (check-it
   "Single quote immediately following DOCTYPE public keyword"
   "<!DOCTYPE d PUBLIC'l'>"
   #f
   (list
    (missing-whitespace-after-doctype-public-keyword (location 1 18 19) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 22 23)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\d))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\P)
      (character-token (location 1 13 14) (location 1 14 15) #\U)
      (character-token (location 1 14 15) (location 1 15 16) #\B)
      (character-token (location 1 15 16) (location 1 16 17) #\L)
      (character-token (location 1 16 17) (location 1 17 18) #\I)
      (character-token (location 1 17 18) (location 1 18 19) #\C))
     (list (character-token (location 1 19 20) (location 1 20 21) #\l))
     #f
     #f
     (character-token (location 1 21 22) (location 1 22 23) #\>)
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\')
      (character-token (location 1 20 21) (location 1 21 22) #\')))))
  (check-it
   "Double quote immediately following DOCTYPE public keyword"
   "<!DOCTYPE d PUBLIC\"l\">"
   #f
   (list
    (missing-whitespace-after-doctype-public-keyword (location 1 18 19) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 22 23)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\d))
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\P)
      (character-token (location 1 13 14) (location 1 14 15) #\U)
      (character-token (location 1 14 15) (location 1 15 16) #\B)
      (character-token (location 1 15 16) (location 1 16 17) #\L)
      (character-token (location 1 16 17) (location 1 17 18) #\I)
      (character-token (location 1 17 18) (location 1 18 19) #\C))
     (list (character-token (location 1 19 20) (location 1 20 21) #\l))
     #f
     #f
     (character-token (location 1 21 22) (location 1 22 23) #\>)
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\")
      (character-token (location 1 20 21) (location 1 21 22) #\")))))
  (check-it
   "No quote after DOCTYPE system keyword"
   "<!DOCTYPE k systemz>"
   #t
   (list
    (missing-quote-before-doctype-system-identifier (location 1 18 19) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 20 21)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\k))
     #f
     #f
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\s)
      (character-token (location 1 13 14) (location 1 14 15) #\y)
      (character-token (location 1 14 15) (location 1 15 16) #\s)
      (character-token (location 1 15 16) (location 1 16 17) #\t)
      (character-token (location 1 16 17) (location 1 17 18) #\e)
      (character-token (location 1 17 18) (location 1 18 19) #\m))
     #f
     (character-token (location 1 19 20) (location 1 20 21) #\>)
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) '(#\z . #f))))))
  (check-it
   "Single-quote immediately after DOCTYPE system keyword"
   "<!DOCTYPE k system'hi!'>"
   #f
   (list
    (missing-whitespace-after-doctype-system-keyword (location 1 18 19) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 24 25)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\k))
     #f
     #f
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\s)
      (character-token (location 1 13 14) (location 1 14 15) #\y)
      (character-token (location 1 14 15) (location 1 15 16) #\s)
      (character-token (location 1 15 16) (location 1 16 17) #\t)
      (character-token (location 1 16 17) (location 1 17 18) #\e)
      (character-token (location 1 17 18) (location 1 18 19) #\m))
     (list
      (character-token (location 1 19 20) (location 1 20 21) #\h)
      (character-token (location 1 20 21) (location 1 21 22) #\i)
      (character-token (location 1 21 22) (location 1 22 23) #\!))
     (character-token (location 1 23 24) (location 1 24 25) #\>)
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\')
      (character-token (location 1 22 23) (location 1 23 24) #\')))))
  (check-it
   "Double-quote immediately after DOCTYPE system keyword"
   "<!DOCTYPE k system\"hi!\">"
   #f
   (list
    (missing-whitespace-after-doctype-system-keyword (location 1 18 19) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 24 25)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\k))
     #f
     #f
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\s)
      (character-token (location 1 13 14) (location 1 14 15) #\y)
      (character-token (location 1 14 15) (location 1 15 16) #\s)
      (character-token (location 1 15 16) (location 1 16 17) #\t)
      (character-token (location 1 16 17) (location 1 17 18) #\e)
      (character-token (location 1 17 18) (location 1 18 19) #\m))
     (list
      (character-token (location 1 19 20) (location 1 20 21) #\h)
      (character-token (location 1 20 21) (location 1 21 22) #\i)
      (character-token (location 1 21 22) (location 1 22 23) #\!))
     (character-token (location 1 23 24) (location 1 24 25) #\>)
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\")
      (character-token (location 1 22 23) (location 1 23 24) #\")))))
  (check-it
   "EOF after DOCTYPE system keyword"
   "<!DOCTYPE g system"
   #t
   (list
    (eof-in-doctype (location 1 18 19) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 18 19)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\g))
     #f
     #f
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\s)
      (character-token (location 1 13 14) (location 1 14 15) #\y)
      (character-token (location 1 14 15) (location 1 15 16) #\s)
      (character-token (location 1 15 16) (location 1 16 17) #\t)
      (character-token (location 1 16 17) (location 1 17 18) #\e)
      (character-token (location 1 17 18) (location 1 18 19) #\m))
     #f
     #f
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)))))
  (check-it
   "Null character in bogus DOCTYPE"
   (format "<!DOCTYPE l ~ap>" #\nul)
   #t
   (list
    (invalid-character-sequence-after-doctype-name (location 1 12 13) #f)
    (unexpected-null-character (location 1 12 13) #f)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\l))
     #f
     #f
     #f
     #f
     (character-token (location 1 14 15) (location 1 15 16) #\>)
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 12 13) (location 1 13 14) '(#\nul . #f))
      (character-token (location 1 13 14) (location 1 14 15) '(#\p . #f))))))
  (check-it
   "EOF in bogus DOCTYPE"
   "<!DOCTYPE z z z"
   #t
   (list
    (invalid-character-sequence-after-doctype-name (location 1 12 13) #f)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\z))
     #f
     #f
     #f
     #f
     #f
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 12 13) (location 1 13 14) '(#\z . #f))
      (character-token (location 1 13 14) (location 1 14 15) '(#\space . #f))
      (character-token (location 1 14 15) (location 1 15 16) '(#\z . #f))))))
  (check-it
   "Two whitespace chars after DOCTYPE name"
   (format "<!DOCTYPE html ~a >" #\tab)
   #f
   (list
    (doctype-token
     (location 1 0 1)
     (location 1 18 19)
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
     (character-token (location 1 17 18) (location 1 18 19) #\>)
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 14 15) (location 1 15 16) #\space)
      (character-token (location 1 15 16) (location 1 16 17) #\tab)
      (character-token (location 1 16 17) (location 1 17 18) #\space)))))
  (check-it
   "EOF after DOCTYPE name"
   "<!DOCTYPE html "
   #t
   (list
    (eof-in-doctype (location 1 15 16) #f)
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
     #f
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 14 15) (location 1 15 16) #\space)))))
  (check-it
   "Null character in DOCTYPE name"
   (format "<!DOCTYPE h~a>" #\nul)
   #f
   (list
    (unexpected-null-character (location 1 12 13) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 13 14)
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
      (character-token (location 1 11 12) (location 1 12 13) '(#\nul . #\�)))
     #f
     #f
     #f
     #f
     (character-token (location 1 12 13) (location 1 13 14) #\>)
     #f
     (list (character-token (location 1 9 10) (location 1 10 11) #\space)))))
  (check-it
   "EOF in DOCTYPE name"
   "<!DOCTYPE h"
   #t
   (list
    (eof-in-doctype (location 1 11 12) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 11 12)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\h))
     #f
     #f
     #f
     #f
     #f
     #t
     (list (character-token (location 1 9 10) (location 1 10 11) #\space)))))
  (check-it
   "DOCTYPE with single-quoted system identifier"
   "<!DOCTYPE h SYSTEM 'hi'>"
   #f
   (list
    (doctype-token
     (location 1 0 1)
     (location 1 24 25)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\h))
     #f
     #f
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\S)
      (character-token (location 1 13 14) (location 1 14 15) #\Y)
      (character-token (location 1 14 15) (location 1 15 16) #\S)
      (character-token (location 1 15 16) (location 1 16 17) #\T)
      (character-token (location 1 16 17) (location 1 17 18) #\E)
      (character-token (location 1 17 18) (location 1 18 19) #\M))
     (list
      (character-token (location 1 20 21) (location 1 21 22) #\h)
      (character-token (location 1 21 22) (location 1 22 23) #\i))
     (character-token (location 1 23 24) (location 1 24 25) #\>)
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 19 20) (location 1 20 21) #\')
      (character-token (location 1 22 23) (location 1 23 24) #\')))))
  (check-it
   "DOCTYPE with multiple whitespace after system keyword (no value)"
   (format "<!DOCTYPE h SYSTEM ~a" #\tab)
   #t
   (list
    (eof-in-doctype (location 1 24 21) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 24 21)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\h))
     #f
     #f
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\S)
      (character-token (location 1 13 14) (location 1 14 15) #\Y)
      (character-token (location 1 14 15) (location 1 15 16) #\S)
      (character-token (location 1 15 16) (location 1 16 17) #\T)
      (character-token (location 1 16 17) (location 1 17 18) #\E)
      (character-token (location 1 17 18) (location 1 18 19) #\M))
     #f
     #f
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)
      (character-token (location 1 23 20) (location 1 24 21) #\tab)))))
  (check-it
   "DOCTYPE with name, system keyword, space, then EOF"
   "<!DOCTYPE h SYSTEM "
   #t
   (list
    (eof-in-doctype (location 1 19 20) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 19 20)
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
     (list (character-token (location 1 10 11) (location 1 11 12) #\h))
     #f
     #f
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\S)
      (character-token (location 1 13 14) (location 1 14 15) #\Y)
      (character-token (location 1 14 15) (location 1 15 16) #\S)
      (character-token (location 1 15 16) (location 1 16 17) #\T)
      (character-token (location 1 16 17) (location 1 17 18) #\E)
      (character-token (location 1 17 18) (location 1 18 19) #\M))
     #f
     #f
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 18 19) (location 1 19 20) #\space)))))
  (check-it
   "DOCTYPE with a name, space, then junk ended by \">\""
   "<!DOCTYPE html jz>"
   #t
   (list
    (invalid-character-sequence-after-doctype-name (location 1 15 16) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 18 19)
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
     (character-token (location 1 17 18) (location 1 18 19) #\>)
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 14 15) (location 1 15 16) #\space)
      (character-token (location 1 15 16) (location 1 16 17) '(#\j . #f))
      (character-token (location 1 16 17) (location 1 17 18) '(#\z . #f))))))
  (check-it
   "DOCTYPE with no space after \"doctype\", just more characters, then \">\""
   "<!DOCTYPEhtml>"
   #f
   (list
    (missing-whitespace-before-doctype-name (location 1 9 10) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 14 15)
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
      (character-token (location 1 9 10) (location 1 10 11) #\h)
      (character-token (location 1 10 11) (location 1 11 12) #\t)
      (character-token (location 1 11 12) (location 1 12 13) #\m)
      (character-token (location 1 12 13) (location 1 13 14) #\l))
     #f
     #f
     #f
     #f
     (character-token (location 1 13 14) (location 1 14 15) #\>)
     #f
     '())))
  (check-it
   "DOCTYPE with lowercase name, then a closing >"
   "<!DOCTYPE html>"
   #f
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
   "DOCTYPE with all-caps name (needs to be downcased)"
   "<!DOCTYPE HTML>"
   #f
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
      (character-token (location 1 10 11) (location 1 11 12) '(#\H . #\h))
      (character-token (location 1 11 12) (location 1 12 13) '(#\T . #\t))
      (character-token (location 1 12 13) (location 1 13 14) '(#\M . #\m))
      (character-token (location 1 13 14) (location 1 14 15) '(#\L . #\l)))
     #f
     #f
     #f
     #f
     (character-token (location 1 14 15) (location 1 15 16) #\>)
     #f
     (list (character-token (location 1 9 10) (location 1 10 11) #\space)))))
  (check-it
   "DOCTYPE with null character in its name"
   (format "<!DOCTYPE ~ahtml>" #\u0000)
   #f
   (list
    (unexpected-null-character (location 1 11 12) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 16 17)
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
      (character-token (location 1 10 11) (location 1 11 12) '(#\nul . #\�))
      (character-token (location 1 11 12) (location 1 12 13) #\h)
      (character-token (location 1 12 13) (location 1 13 14) #\t)
      (character-token (location 1 13 14) (location 1 14 15) #\m)
      (character-token (location 1 14 15) (location 1 15 16) #\l))
     #f
     #f
     #f
     #f
     (character-token (location 1 15 16) (location 1 16 17) #\>)
     #f
     (list (character-token (location 1 9 10) (location 1 10 11) #\space)))))
  (check-it
   "DOCTYPE immediately followed by EOF (no space, no closing >)"
   "<!DOCTYPE"
   #t
   (list
    (eof-in-doctype (location 1 9 10) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 9 10)
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
     '()
     #f
     #f
     #f
     #f
     #f
     #t
     '())))
  (check-it
   "DOCTYPE followed by one space, then EOF (no closing >)"
   "<!DOCTYPE "
   #t
   (list
    (eof-in-doctype (location 1 10 11) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 10 11)
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
     '()
     #f
     #f
     #f
     #f
     #f
     #t
     (list (character-token (location 1 9 10) (location 1 10 11) #\space)))))
  (check-it
   "DOCTYPE followed by two spaces, then EOF (no closing >)"
   "<!DOCTYPE  "
   #t
   (list
    (eof-in-doctype (location 1 11 12) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 11 12)
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
     '()
     #f
     #f
     #f
     #f
     #f
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 10 11) (location 1 11 12) #\space)))))
  (check-it
   "DOCTYPE followed by > (no public, no system, just >)"
   "<!DOCTYPE>"
   #t
   (list
    (missing-doctype-name (location 1 10 11) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 10 11)
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
     '()
     #f
     #f
     #f
     #f
     (character-token (location 1 9 10) (location 1 10 11) #\>)
     #t
     '())))
  (check-it
   "DOCTYPE real-world example: public identifier with system name (XHTML 1.0 Transitional)"
   #<<DOCTYPE
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
DOCTYPE
   #f
   (list
    (doctype-token
     (location 1 0 1)
     (location 1 121 122)
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
     (list
      (character-token (location 1 15 16) (location 1 16 17) #\P)
      (character-token (location 1 16 17) (location 1 17 18) #\U)
      (character-token (location 1 17 18) (location 1 18 19) #\B)
      (character-token (location 1 18 19) (location 1 19 20) #\L)
      (character-token (location 1 19 20) (location 1 20 21) #\I)
      (character-token (location 1 20 21) (location 1 21 22) #\C))
     (list
      (character-token (location 1 23 24) (location 1 24 25) #\-)
      (character-token (location 1 24 25) (location 1 25 26) #\/)
      (character-token (location 1 25 26) (location 1 26 27) #\/)
      (character-token (location 1 26 27) (location 1 27 28) #\W)
      (character-token (location 1 27 28) (location 1 28 29) #\3)
      (character-token (location 1 28 29) (location 1 29 30) #\C)
      (character-token (location 1 29 30) (location 1 30 31) #\/)
      (character-token (location 1 30 31) (location 1 31 32) #\/)
      (character-token (location 1 31 32) (location 1 32 33) #\D)
      (character-token (location 1 32 33) (location 1 33 34) #\T)
      (character-token (location 1 33 34) (location 1 34 35) #\D)
      (character-token (location 1 34 35) (location 1 35 36) #\space)
      (character-token (location 1 35 36) (location 1 36 37) #\X)
      (character-token (location 1 36 37) (location 1 37 38) #\H)
      (character-token (location 1 37 38) (location 1 38 39) #\T)
      (character-token (location 1 38 39) (location 1 39 40) #\M)
      (character-token (location 1 39 40) (location 1 40 41) #\L)
      (character-token (location 1 40 41) (location 1 41 42) #\space)
      (character-token (location 1 41 42) (location 1 42 43) #\1)
      (character-token (location 1 42 43) (location 1 43 44) #\.)
      (character-token (location 1 43 44) (location 1 44 45) #\0)
      (character-token (location 1 44 45) (location 1 45 46) #\space)
      (character-token (location 1 45 46) (location 1 46 47) #\T)
      (character-token (location 1 46 47) (location 1 47 48) #\r)
      (character-token (location 1 47 48) (location 1 48 49) #\a)
      (character-token (location 1 48 49) (location 1 49 50) #\n)
      (character-token (location 1 49 50) (location 1 50 51) #\s)
      (character-token (location 1 50 51) (location 1 51 52) #\i)
      (character-token (location 1 51 52) (location 1 52 53) #\t)
      (character-token (location 1 52 53) (location 1 53 54) #\i)
      (character-token (location 1 53 54) (location 1 54 55) #\o)
      (character-token (location 1 54 55) (location 1 55 56) #\n)
      (character-token (location 1 55 56) (location 1 56 57) #\a)
      (character-token (location 1 56 57) (location 1 57 58) #\l)
      (character-token (location 1 57 58) (location 1 58 59) #\/)
      (character-token (location 1 58 59) (location 1 59 60) #\/)
      (character-token (location 1 59 60) (location 1 60 61) #\E)
      (character-token (location 1 60 61) (location 1 61 62) #\N))
     #f
     (list
      (character-token (location 1 64 65) (location 1 65 66) #\h)
      (character-token (location 1 65 66) (location 1 66 67) #\t)
      (character-token (location 1 66 67) (location 1 67 68) #\t)
      (character-token (location 1 67 68) (location 1 68 69) #\p)
      (character-token (location 1 68 69) (location 1 69 70) #\:)
      (character-token (location 1 69 70) (location 1 70 71) #\/)
      (character-token (location 1 70 71) (location 1 71 72) #\/)
      (character-token (location 1 71 72) (location 1 72 73) #\w)
      (character-token (location 1 72 73) (location 1 73 74) #\w)
      (character-token (location 1 73 74) (location 1 74 75) #\w)
      (character-token (location 1 74 75) (location 1 75 76) #\.)
      (character-token (location 1 75 76) (location 1 76 77) #\w)
      (character-token (location 1 76 77) (location 1 77 78) #\3)
      (character-token (location 1 77 78) (location 1 78 79) #\.)
      (character-token (location 1 78 79) (location 1 79 80) #\o)
      (character-token (location 1 79 80) (location 1 80 81) #\r)
      (character-token (location 1 80 81) (location 1 81 82) #\g)
      (character-token (location 1 81 82) (location 1 82 83) #\/)
      (character-token (location 1 82 83) (location 1 83 84) #\T)
      (character-token (location 1 83 84) (location 1 84 85) #\R)
      (character-token (location 1 84 85) (location 1 85 86) #\/)
      (character-token (location 1 85 86) (location 1 86 87) #\x)
      (character-token (location 1 86 87) (location 1 87 88) #\h)
      (character-token (location 1 87 88) (location 1 88 89) #\t)
      (character-token (location 1 88 89) (location 1 89 90) #\m)
      (character-token (location 1 89 90) (location 1 90 91) #\l)
      (character-token (location 1 90 91) (location 1 91 92) #\1)
      (character-token (location 1 91 92) (location 1 92 93) #\/)
      (character-token (location 1 92 93) (location 1 93 94) #\D)
      (character-token (location 1 93 94) (location 1 94 95) #\T)
      (character-token (location 1 94 95) (location 1 95 96) #\D)
      (character-token (location 1 95 96) (location 1 96 97) #\/)
      (character-token (location 1 96 97) (location 1 97 98) #\x)
      (character-token (location 1 97 98) (location 1 98 99) #\h)
      (character-token (location 1 98 99) (location 1 99 100) #\t)
      (character-token (location 1 99 100) (location 1 100 101) #\m)
      (character-token (location 1 100 101) (location 1 101 102) #\l)
      (character-token (location 1 101 102) (location 1 102 103) #\1)
      (character-token (location 1 102 103) (location 1 103 104) #\-)
      (character-token (location 1 103 104) (location 1 104 105) #\t)
      (character-token (location 1 104 105) (location 1 105 106) #\r)
      (character-token (location 1 105 106) (location 1 106 107) #\a)
      (character-token (location 1 106 107) (location 1 107 108) #\n)
      (character-token (location 1 107 108) (location 1 108 109) #\s)
      (character-token (location 1 108 109) (location 1 109 110) #\i)
      (character-token (location 1 109 110) (location 1 110 111) #\t)
      (character-token (location 1 110 111) (location 1 111 112) #\i)
      (character-token (location 1 111 112) (location 1 112 113) #\o)
      (character-token (location 1 112 113) (location 1 113 114) #\n)
      (character-token (location 1 113 114) (location 1 114 115) #\a)
      (character-token (location 1 114 115) (location 1 115 116) #\l)
      (character-token (location 1 115 116) (location 1 116 117) #\.)
      (character-token (location 1 116 117) (location 1 117 118) #\d)
      (character-token (location 1 117 118) (location 1 118 119) #\t)
      (character-token (location 1 118 119) (location 1 119 120) #\d))
     (character-token (location 1 120 121) (location 1 121 122) #\>)
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 14 15) (location 1 15 16) #\space)
      (character-token (location 1 21 22) (location 1 22 23) #\space)
      (character-token (location 1 22 23) (location 1 23 24) #\")
      (character-token (location 1 61 62) (location 1 62 63) #\")
      (character-token (location 1 62 63) (location 1 63 64) #\space)
      (character-token (location 1 63 64) (location 1 64 65) #\")
      (character-token (location 1 119 120) (location 1 120 121) #\")))))
  (check-it
   "DOCTYPE name, public keyword, and double-quoted value present, but then EOF (no closing >)"
   "<!DOCTYPE html PUBLIC \"foo\" "
   #t
   (list
    (eof-in-doctype (location 1 28 29) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 28 29)
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
     (list
      (character-token (location 1 15 16) (location 1 16 17) #\P)
      (character-token (location 1 16 17) (location 1 17 18) #\U)
      (character-token (location 1 17 18) (location 1 18 19) #\B)
      (character-token (location 1 18 19) (location 1 19 20) #\L)
      (character-token (location 1 19 20) (location 1 20 21) #\I)
      (character-token (location 1 20 21) (location 1 21 22) #\C))
     (list
      (character-token (location 1 23 24) (location 1 24 25) #\f)
      (character-token (location 1 24 25) (location 1 25 26) #\o)
      (character-token (location 1 25 26) (location 1 26 27) #\o))
     #f
     #f
     #f
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 14 15) (location 1 15 16) #\space)
      (character-token (location 1 21 22) (location 1 22 23) #\space)
      (character-token (location 1 22 23) (location 1 23 24) #\")
      (character-token (location 1 26 27) (location 1 27 28) #\")
      (character-token (location 1 27 28) (location 1 28 29) #\space)))))
  (check-it
   "DOCTYPE name and public keyword and double-quoted value"
   "<!DOCTYPE html PUBLIC \"foo\">"
   #f
   (list
    (doctype-token
     (location 1 0 1)
     (location 1 28 29)
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
     (list
      (character-token (location 1 15 16) (location 1 16 17) #\P)
      (character-token (location 1 16 17) (location 1 17 18) #\U)
      (character-token (location 1 17 18) (location 1 18 19) #\B)
      (character-token (location 1 18 19) (location 1 19 20) #\L)
      (character-token (location 1 19 20) (location 1 20 21) #\I)
      (character-token (location 1 20 21) (location 1 21 22) #\C))
     (list
      (character-token (location 1 23 24) (location 1 24 25) #\f)
      (character-token (location 1 24 25) (location 1 25 26) #\o)
      (character-token (location 1 25 26) (location 1 26 27) #\o))
     #f
     #f
     (character-token (location 1 27 28) (location 1 28 29) #\>)
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 14 15) (location 1 15 16) #\space)
      (character-token (location 1 21 22) (location 1 22 23) #\space)
      (character-token (location 1 22 23) (location 1 23 24) #\")
      (character-token (location 1 26 27) (location 1 27 28) #\")))))
  (check-it
   "DOCTYPE name and public keyword present, with double-quoted value, followed by space and >"
   "<!DOCTYPE html PUBLIC \"foo\" >"
   #f
   (list
    (doctype-token
     (location 1 0 1)
     (location 1 29 30)
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
     (list
      (character-token (location 1 15 16) (location 1 16 17) #\P)
      (character-token (location 1 16 17) (location 1 17 18) #\U)
      (character-token (location 1 17 18) (location 1 18 19) #\B)
      (character-token (location 1 18 19) (location 1 19 20) #\L)
      (character-token (location 1 19 20) (location 1 20 21) #\I)
      (character-token (location 1 20 21) (location 1 21 22) #\C))
     (list
      (character-token (location 1 23 24) (location 1 24 25) #\f)
      (character-token (location 1 24 25) (location 1 25 26) #\o)
      (character-token (location 1 25 26) (location 1 26 27) #\o))
     #f
     #f
     (character-token (location 1 28 29) (location 1 29 30) #\>)
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 14 15) (location 1 15 16) #\space)
      (character-token (location 1 21 22) (location 1 22 23) #\space)
      (character-token (location 1 22 23) (location 1 23 24) #\")
      (character-token (location 1 26 27) (location 1 27 28) #\")
      (character-token (location 1 27 28) (location 1 28 29) #\space)))))
  (check-it
   "DOCTYPE name and public keyword present, then a single-quoted value"
   "<!DOCTYPE html PUBLIC \"foo\" 'bar'>"
   #f
   (list
    (doctype-token
     (location 1 0 1)
     (location 1 34 35)
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
     (list
      (character-token (location 1 15 16) (location 1 16 17) #\P)
      (character-token (location 1 16 17) (location 1 17 18) #\U)
      (character-token (location 1 17 18) (location 1 18 19) #\B)
      (character-token (location 1 18 19) (location 1 19 20) #\L)
      (character-token (location 1 19 20) (location 1 20 21) #\I)
      (character-token (location 1 20 21) (location 1 21 22) #\C))
     (list
      (character-token (location 1 23 24) (location 1 24 25) #\f)
      (character-token (location 1 24 25) (location 1 25 26) #\o)
      (character-token (location 1 25 26) (location 1 26 27) #\o))
     #f
     (list
      (character-token (location 1 29 30) (location 1 30 31) #\b)
      (character-token (location 1 30 31) (location 1 31 32) #\a)
      (character-token (location 1 31 32) (location 1 32 33) #\r))
     (character-token (location 1 33 34) (location 1 34 35) #\>)
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 14 15) (location 1 15 16) #\space)
      (character-token (location 1 21 22) (location 1 22 23) #\space)
      (character-token (location 1 22 23) (location 1 23 24) #\")
      (character-token (location 1 26 27) (location 1 27 28) #\")
      (character-token (location 1 27 28) (location 1 28 29) #\space)
      (character-token (location 1 28 29) (location 1 29 30) #\')
      (character-token (location 1 32 33) (location 1 33 34) #\')))))
  (check-it
   "DOCTYPE name and public keyword present, followed by a double-quoted value (system identifier), and another double-quoted value"
   "<!DOCTYPE html PUBLIC \"foo\"\"bar\">"
   #f
   (list
    (missing-whitespace-between-doctype-public-and-system-identifiers
     (location 1 27 28)
     #f)
    (doctype-token
     (location 1 0 1)
     (location 1 33 34)
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
     (list
      (character-token (location 1 15 16) (location 1 16 17) #\P)
      (character-token (location 1 16 17) (location 1 17 18) #\U)
      (character-token (location 1 17 18) (location 1 18 19) #\B)
      (character-token (location 1 18 19) (location 1 19 20) #\L)
      (character-token (location 1 19 20) (location 1 20 21) #\I)
      (character-token (location 1 20 21) (location 1 21 22) #\C))
     (list
      (character-token (location 1 23 24) (location 1 24 25) #\f)
      (character-token (location 1 24 25) (location 1 25 26) #\o)
      (character-token (location 1 25 26) (location 1 26 27) #\o))
     #f
     (list
      (character-token (location 1 28 29) (location 1 29 30) #\b)
      (character-token (location 1 29 30) (location 1 30 31) #\a)
      (character-token (location 1 30 31) (location 1 31 32) #\r))
     (character-token (location 1 32 33) (location 1 33 34) #\>)
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 14 15) (location 1 15 16) #\space)
      (character-token (location 1 21 22) (location 1 22 23) #\space)
      (character-token (location 1 22 23) (location 1 23 24) #\")
      (character-token (location 1 26 27) (location 1 27 28) #\")
      (character-token (location 1 27 28) (location 1 28 29) #\")
      (character-token (location 1 31 32) (location 1 32 33) #\")))))
  (check-it
   "DOCTYPE name and public keyword present, with double-quoted value, followed by junk and >"
   "<!DOCTYPE html PUBLIC \"foo\" bar>"
   #t
   (list
    (missing-quote-before-doctype-system-identifier (location 1 28 29) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 32 33)
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
     (list
      (character-token (location 1 15 16) (location 1 16 17) #\P)
      (character-token (location 1 16 17) (location 1 17 18) #\U)
      (character-token (location 1 17 18) (location 1 18 19) #\B)
      (character-token (location 1 18 19) (location 1 19 20) #\L)
      (character-token (location 1 19 20) (location 1 20 21) #\I)
      (character-token (location 1 20 21) (location 1 21 22) #\C))
     (list
      (character-token (location 1 23 24) (location 1 24 25) #\f)
      (character-token (location 1 24 25) (location 1 25 26) #\o)
      (character-token (location 1 25 26) (location 1 26 27) #\o))
     #f
     #f
     (character-token (location 1 31 32) (location 1 32 33) #\>)
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 14 15) (location 1 15 16) #\space)
      (character-token (location 1 21 22) (location 1 22 23) #\space)
      (character-token (location 1 22 23) (location 1 23 24) #\")
      (character-token (location 1 26 27) (location 1 27 28) #\")
      (character-token (location 1 27 28) (location 1 28 29) #\space)
      (character-token (location 1 28 29) (location 1 29 30) '(#\b . #f))
      (character-token (location 1 29 30) (location 1 30 31) '(#\a . #f))
      (character-token (location 1 30 31) (location 1 31 32) '(#\r . #f))))))
  (check-it
   "DOCTYPE public keyword present, followed by EOF"
   "<!DOCTYPE html PUBLIC"
   #t
   (list
    (eof-in-doctype (location 1 21 22) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 21 22)
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
     (list
      (character-token (location 1 15 16) (location 1 16 17) #\P)
      (character-token (location 1 16 17) (location 1 17 18) #\U)
      (character-token (location 1 17 18) (location 1 18 19) #\B)
      (character-token (location 1 18 19) (location 1 19 20) #\L)
      (character-token (location 1 19 20) (location 1 20 21) #\I)
      (character-token (location 1 20 21) (location 1 21 22) #\C))
     #f
     #f
     #f
     #f
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 14 15) (location 1 15 16) #\space)))))
  (check-it
   "DOCTYPE name and system keyword present, with double-quoted value"
   "<!DOCTYPE frag SYSTEM \"http://example.com\">"
   #f
   (list
    (doctype-token
     (location 1 0 1)
     (location 1 43 44)
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
      (character-token (location 1 10 11) (location 1 11 12) #\f)
      (character-token (location 1 11 12) (location 1 12 13) #\r)
      (character-token (location 1 12 13) (location 1 13 14) #\a)
      (character-token (location 1 13 14) (location 1 14 15) #\g))
     #f
     #f
     (list
      (character-token (location 1 15 16) (location 1 16 17) #\S)
      (character-token (location 1 16 17) (location 1 17 18) #\Y)
      (character-token (location 1 17 18) (location 1 18 19) #\S)
      (character-token (location 1 18 19) (location 1 19 20) #\T)
      (character-token (location 1 19 20) (location 1 20 21) #\E)
      (character-token (location 1 20 21) (location 1 21 22) #\M))
     (list
      (character-token (location 1 23 24) (location 1 24 25) #\h)
      (character-token (location 1 24 25) (location 1 25 26) #\t)
      (character-token (location 1 25 26) (location 1 26 27) #\t)
      (character-token (location 1 26 27) (location 1 27 28) #\p)
      (character-token (location 1 27 28) (location 1 28 29) #\:)
      (character-token (location 1 28 29) (location 1 29 30) #\/)
      (character-token (location 1 29 30) (location 1 30 31) #\/)
      (character-token (location 1 30 31) (location 1 31 32) #\e)
      (character-token (location 1 31 32) (location 1 32 33) #\x)
      (character-token (location 1 32 33) (location 1 33 34) #\a)
      (character-token (location 1 33 34) (location 1 34 35) #\m)
      (character-token (location 1 34 35) (location 1 35 36) #\p)
      (character-token (location 1 35 36) (location 1 36 37) #\l)
      (character-token (location 1 36 37) (location 1 37 38) #\e)
      (character-token (location 1 37 38) (location 1 38 39) #\.)
      (character-token (location 1 38 39) (location 1 39 40) #\c)
      (character-token (location 1 39 40) (location 1 40 41) #\o)
      (character-token (location 1 40 41) (location 1 41 42) #\m))
     (character-token (location 1 42 43) (location 1 43 44) #\>)
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 14 15) (location 1 15 16) #\space)
      (character-token (location 1 21 22) (location 1 22 23) #\space)
      (character-token (location 1 22 23) (location 1 23 24) #\")
      (character-token (location 1 41 42) (location 1 42 43) #\")))))
  (check-it
   "DOCTYPE name and SYSTEM keyword present, but without value (> after SYSTEM)"
   "<!DOCTYPE frag SYSTEM>"
   #t
   (list
    (missing-doctype-system-identifier (location 1 21 22) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 22 23)
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
      (character-token (location 1 10 11) (location 1 11 12) #\f)
      (character-token (location 1 11 12) (location 1 12 13) #\r)
      (character-token (location 1 12 13) (location 1 13 14) #\a)
      (character-token (location 1 13 14) (location 1 14 15) #\g))
     #f
     #f
     (list
      (character-token (location 1 15 16) (location 1 16 17) #\S)
      (character-token (location 1 16 17) (location 1 17 18) #\Y)
      (character-token (location 1 17 18) (location 1 18 19) #\S)
      (character-token (location 1 18 19) (location 1 19 20) #\T)
      (character-token (location 1 19 20) (location 1 20 21) #\E)
      (character-token (location 1 20 21) (location 1 21 22) #\M))
     #f
     (character-token (location 1 21 22) (location 1 22 23) #\>)
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 14 15) (location 1 15 16) #\space)))))
  (check-it
   "DOCTYPE name and system keyword present, followed by whitespace and >"
   "<!DOCTYPE frag SYSTEM >"
   #t
   (list
   (missing-doctype-system-identifier (location 1 22 23) #f)
   (doctype-token
    (location 1 0 1)
    (location 1 23 24)
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
     (character-token (location 1 10 11) (location 1 11 12) #\f)
     (character-token (location 1 11 12) (location 1 12 13) #\r)
     (character-token (location 1 12 13) (location 1 13 14) #\a)
     (character-token (location 1 13 14) (location 1 14 15) #\g))
    #f
    #f
    (list
     (character-token (location 1 15 16) (location 1 16 17) #\S)
     (character-token (location 1 16 17) (location 1 17 18) #\Y)
     (character-token (location 1 17 18) (location 1 18 19) #\S)
     (character-token (location 1 18 19) (location 1 19 20) #\T)
     (character-token (location 1 19 20) (location 1 20 21) #\E)
     (character-token (location 1 20 21) (location 1 21 22) #\M))
    #f
    (character-token (location 1 22 23) (location 1 23 24) #\>)
    #t
    (list
     (character-token (location 1 9 10) (location 1 10 11) #\space)
     (character-token (location 1 14 15) (location 1 15 16) #\space)
     (character-token (location 1 21 22) (location 1 22 23) #\space)))))
  (check-it
   "DOCTYPE name & system keyword present, unquoted"
   "<!DOCTYPE frag SYSTEM http://example.com>"
   #t
   (list
    (missing-quote-before-doctype-system-identifier (location 1 22 23) #f)
    (doctype-token
     (location 1 0 1)
     (location 1 41 42)
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
      (character-token (location 1 10 11) (location 1 11 12) #\f)
      (character-token (location 1 11 12) (location 1 12 13) #\r)
      (character-token (location 1 12 13) (location 1 13 14) #\a)
      (character-token (location 1 13 14) (location 1 14 15) #\g))
     #f
     #f
     (list
      (character-token (location 1 15 16) (location 1 16 17) #\S)
      (character-token (location 1 16 17) (location 1 17 18) #\Y)
      (character-token (location 1 17 18) (location 1 18 19) #\S)
      (character-token (location 1 18 19) (location 1 19 20) #\T)
      (character-token (location 1 19 20) (location 1 20 21) #\E)
      (character-token (location 1 20 21) (location 1 21 22) #\M))
     #f
     (character-token (location 1 40 41) (location 1 41 42) #\>)
     #t
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 14 15) (location 1 15 16) #\space)
      (character-token (location 1 21 22) (location 1 22 23) #\space)
      (character-token (location 1 22 23) (location 1 23 24) '(#\h . #f))
      (character-token (location 1 23 24) (location 1 24 25) '(#\t . #f))
      (character-token (location 1 24 25) (location 1 25 26) '(#\t . #f))
      (character-token (location 1 25 26) (location 1 26 27) '(#\p . #f))
      (character-token (location 1 26 27) (location 1 27 28) '(#\: . #f))
      (character-token (location 1 27 28) (location 1 28 29) '(#\/ . #f))
      (character-token (location 1 28 29) (location 1 29 30) '(#\/ . #f))
      (character-token (location 1 29 30) (location 1 30 31) '(#\e . #f))
      (character-token (location 1 30 31) (location 1 31 32) '(#\x . #f))
      (character-token (location 1 31 32) (location 1 32 33) '(#\a . #f))
      (character-token (location 1 32 33) (location 1 33 34) '(#\m . #f))
      (character-token (location 1 33 34) (location 1 34 35) '(#\p . #f))
      (character-token (location 1 34 35) (location 1 35 36) '(#\l . #f))
      (character-token (location 1 35 36) (location 1 36 37) '(#\e . #f))
      (character-token (location 1 36 37) (location 1 37 38) '(#\. . #f))
      (character-token (location 1 37 38) (location 1 38 39) '(#\c . #f))
      (character-token (location 1 38 39) (location 1 39 40) '(#\o . #f))
      (character-token (location 1 39 40) (location 1 40 41) '(#\m . #f)))))))
