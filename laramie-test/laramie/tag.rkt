#lang racket/base

(require laramie
         (file "util.rkt"))

(module+ test
  (require rackunit
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
   "Open tag, then EOF"
   "<"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (eof-before-tag-name (location 1 1 2) #f)))
  (check-it
   "Invalid start of a tag"
   "<{"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (invalid-first-character-of-tag-name
     (location 1 1 2)
     (character-token (location 1 1 2) (location 1 2 3) #\{))))
  (check-it
   "Attribute with no value"
   "<html lang>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 11 12)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list
      (character-token (location 1 1 2) (location 1 2 3) #\h)
      (character-token (location 1 2 3) (location 1 3 4) #\t)
      (character-token (location 1 3 4) (location 1 4 5) #\m)
      (character-token (location 1 4 5) (location 1 5 6) #\l))
     (list
      (attribute-token
       (location 1 5 6)
       (location 1 11 12)
       (list
        (character-token (location 1 6 7) (location 1 7 8) #\l)
        (character-token (location 1 7 8) (location 1 8 9) #\a)
        (character-token (location 1 8 9) (location 1 9 10) #\n)
        (character-token (location 1 9 10) (location 1 10 11) #\g))
       #f
       #f))
     #f
     (character-token (location 1 10 11) (location 1 11 12) #\>)
     (list (character-token (location 1 5 6) (location 1 6 7) #\space)))))
  (check-it
   "Double-quoted attribute"
   "<p class=\"hi\">"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 14 15)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 14 15)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\c)
        (character-token (location 1 4 5) (location 1 5 6) #\l)
        (character-token (location 1 5 6) (location 1 6 7) #\a)
        (character-token (location 1 6 7) (location 1 7 8) #\s)
        (character-token (location 1 7 8) (location 1 8 9) #\s))
       (character-token (location 1 8 9) (location 1 9 10) #\=)
       (quoted-attr-value
        (character-token (location 1 9 10) (location 1 10 11) #\")
        (list
         (character-token (location 1 10 11) (location 1 11 12) #\h)
         (character-token (location 1 11 12) (location 1 12 13) #\i))
        (character-token (location 1 12 13) (location 1 13 14) #\"))))
     #f
     (character-token (location 1 13 14) (location 1 14 15) #\>)
     (list (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "Single-quoted attribute"
   "<p class='hi'>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 14 15)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 14 15)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\c)
        (character-token (location 1 4 5) (location 1 5 6) #\l)
        (character-token (location 1 5 6) (location 1 6 7) #\a)
        (character-token (location 1 6 7) (location 1 7 8) #\s)
        (character-token (location 1 7 8) (location 1 8 9) #\s))
       (character-token (location 1 8 9) (location 1 9 10) #\=)
       (quoted-attr-value
        (character-token (location 1 9 10) (location 1 10 11) #\')
        (list
         (character-token (location 1 10 11) (location 1 11 12) #\h)
         (character-token (location 1 11 12) (location 1 12 13) #\i))
        (character-token (location 1 12 13) (location 1 13 14) #\'))))
     #f
     (character-token (location 1 13 14) (location 1 14 15) #\>)
     (list (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "Single-quoted attribute, then double-quoted attribute"
   "<p class='hi' x=\"hi\">"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 21 22)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 14 15)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\c)
        (character-token (location 1 4 5) (location 1 5 6) #\l)
        (character-token (location 1 5 6) (location 1 6 7) #\a)
        (character-token (location 1 6 7) (location 1 7 8) #\s)
        (character-token (location 1 7 8) (location 1 8 9) #\s))
       (character-token (location 1 8 9) (location 1 9 10) #\=)
       (quoted-attr-value
        (character-token (location 1 9 10) (location 1 10 11) #\')
        (list
         (character-token (location 1 10 11) (location 1 11 12) #\h)
         (character-token (location 1 11 12) (location 1 12 13) #\i))
        (character-token (location 1 12 13) (location 1 13 14) #\')))
      (attribute-token
       (location 1 13 14)
       (location 1 21 22)
       (list (character-token (location 1 14 15) (location 1 15 16) #\x))
       (character-token (location 1 15 16) (location 1 16 17) #\=)
       (quoted-attr-value
        (character-token (location 1 16 17) (location 1 17 18) #\")
        (list
         (character-token (location 1 17 18) (location 1 18 19) #\h)
         (character-token (location 1 18 19) (location 1 19 20) #\i))
        (character-token (location 1 19 20) (location 1 20 21) #\"))))
     #f
     (character-token (location 1 20 21) (location 1 21 22) #\>)
     (list
      (character-token (location 1 13 14) (location 1 14 15) #\space)
      (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "Single-quoted attribute, then attribute without value"
   "<p class='hi' xyz>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 18 19)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 14 15)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\c)
        (character-token (location 1 4 5) (location 1 5 6) #\l)
        (character-token (location 1 5 6) (location 1 6 7) #\a)
        (character-token (location 1 6 7) (location 1 7 8) #\s)
        (character-token (location 1 7 8) (location 1 8 9) #\s))
       (character-token (location 1 8 9) (location 1 9 10) #\=)
       (quoted-attr-value
        (character-token (location 1 9 10) (location 1 10 11) #\')
        (list
         (character-token (location 1 10 11) (location 1 11 12) #\h)
         (character-token (location 1 11 12) (location 1 12 13) #\i))
        (character-token (location 1 12 13) (location 1 13 14) #\')))
      (attribute-token
       (location 1 13 14)
       (location 1 18 19)
       (list
        (character-token (location 1 14 15) (location 1 15 16) #\x)
        (character-token (location 1 15 16) (location 1 16 17) #\y)
        (character-token (location 1 16 17) (location 1 17 18) #\z))
       #f
       #f))
     #f
     (character-token (location 1 17 18) (location 1 18 19) #\>)
     (list
      (character-token (location 1 13 14) (location 1 14 15) #\space)
      (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "Two double-quoted attributes"
   "<p class=\"hi\" style=\"bye\">"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 26 27)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 14 15)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\c)
        (character-token (location 1 4 5) (location 1 5 6) #\l)
        (character-token (location 1 5 6) (location 1 6 7) #\a)
        (character-token (location 1 6 7) (location 1 7 8) #\s)
        (character-token (location 1 7 8) (location 1 8 9) #\s))
       (character-token (location 1 8 9) (location 1 9 10) #\=)
       (quoted-attr-value
        (character-token (location 1 9 10) (location 1 10 11) #\")
        (list
         (character-token (location 1 10 11) (location 1 11 12) #\h)
         (character-token (location 1 11 12) (location 1 12 13) #\i))
        (character-token (location 1 12 13) (location 1 13 14) #\")))
      (attribute-token
       (location 1 13 14)
       (location 1 26 27)
       (list
        (character-token (location 1 14 15) (location 1 15 16) #\s)
        (character-token (location 1 15 16) (location 1 16 17) #\t)
        (character-token (location 1 16 17) (location 1 17 18) #\y)
        (character-token (location 1 17 18) (location 1 18 19) #\l)
        (character-token (location 1 18 19) (location 1 19 20) #\e))
       (character-token (location 1 19 20) (location 1 20 21) #\=)
       (quoted-attr-value
        (character-token (location 1 20 21) (location 1 21 22) #\")
        (list
         (character-token (location 1 21 22) (location 1 22 23) #\b)
         (character-token (location 1 22 23) (location 1 23 24) #\y)
         (character-token (location 1 23 24) (location 1 24 25) #\e))
        (character-token (location 1 24 25) (location 1 25 26) #\"))))
     #f
     (character-token (location 1 25 26) (location 1 26 27) #\>)
     (list
      (character-token (location 1 13 14) (location 1 14 15) #\space)
      (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "Bogus self-closing tag"
   "<p/ >"
   (list
    (unexpected-solidus-in-tag (location 1 3 4) #f)
    (start-tag-token
     (location 1 0 1)
     (location 1 5 6)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     '()
     (character-token (location 1 2 3) (location 1 3 4) #\/)
     (character-token (location 1 4 5) (location 1 5 6) #\>)
     (list (character-token (location 1 3 4) (location 1 4 5) #\space)))))
  (check-it
   "EOF after tag name"
   "<foo "
   (list
    (eof-in-tag (location 1 5 6) #f)
    (character-token (location 1 0 1) (location 1 1 2) '(#\< . #f))
    (character-token (location 1 1 2) (location 1 2 3) '(#\f . #f))
    (character-token (location 1 2 3) (location 1 3 4) '(#\o . #f))
    (character-token (location 1 3 4) (location 1 4 5) '(#\o . #f))
    (character-token (location 1 4 5) (location 1 5 6) '(#\space . #f))))
  (check-it
   "Attribute begins with equals sign"
   "<html =what>"
   (list
    (unexpected-equals-sign-before-attribute-name (location 1 6 7) #f)
    (start-tag-token
     (location 1 0 1)
     (location 1 12 13)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list
      (character-token (location 1 1 2) (location 1 2 3) #\h)
      (character-token (location 1 2 3) (location 1 3 4) #\t)
      (character-token (location 1 3 4) (location 1 4 5) #\m)
      (character-token (location 1 4 5) (location 1 5 6) #\l))
     (list
      (attribute-token
       (location 1 6 7)
       (location 1 12 13)
       (list
        (character-token (location 1 6 7) (location 1 7 8) #\=)
        (character-token (location 1 7 8) (location 1 8 9) #\w)
        (character-token (location 1 8 9) (location 1 9 10) #\h)
        (character-token (location 1 9 10) (location 1 10 11) #\a)
        (character-token (location 1 10 11) (location 1 11 12) #\t))
       #f
       #f))
     #f
     (character-token (location 1 11 12) (location 1 12 13) #\>)
     (list (character-token (location 1 5 6) (location 1 6 7) #\space)))))
  (check-it
   "Null character in attribute name"
   (format "<foo b~ar='moo'>" #\nul)
   (list
    (unexpected-null-character (location 1 6 7) #f)
    (start-tag-token
     (location 1 0 1)
     (location 1 15 16)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list
      (character-token (location 1 1 2) (location 1 2 3) #\f)
      (character-token (location 1 2 3) (location 1 3 4) #\o)
      (character-token (location 1 3 4) (location 1 4 5) #\o))
     (list
      (attribute-token
       (location 1 4 5)
       (location 1 15 16)
       (list
        (character-token (location 1 5 6) (location 1 6 7) #\b)
        (character-token (location 1 6 7) (location 1 7 8) '(#\nul . #\�))
        (character-token (location 1 7 8) (location 1 8 9) #\r))
       (character-token (location 1 8 9) (location 1 9 10) #\=)
       (quoted-attr-value
        (character-token (location 1 9 10) (location 1 10 11) #\')
        (list
         (character-token (location 1 10 11) (location 1 11 12) #\m)
         (character-token (location 1 11 12) (location 1 12 13) #\o)
         (character-token (location 1 12 13) (location 1 13 14) #\o))
        (character-token (location 1 13 14) (location 1 14 15) #\'))))
     #f
     (character-token (location 1 14 15) (location 1 15 16) #\>)
     (list (character-token (location 1 4 5) (location 1 5 6) #\space)))))
  (check-it
   "EOF in attribute name"
   "<foo bar"
   (list
    (eof-in-tag (location 1 8 9) #f)
    (character-token (location 1 0 1) (location 1 1 2) '(#\< . #f))
    (character-token (location 1 1 2) (location 1 2 3) '(#\f . #f))
    (character-token (location 1 2 3) (location 1 3 4) '(#\o . #f))
    (character-token (location 1 3 4) (location 1 4 5) '(#\o . #f))
    (character-token (location 1 4 5) (location 1 5 6) '(#\space . #f))
    (character-token (location 1 5 6) (location 1 6 7) '(#\b . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\a . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))))
  (check-it
   "Uppercase letter in attribute name"
   "<htmll LANG='en'>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 17 18)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list
      (character-token (location 1 1 2) (location 1 2 3) #\h)
      (character-token (location 1 2 3) (location 1 3 4) #\t)
      (character-token (location 1 3 4) (location 1 4 5) #\m)
      (character-token (location 1 4 5) (location 1 5 6) #\l)
      (character-token (location 1 5 6) (location 1 6 7) #\l))
     (list
      (attribute-token
       (location 1 6 7)
       (location 1 17 18)
       (list
        (character-token (location 1 7 8) (location 1 8 9) '(#\L . #\l))
        (character-token (location 1 8 9) (location 1 9 10) '(#\A . #\a))
        (character-token (location 1 9 10) (location 1 10 11) '(#\N . #\n))
        (character-token (location 1 10 11) (location 1 11 12) '(#\G . #\g)))
       (character-token (location 1 11 12) (location 1 12 13) #\=)
       (quoted-attr-value
        (character-token (location 1 12 13) (location 1 13 14) #\')
        (list
         (character-token (location 1 13 14) (location 1 14 15) #\e)
         (character-token (location 1 14 15) (location 1 15 16) #\n))
        (character-token (location 1 15 16) (location 1 16 17) #\'))))
     #f
     (character-token (location 1 16 17) (location 1 17 18) #\>)
     (list (character-token (location 1 6 7) (location 1 7 8) #\space)))))
  (check-it
   "Two tag properties"
   "<p class foo>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 13 14)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 9 10)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\c)
        (character-token (location 1 4 5) (location 1 5 6) #\l)
        (character-token (location 1 5 6) (location 1 6 7) #\a)
        (character-token (location 1 6 7) (location 1 7 8) #\s)
        (character-token (location 1 7 8) (location 1 8 9) #\s))
       #f
       #f)
      (attribute-token
       (location 1 2 3)
       (location 1 13 14)
       (list
        (character-token (location 1 9 10) (location 1 10 11) #\f)
        (character-token (location 1 10 11) (location 1 11 12) #\o)
        (character-token (location 1 11 12) (location 1 12 13) #\o))
       #f
       #f))
     #f
     (character-token (location 1 12 13) (location 1 13 14) #\>)
     (list
      (character-token (location 1 8 9) (location 1 9 10) #\space)
      (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "Double quote in attribute name"
   "<html class\"hi\">"
   (list
    (unexpected-character-in-attribute-name
     (location 1 11 12)
     (character-token (location 1 11 12) (location 1 12 13) #\"))
    (unexpected-character-in-attribute-name
     (location 1 14 15)
     (character-token (location 1 14 15) (location 1 15 16) #\"))
    (start-tag-token
     (location 1 0 1)
     (location 1 16 17)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list
      (character-token (location 1 1 2) (location 1 2 3) #\h)
      (character-token (location 1 2 3) (location 1 3 4) #\t)
      (character-token (location 1 3 4) (location 1 4 5) #\m)
      (character-token (location 1 4 5) (location 1 5 6) #\l))
     (list
      (attribute-token
       (location 1 5 6)
       (location 1 16 17)
       (list
        (character-token (location 1 6 7) (location 1 7 8) #\c)
        (character-token (location 1 7 8) (location 1 8 9) #\l)
        (character-token (location 1 8 9) (location 1 9 10) #\a)
        (character-token (location 1 9 10) (location 1 10 11) #\s)
        (character-token (location 1 10 11) (location 1 11 12) #\s)
        (character-token (location 1 11 12) (location 1 12 13) #\")
        (character-token (location 1 12 13) (location 1 13 14) #\h)
        (character-token (location 1 13 14) (location 1 14 15) #\i)
        (character-token (location 1 14 15) (location 1 15 16) #\"))
       #f
       #f))
     #f
     (character-token (location 1 15 16) (location 1 16 17) #\>)
     (list (character-token (location 1 5 6) (location 1 6 7) #\space)))))
  (check-it
   "Two spaces after attribute name"
   "<p class  >"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 11 12)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 11 12)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\c)
        (character-token (location 1 4 5) (location 1 5 6) #\l)
        (character-token (location 1 5 6) (location 1 6 7) #\a)
        (character-token (location 1 6 7) (location 1 7 8) #\s)
        (character-token (location 1 7 8) (location 1 8 9) #\s))
       #f
       #f))
     #f
     (character-token (location 1 10 11) (location 1 11 12) #\>)
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\space)
      (character-token (location 1 8 9) (location 1 9 10) #\space)
      (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "Space after attribute before value"
   "<p class ='foo'>"
   (list
    (unexpected-character-in-unquoted-attribute-value
     (location 1 9 10)
     (character-token (location 1 9 10) (location 1 10 11) #\=))
    (unexpected-character-in-unquoted-attribute-value
     (location 1 10 11)
     (character-token (location 1 10 11) (location 1 11 12) #\'))
    (unexpected-character-in-unquoted-attribute-value
     (location 1 14 15)
     (character-token (location 1 14 15) (location 1 15 16) #\'))
    (start-tag-token
     (location 1 0 1)
     (location 1 16 17)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 16 17)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\c)
        (character-token (location 1 4 5) (location 1 5 6) #\l)
        (character-token (location 1 5 6) (location 1 6 7) #\a)
        (character-token (location 1 6 7) (location 1 7 8) #\s)
        (character-token (location 1 7 8) (location 1 8 9) #\s))
       #f
       (quoted-attr-value
        #f
        (list
         (character-token (location 1 9 10) (location 1 10 11) #\=)
         (character-token (location 1 10 11) (location 1 11 12) #\')
         (character-token (location 1 11 12) (location 1 12 13) #\f)
         (character-token (location 1 12 13) (location 1 13 14) #\o)
         (character-token (location 1 13 14) (location 1 14 15) #\o)
         (character-token (location 1 14 15) (location 1 15 16) #\'))
        #f)))
     #f
     (character-token (location 1 15 16) (location 1 16 17) #\>)
     (list
      (character-token (location 1 8 9) (location 1 9 10) #\space)
      (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "Slash after attribute name"
   "<p style/>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 10 11)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 10 11)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\s)
        (character-token (location 1 4 5) (location 1 5 6) #\t)
        (character-token (location 1 5 6) (location 1 6 7) #\y)
        (character-token (location 1 6 7) (location 1 7 8) #\l)
        (character-token (location 1 7 8) (location 1 8 9) #\e))
       #f
       #f))
     (character-token (location 1 8 9) (location 1 9 10) #\/)
     (character-token (location 1 9 10) (location 1 10 11) #\>)
     (list (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "EOF after attribute"
   "<html lang"
   (list
    (eof-in-tag (location 1 10 11) #f)
    (character-token (location 1 0 1) (location 1 1 2) '(#\< . #f))
    (character-token (location 1 1 2) (location 1 2 3) '(#\h . #f))
    (character-token (location 1 2 3) (location 1 3 4) '(#\t . #f))
    (character-token (location 1 3 4) (location 1 4 5) '(#\m . #f))
    (character-token (location 1 4 5) (location 1 5 6) '(#\l . #f))
    (character-token (location 1 5 6) (location 1 6 7) '(#\space . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\l . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\a . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\n . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\g . #f))))
  (check-it
   "EOF after attribute & equals sign"
   "<html lang="
   (list
    (eof-in-tag (location 1 11 12) #f)
    (character-token (location 1 0 1) (location 1 1 2) '(#\< . #f))
    (character-token (location 1 1 2) (location 1 2 3) '(#\h . #f))
    (character-token (location 1 2 3) (location 1 3 4) '(#\t . #f))
    (character-token (location 1 3 4) (location 1 4 5) '(#\m . #f))
    (character-token (location 1 4 5) (location 1 5 6) '(#\l . #f))
    (character-token (location 1 5 6) (location 1 6 7) '(#\space . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\l . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\a . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\n . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\g . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\= . #f))))
  (check-it
   "EOF in single-quoted attribute value"
   "<p class='foo"
   (list
    (eof-in-tag (location 1 13 14) #f)
    (character-token (location 1 0 1) (location 1 1 2) '(#\< . #f))
    (character-token (location 1 1 2) (location 1 2 3) '(#\p . #f))
    (character-token (location 1 2 3) (location 1 3 4) '(#\space . #f))
    (character-token (location 1 3 4) (location 1 4 5) '(#\c . #f))
    (character-token (location 1 4 5) (location 1 5 6) '(#\l . #f))
    (character-token (location 1 5 6) (location 1 6 7) '(#\a . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\s . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\s . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\= . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\' . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\f . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\o . #f))
    (character-token (location 1 12 13) (location 1 13 14) '(#\o . #f))))
  (check-it
   "EOF in double-quoted attribute value"
   "<p class=\"foo"
   (list
    (eof-in-tag (location 1 13 14) #f)
    (character-token (location 1 0 1) (location 1 1 2) '(#\< . #f))
    (character-token (location 1 1 2) (location 1 2 3) '(#\p . #f))
    (character-token (location 1 2 3) (location 1 3 4) '(#\space . #f))
    (character-token (location 1 3 4) (location 1 4 5) '(#\c . #f))
    (character-token (location 1 4 5) (location 1 5 6) '(#\l . #f))
    (character-token (location 1 5 6) (location 1 6 7) '(#\a . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\s . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\s . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\= . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\" . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\f . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\o . #f))
    (character-token (location 1 12 13) (location 1 13 14) '(#\o . #f))))
  (check-it
   "Null character in single-quoted attribute value"
   (format "<q lang='e~an'>" #\nul)
   (list
    (unexpected-null-character (location 1 10 11) #f)
    (start-tag-token
     (location 1 0 1)
     (location 1 14 15)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\q))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 14 15)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\l)
        (character-token (location 1 4 5) (location 1 5 6) #\a)
        (character-token (location 1 5 6) (location 1 6 7) #\n)
        (character-token (location 1 6 7) (location 1 7 8) #\g))
       (character-token (location 1 7 8) (location 1 8 9) #\=)
       (quoted-attr-value
        (character-token (location 1 8 9) (location 1 9 10) #\')
        (list
         (character-token (location 1 9 10) (location 1 10 11) #\e)
         (character-token (location 1 10 11) (location 1 11 12) '(#\nul . #\�))
         (character-token (location 1 11 12) (location 1 12 13) #\n))
        (character-token (location 1 12 13) (location 1 13 14) #\'))))
     #f
     (character-token (location 1 13 14) (location 1 14 15) #\>)
     (list (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "EOF after quoted attribute"
   "<p x='foo'"
   (list
    (eof-in-tag (location 1 10 11) #f)
    (character-token (location 1 0 1) (location 1 1 2) '(#\< . #f))
    (character-token (location 1 1 2) (location 1 2 3) '(#\p . #f))
    (character-token (location 1 2 3) (location 1 3 4) '(#\space . #f))
    (character-token (location 1 3 4) (location 1 4 5) '(#\x . #f))
    (character-token (location 1 4 5) (location 1 5 6) '(#\= . #f))
    (character-token (location 1 5 6) (location 1 6 7) '(#\' . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\f . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\o . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\o . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\' . #f))))
  (check-it
   "EOF after two attributes"
   "<p x='y' y='z'"
   (list
    (eof-in-tag (location 1 14 15) #f)
    (character-token (location 1 0 1) (location 1 1 2) '(#\< . #f))
    (character-token (location 1 1 2) (location 1 2 3) '(#\p . #f))
    (character-token (location 1 2 3) (location 1 3 4) '(#\space . #f))
    (character-token (location 1 3 4) (location 1 4 5) '(#\x . #f))
    (character-token (location 1 4 5) (location 1 5 6) '(#\= . #f))
    (character-token (location 1 5 6) (location 1 6 7) '(#\' . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\y . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\' . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\space . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\y . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\= . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\' . #f))
    (character-token (location 1 12 13) (location 1 13 14) '(#\z . #f))
    (character-token (location 1 13 14) (location 1 14 15) '(#\' . #f))))
  (check-it
   "Missing whitespace between quoted attributes"
   "<p x='y'y='z'>"
   (list
    (missing-whitespace-between-attributes (location 1 8 9) #f)
    (start-tag-token
     (location 1 0 1)
     (location 1 14 15)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 8 9)
       (list (character-token (location 1 3 4) (location 1 4 5) #\x))
       (character-token (location 1 4 5) (location 1 5 6) #\=)
       (quoted-attr-value
        (character-token (location 1 5 6) (location 1 6 7) #\')
        (list (character-token (location 1 6 7) (location 1 7 8) #\y))
        (character-token (location 1 7 8) (location 1 8 9) #\')))
      (attribute-token
       (location 1 7 8)
       (location 1 14 15)
       (list (character-token (location 1 8 9) (location 1 9 10) #\y))
       (character-token (location 1 9 10) (location 1 10 11) #\=)
       (quoted-attr-value
        (character-token (location 1 10 11) (location 1 11 12) #\')
        (list (character-token (location 1 11 12) (location 1 12 13) #\z))
        (character-token (location 1 12 13) (location 1 13 14) #\'))))
     #f
     (character-token (location 1 13 14) (location 1 14 15) #\>)
     (list (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "Null character in double-quoted attribute value"
   (format "<q lang=\"e~an\">" #\nul)
   (list
    (unexpected-null-character (location 1 10 11) #f)
    (start-tag-token
     (location 1 0 1)
     (location 1 14 15)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\q))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 14 15)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\l)
        (character-token (location 1 4 5) (location 1 5 6) #\a)
        (character-token (location 1 5 6) (location 1 6 7) #\n)
        (character-token (location 1 6 7) (location 1 7 8) #\g))
       (character-token (location 1 7 8) (location 1 8 9) #\=)
       (quoted-attr-value
        (character-token (location 1 8 9) (location 1 9 10) #\")
        (list
         (character-token (location 1 9 10) (location 1 10 11) #\e)
         (character-token (location 1 10 11) (location 1 11 12) '(#\nul . #\�))
         (character-token (location 1 11 12) (location 1 12 13) #\n))
        (character-token (location 1 12 13) (location 1 13 14) #\"))))
     #f
     (character-token (location 1 13 14) (location 1 14 15) #\>)
     (list (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "Space after attribute name, before value"
   "<p lang= 'en'>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 14 15)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 14 15)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\l)
        (character-token (location 1 4 5) (location 1 5 6) #\a)
        (character-token (location 1 5 6) (location 1 6 7) #\n)
        (character-token (location 1 6 7) (location 1 7 8) #\g))
       (character-token (location 1 7 8) (location 1 8 9) #\=)
       (quoted-attr-value
        (character-token (location 1 9 10) (location 1 10 11) #\')
        (list
         (character-token (location 1 10 11) (location 1 11 12) #\e)
         (character-token (location 1 11 12) (location 1 12 13) #\n))
        (character-token (location 1 12 13) (location 1 13 14) #\'))))
     #f
     (character-token (location 1 13 14) (location 1 14 15) #\>)
     (list
      (character-token (location 1 8 9) (location 1 9 10) #\space)
      (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "> after attribute name and equals sign"
   "<p class=>"
   (list
    (missing-attribute-value (location 1 9 10) #f)
    (start-tag-token
     (location 1 0 1)
     (location 1 10 11)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 10 11)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\c)
        (character-token (location 1 4 5) (location 1 5 6) #\l)
        (character-token (location 1 5 6) (location 1 6 7) #\a)
        (character-token (location 1 6 7) (location 1 7 8) #\s)
        (character-token (location 1 7 8) (location 1 8 9) #\s))
       (character-token (location 1 8 9) (location 1 9 10) #\=)
       #f))
     #f
     (character-token (location 1 9 10) (location 1 10 11) #\>)
     (list (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "Unquoted attribute value"
   "<html lang=en>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 14 15)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list
      (character-token (location 1 1 2) (location 1 2 3) #\h)
      (character-token (location 1 2 3) (location 1 3 4) #\t)
      (character-token (location 1 3 4) (location 1 4 5) #\m)
      (character-token (location 1 4 5) (location 1 5 6) #\l))
     (list
      (attribute-token
       (location 1 5 6)
       (location 1 14 15)
       (list
        (character-token (location 1 6 7) (location 1 7 8) #\l)
        (character-token (location 1 7 8) (location 1 8 9) #\a)
        (character-token (location 1 8 9) (location 1 9 10) #\n)
        (character-token (location 1 9 10) (location 1 10 11) #\g))
       (character-token (location 1 10 11) (location 1 11 12) #\=)
       (quoted-attr-value
        #f
        (list
         (character-token (location 1 11 12) (location 1 12 13) #\e)
         (character-token (location 1 12 13) (location 1 13 14) #\n))
        #f)))
     #f
     (character-token (location 1 13 14) (location 1 14 15) #\>)
     (list (character-token (location 1 5 6) (location 1 6 7) #\space)))))
  (check-it
   "EOF in self-closing tag"
   "<html/"
   (list
    (eof-in-tag (location 1 6 7) #f)
    (character-token (location 1 0 1) (location 1 1 2) '(#\< . #f))
    (character-token (location 1 1 2) (location 1 2 3) '(#\h . #f))
    (character-token (location 1 2 3) (location 1 3 4) '(#\t . #f))
    (character-token (location 1 3 4) (location 1 4 5) '(#\m . #f))
    (character-token (location 1 4 5) (location 1 5 6) '(#\l . #f))
    (character-token (location 1 5 6) (location 1 6 7) '(#\/ . #f))))
  (check-it
   "Null character in tag name"
   (format "<nu~al>" #\nul)
   (list
    (unexpected-null-character (location 1 3 4) #f)
    (start-tag-token
     (location 1 0 1)
     (location 1 6 7)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list
      (character-token (location 1 1 2) (location 1 2 3) #\n)
      (character-token (location 1 2 3) (location 1 3 4) #\u)
      (character-token (location 1 3 4) (location 1 4 5) '(#\nul . #\�))
      (character-token (location 1 4 5) (location 1 5 6) #\l))
     '()
     #f
     (character-token (location 1 5 6) (location 1 6 7) #\>)
     '())))
  (check-it
   "Ampersand entity in unquoted attribute value"
   "<html lang=&amp;>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 17 18)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list
      (character-token (location 1 1 2) (location 1 2 3) #\h)
      (character-token (location 1 2 3) (location 1 3 4) #\t)
      (character-token (location 1 3 4) (location 1 4 5) #\m)
      (character-token (location 1 4 5) (location 1 5 6) #\l))
     (list
      (attribute-token
       (location 1 5 6)
       (location 1 17 18)
       (list
        (character-token (location 1 6 7) (location 1 7 8) #\l)
        (character-token (location 1 7 8) (location 1 8 9) #\a)
        (character-token (location 1 8 9) (location 1 9 10) #\n)
        (character-token (location 1 9 10) (location 1 10 11) #\g))
       (character-token (location 1 10 11) (location 1 11 12) #\=)
       (quoted-attr-value
        #f
        (list
         (character-reference-token
          (location 1 11 12)
          (location 1 16 17)
          (list
           (character-token (location 1 11 12) (location 1 12 13) #\&)
           (character-token (location 1 12 13) (location 1 13 14) #\a)
           (character-token (location 1 13 14) (location 1 14 15) #\m)
           (character-token (location 1 14 15) (location 1 15 16) #\p)
           (character-token (location 1 15 16) (location 1 16 17) #\;))
          #\&
          #f))
        #f)))
     #f
     (character-token (location 1 16 17) (location 1 17 18) #\>)
     (list (character-token (location 1 5 6) (location 1 6 7) #\space)))))
  (check-it
   "Logical and (&and) in single-quoted attribute value"
   "<p class='&and;'>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 17 18)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 17 18)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\c)
        (character-token (location 1 4 5) (location 1 5 6) #\l)
        (character-token (location 1 5 6) (location 1 6 7) #\a)
        (character-token (location 1 6 7) (location 1 7 8) #\s)
        (character-token (location 1 7 8) (location 1 8 9) #\s))
       (character-token (location 1 8 9) (location 1 9 10) #\=)
       (quoted-attr-value
        (character-token (location 1 9 10) (location 1 10 11) #\')
        (list
         (character-reference-token
          (location 1 10 11)
          (location 1 15 16)
          (list
           (character-token (location 1 10 11) (location 1 11 12) #\&)
           (character-token (location 1 11 12) (location 1 12 13) #\a)
           (character-token (location 1 12 13) (location 1 13 14) #\n)
           (character-token (location 1 13 14) (location 1 14 15) #\d)
           (character-token (location 1 14 15) (location 1 15 16) #\;))
          #\∧
          #f))
        (character-token (location 1 15 16) (location 1 16 17) #\'))))
     #f
     (character-token (location 1 16 17) (location 1 17 18) #\>)
     (list (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "Logical and (&and) in double-quoted attribute value"
   "<p class=\"&and;\">"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 17 18)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 17 18)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\c)
        (character-token (location 1 4 5) (location 1 5 6) #\l)
        (character-token (location 1 5 6) (location 1 6 7) #\a)
        (character-token (location 1 6 7) (location 1 7 8) #\s)
        (character-token (location 1 7 8) (location 1 8 9) #\s))
       (character-token (location 1 8 9) (location 1 9 10) #\=)
       (quoted-attr-value
        (character-token (location 1 9 10) (location 1 10 11) #\")
        (list
         (character-reference-token
          (location 1 10 11)
          (location 1 15 16)
          (list
           (character-token (location 1 10 11) (location 1 11 12) #\&)
           (character-token (location 1 11 12) (location 1 12 13) #\a)
           (character-token (location 1 12 13) (location 1 13 14) #\n)
           (character-token (location 1 13 14) (location 1 14 15) #\d)
           (character-token (location 1 14 15) (location 1 15 16) #\;))
          #\∧
          #f))
        (character-token (location 1 15 16) (location 1 16 17) #\"))))
     #f
     (character-token (location 1 16 17) (location 1 17 18) #\>)
     (list (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "Less-than sign in unquoted attribute value"
   "<p class=b<ig>"
   (list
    (unexpected-character-in-unquoted-attribute-value
     (location 1 10 11)
     (character-token (location 1 10 11) (location 1 11 12) #\<))
    (start-tag-token
     (location 1 0 1)
     (location 1 14 15)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 1 2) (location 1 2 3) #\p))
     (list
      (attribute-token
       (location 1 2 3)
       (location 1 14 15)
       (list
        (character-token (location 1 3 4) (location 1 4 5) #\c)
        (character-token (location 1 4 5) (location 1 5 6) #\l)
        (character-token (location 1 5 6) (location 1 6 7) #\a)
        (character-token (location 1 6 7) (location 1 7 8) #\s)
        (character-token (location 1 7 8) (location 1 8 9) #\s))
       (character-token (location 1 8 9) (location 1 9 10) #\=)
       (quoted-attr-value
        #f
        (list
         (character-token (location 1 9 10) (location 1 10 11) #\b)
         (character-token (location 1 10 11) (location 1 11 12) #\<)
         (character-token (location 1 11 12) (location 1 12 13) #\i)
         (character-token (location 1 12 13) (location 1 13 14) #\g))
        #f)))
     #f
     (character-token (location 1 13 14) (location 1 14 15) #\>)
     (list (character-token (location 1 2 3) (location 1 3 4) #\space)))))
  (check-it
   "EOF in self-closing tag with attribute (1)"
   "<p class=foo /"
   (list
    (eof-in-tag (location 1 14 15) #f)
    (character-token (location 1 0 1) (location 1 1 2) '(#\< . #f))
    (character-token (location 1 1 2) (location 1 2 3) '(#\p . #f))
    (character-token (location 1 2 3) (location 1 3 4) '(#\space . #f))
    (character-token (location 1 3 4) (location 1 4 5) '(#\c . #f))
    (character-token (location 1 4 5) (location 1 5 6) '(#\l . #f))
    (character-token (location 1 5 6) (location 1 6 7) '(#\a . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\s . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\s . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\= . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\f . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\o . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\o . #f))
    (character-token (location 1 12 13) (location 1 13 14) '(#\space . #f))
    (character-token (location 1 13 14) (location 1 14 15) '(#\/ . #f))))
  (check-it
   "EOF in self-closing tag with attribute (2)"
   "<p class=foo/"
   (list
    (eof-in-tag (location 1 13 14) #f)
    (character-token (location 1 0 1) (location 1 1 2) '(#\< . #f))
    (character-token (location 1 1 2) (location 1 2 3) '(#\p . #f))
    (character-token (location 1 2 3) (location 1 3 4) '(#\space . #f))
    (character-token (location 1 3 4) (location 1 4 5) '(#\c . #f))
    (character-token (location 1 4 5) (location 1 5 6) '(#\l . #f))
    (character-token (location 1 5 6) (location 1 6 7) '(#\a . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\s . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\s . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\= . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\f . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\o . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\o . #f))
    (character-token (location 1 12 13) (location 1 13 14) '(#\/ . #f))))
  (check-it
   "Two unquoted attribute values"
   "<html lang=en dir=ltr>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 22 23)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list
      (character-token (location 1 1 2) (location 1 2 3) #\h)
      (character-token (location 1 2 3) (location 1 3 4) #\t)
      (character-token (location 1 3 4) (location 1 4 5) #\m)
      (character-token (location 1 4 5) (location 1 5 6) #\l))
     (list
      (attribute-token
       (location 1 5 6)
       (location 1 14 15)
       (list
        (character-token (location 1 6 7) (location 1 7 8) #\l)
        (character-token (location 1 7 8) (location 1 8 9) #\a)
        (character-token (location 1 8 9) (location 1 9 10) #\n)
        (character-token (location 1 9 10) (location 1 10 11) #\g))
       (character-token (location 1 10 11) (location 1 11 12) #\=)
       (quoted-attr-value
        #f
        (list
         (character-token (location 1 11 12) (location 1 12 13) #\e)
         (character-token (location 1 12 13) (location 1 13 14) #\n))
        #f))
      (attribute-token
       (location 1 13 14)
       (location 1 22 23)
       (list
        (character-token (location 1 14 15) (location 1 15 16) #\d)
        (character-token (location 1 15 16) (location 1 16 17) #\i)
        (character-token (location 1 16 17) (location 1 17 18) #\r))
       (character-token (location 1 17 18) (location 1 18 19) #\=)
       (quoted-attr-value
        #f
        (list
         (character-token (location 1 18 19) (location 1 19 20) #\l)
         (character-token (location 1 19 20) (location 1 20 21) #\t)
         (character-token (location 1 20 21) (location 1 21 22) #\r))
        #f)))
     #f
     (character-token (location 1 21 22) (location 1 22 23) #\>)
     (list
      (character-token (location 1 13 14) (location 1 14 15) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\space)))))
  (check-it
   "Self-closing tag with attributes"
   "<foo class='what'/>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 19 20)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list
      (character-token (location 1 1 2) (location 1 2 3) #\f)
      (character-token (location 1 2 3) (location 1 3 4) #\o)
      (character-token (location 1 3 4) (location 1 4 5) #\o))
     (list
      (attribute-token
       (location 1 4 5)
       (location 1 18 19)
       (list
        (character-token (location 1 5 6) (location 1 6 7) #\c)
        (character-token (location 1 6 7) (location 1 7 8) #\l)
        (character-token (location 1 7 8) (location 1 8 9) #\a)
        (character-token (location 1 8 9) (location 1 9 10) #\s)
        (character-token (location 1 9 10) (location 1 10 11) #\s))
       (character-token (location 1 10 11) (location 1 11 12) #\=)
       (quoted-attr-value
        (character-token (location 1 11 12) (location 1 12 13) #\')
        (list
         (character-token (location 1 12 13) (location 1 13 14) #\w)
         (character-token (location 1 13 14) (location 1 14 15) #\h)
         (character-token (location 1 14 15) (location 1 15 16) #\a)
         (character-token (location 1 15 16) (location 1 16 17) #\t))
        (character-token (location 1 16 17) (location 1 17 18) #\'))))
     (character-token (location 1 17 18) (location 1 18 19) #\/)
     (character-token (location 1 18 19) (location 1 19 20) #\>)
     (list (character-token (location 1 4 5) (location 1 5 6) #\space)))))
  (check-it
   "Straightforward self-closing tag"
   "<bar/>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 6 7)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list
      (character-token (location 1 1 2) (location 1 2 3) #\b)
      (character-token (location 1 2 3) (location 1 3 4) #\a)
      (character-token (location 1 3 4) (location 1 4 5) #\r))
     '()
     (character-token (location 1 4 5) (location 1 5 6) #\/)
     (character-token (location 1 5 6) (location 1 6 7) #\>)
     '())))
  (check-it
   "Uppercase letter in tag name"
   "<fOO>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 5 6)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list
      (character-token (location 1 1 2) (location 1 2 3) #\f)
      (character-token (location 1 2 3) (location 1 3 4) '(#\O . #\o))
      (character-token (location 1 3 4) (location 1 4 5) '(#\O . #\o)))
     '()
     #f
     (character-token (location 1 4 5) (location 1 5 6) #\>)
     '())))
  (check-it
   "Tag name abruptly ends"
   "<foo"
   (list
    (character-token (location 1 0 1) (location 1 1 2) '(#\< . #f))
    (character-token (location 1 1 2) (location 1 2 3) '(#\f . #f))
    (character-token (location 1 2 3) (location 1 3 4) '(#\o . #f))
    (character-token (location 1 3 4) (location 1 4 5) '(#\o . #f))))
  (check-it
   "Simple tag with no attributes"
   "<foo>"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 5 6)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list
      (character-token (location 1 1 2) (location 1 2 3) #\f)
      (character-token (location 1 2 3) (location 1 3 4) #\o)
      (character-token (location 1 3 4) (location 1 4 5) #\o))
     '()
     #f
     (character-token (location 1 4 5) (location 1 5 6) #\>)
     '())))
  (check-it
   "Whitespace after attribute (no value)"
   "<html lang >"
   (list
    (start-tag-token
     (location 1 0 1)
     (location 1 12 13)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list
      (character-token (location 1 1 2) (location 1 2 3) #\h)
      (character-token (location 1 2 3) (location 1 3 4) #\t)
      (character-token (location 1 3 4) (location 1 4 5) #\m)
      (character-token (location 1 4 5) (location 1 5 6) #\l))
     (list
      (attribute-token
       (location 1 5 6)
       (location 1 12 13)
       (list
        (character-token (location 1 6 7) (location 1 7 8) #\l)
        (character-token (location 1 7 8) (location 1 8 9) #\a)
        (character-token (location 1 8 9) (location 1 9 10) #\n)
        (character-token (location 1 9 10) (location 1 10 11) #\g))
       #f
       #f))
     #f
     (character-token (location 1 11 12) (location 1 12 13) #\>)
     (list
      (character-token (location 1 10 11) (location 1 11 12) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\space))))))

; End tag tests
(module+ test
  (check-it
   "Simple ending tag"
   "</p>"
   (list
    (end-tag-token
     (location 1 0 1)
     (location 1 4 5)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 2 3) (location 1 3 4) #\p))
     '()
     #f
     (character-token (location 1 3 4) (location 1 4 5) #\>)
     '()
     (character-token (location 1 1 2) (location 1 2 3) #\/))))
  (check-it
   "End tag immediately ends"
   "</>"
   (list
    (missing-end-tag-name
     (location 1 2 3)
     (character-token (location 1 2 3) (location 1 3 4) #\>))
    (character-token (location 1 0 1) (location 1 1 2) '(#\< . #f))
    (character-token (location 1 1 2) (location 1 2 3) '(#\/ . #f))))
  (check-it
   "EOF in end tag open "
   "</"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)))
  (check-it
   "Tag is actually bogus comment"
   "</ foo>"
   (list
    (invalid-first-character-of-tag-name (location 1 2 3) #f)
    (comment-token
     (location 1 0 1)
     (location 1 6 7)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\/)
     #f
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\space)
      (character-token (location 1 3 4) (location 1 4 5) #\f)
      (character-token (location 1 4 5) (location 1 5 6) #\o)
      (character-token (location 1 5 6) (location 1 6 7) #\o))
     #t
     #f
     #f)
    (character-token (location 1 6 7) (location 1 7 8) #\>)))
  (check-it
   "End tag has attributes"
   "</a c='d'>"
   (list
    (end-tag-token
     (location 1 0 1)
     (location 1 10 11)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 2 3) (location 1 3 4) #\a))
     (list
      (attribute-token
       (location 1 3 4)
       (location 1 10 11)
       (list (character-token (location 1 4 5) (location 1 5 6) #\c))
       (character-token (location 1 5 6) (location 1 6 7) #\=)
       (quoted-attr-value
        (character-token (location 1 6 7) (location 1 7 8) #\')
        (list (character-token (location 1 7 8) (location 1 8 9) #\d))
        (character-token (location 1 8 9) (location 1 9 10) #\'))))
     #f
     (character-token (location 1 9 10) (location 1 10 11) #\>)
     (list (character-token (location 1 3 4) (location 1 4 5) #\space))
     (character-token (location 1 1 2) (location 1 2 3) #\/)))))
