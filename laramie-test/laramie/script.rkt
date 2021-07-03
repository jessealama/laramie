#lang racket/base

(require laramie/tokenize)

(module+ test
  (require rackunit
           racket/format
           racket/function
           syntax/parse/define))

(module+ test
  (define-simple-macro (check-it test-name subject value)
    (let ([tokens (parameterize ([current-tag-name (map (lambda (c)
                                                          (character-token (location 1 0 0) (location 1 0 1) c))
                                                        (string->list "script"))])
                    (tokenize subject
                              #:include-dropped? #t
                              #:include-errors? #t
                              #:initial-tokenizer script:data))])
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
   "No script content"
   "</script>"
   (list
    (end-tag-token
     (location 1 0 1)
     (location 1 9 10)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\s)
      (character-token (location 1 3 4) (location 1 4 5) #\c)
      (character-token (location 1 4 5) (location 1 5 6) #\r)
      (character-token (location 1 5 6) (location 1 6 7) #\i)
      (character-token (location 1 6 7) (location 1 7 8) #\p)
      (character-token (location 1 7 8) (location 1 8 9) #\t))
     '()
     #f
     (character-token (location 1 8 9) (location 1 9 10) #\>)
     '()
     (character-token (location 1 1 2) (location 1 2 3) #\/))))
  (check-it
   "Comparison, then end tag"
   "a < b;</script>"
   (list
    (string-token (location 1 0 1) (location 1 2 3) "a ")
    (character-token (location 1 2 3) (location 1 3 4) #\<)
    (character-token (location 1 3 4) (location 1 4 5) #\space)
    (string-token (location 1 4 5) (location 1 6 7) "b;")
    (end-tag-token
     (location 1 6 7)
     (location 1 15 16)
     (character-token (location 1 6 7) (location 1 7 8) #\<)
     (list
      (character-token (location 1 8 9) (location 1 9 10) #\s)
      (character-token (location 1 9 10) (location 1 10 11) #\c)
      (character-token (location 1 10 11) (location 1 11 12) #\r)
      (character-token (location 1 11 12) (location 1 12 13) #\i)
      (character-token (location 1 12 13) (location 1 13 14) #\p)
      (character-token (location 1 13 14) (location 1 14 15) #\t))
     '()
     #f
     (character-token (location 1 14 15) (location 1 15 16) #\>)
     '()
     (character-token (location 1 7 8) (location 1 8 9) #\/))))
  (check-it
   "Incorrect closing tag, then EOF"
   "</skript>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)
    (character-token (location 1 2 3) (location 1 3 4) #\s)
    (character-token (location 1 3 4) (location 1 4 5) #\k)
    (character-token (location 1 4 5) (location 1 5 6) #\r)
    (character-token (location 1 5 6) (location 1 6 7) #\i)
    (character-token (location 1 6 7) (location 1 7 8) #\p)
    (character-token (location 1 7 8) (location 1 8 9) #\t)
    (string-token (location 1 8 9) (location 1 9 10) ">")))
  (check-it
   "Incorrect closing tag, then correct closing tag"
   "</wat></script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)
    (character-token (location 1 2 3) (location 1 3 4) #\w)
    (character-token (location 1 3 4) (location 1 4 5) #\a)
    (character-token (location 1 4 5) (location 1 5 6) #\t)
    (string-token (location 1 5 6) (location 1 6 7) ">")
    (end-tag-token
     (location 1 6 7)
     (location 1 15 16)
     (character-token (location 1 6 7) (location 1 7 8) #\<)
     (list
      (character-token (location 1 8 9) (location 1 9 10) #\s)
      (character-token (location 1 9 10) (location 1 10 11) #\c)
      (character-token (location 1 10 11) (location 1 11 12) #\r)
      (character-token (location 1 11 12) (location 1 12 13) #\i)
      (character-token (location 1 12 13) (location 1 13 14) #\p)
      (character-token (location 1 13 14) (location 1 14 15) #\t))
     '()
     #f
     (character-token (location 1 14 15) (location 1 15 16) #\>)
     '()
     (character-token (location 1 7 8) (location 1 8 9) #\/))))
  (check-it
   "Null in script content"
   (format "~a</script>" #\nul)
   (list
    (unexpected-null-character (location 1 0 1) #f)
    (character-token (location 1 0 1) (location 1 1 2) '(#\nul . #\�))
    (end-tag-token
     (location 1 1 2)
     (location 1 10 11)
     (character-token (location 1 1 2) (location 1 2 3) #\<)
     (list
      (character-token (location 1 3 4) (location 1 4 5) #\s)
      (character-token (location 1 4 5) (location 1 5 6) #\c)
      (character-token (location 1 5 6) (location 1 6 7) #\r)
      (character-token (location 1 6 7) (location 1 7 8) #\i)
      (character-token (location 1 7 8) (location 1 8 9) #\p)
      (character-token (location 1 8 9) (location 1 9 10) #\t))
     '()
     #f
     (character-token (location 1 9 10) (location 1 10 11) #\>)
     '()
     (character-token (location 1 2 3) (location 1 3 4) #\/))))
  (check-it
   "<, then EOF"
   "<"
   (list (character-token (location 1 0 1) (location 1 1 2) #\<)))
  (check-it
   "Looks like escape, but isn't"
   "<!</script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (end-tag-token
     (location 1 2 3)
     (location 1 11 12)
     (character-token (location 1 2 3) (location 1 3 4) #\<)
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\s)
      (character-token (location 1 5 6) (location 1 6 7) #\c)
      (character-token (location 1 6 7) (location 1 7 8) #\r)
      (character-token (location 1 7 8) (location 1 8 9) #\i)
      (character-token (location 1 8 9) (location 1 9 10) #\p)
      (character-token (location 1 9 10) (location 1 10 11) #\t))
     '()
     #f
     (character-token (location 1 10 11) (location 1 11 12) #\>)
     '()
     (character-token (location 1 3 4) (location 1 4 5) #\/))))
  (check-it
   "Looks like end tag, but isn't"
   "</9</script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)
    (string-token (location 1 2 3) (location 1 3 4) "9")
    (end-tag-token
     (location 1 3 4)
     (location 1 12 13)
     (character-token (location 1 3 4) (location 1 4 5) #\<)
     (list
      (character-token (location 1 5 6) (location 1 6 7) #\s)
      (character-token (location 1 6 7) (location 1 7 8) #\c)
      (character-token (location 1 7 8) (location 1 8 9) #\r)
      (character-token (location 1 8 9) (location 1 9 10) #\i)
      (character-token (location 1 9 10) (location 1 10 11) #\p)
      (character-token (location 1 10 11) (location 1 11 12) #\t))
     '()
     #f
     (character-token (location 1 11 12) (location 1 12 13) #\>)
     '()
     (character-token (location 1 4 5) (location 1 5 6) #\/))))
  (check-it
   "End tag doesn't end (EOF)"
   "a<script"
   (list
    (string-token (location 1 0 1) (location 1 1 2) "a")
    (character-token (location 1 1 2) (location 1 2 3) #\<)
    (character-token (location 1 2 3) (location 1 3 4) #\s)
    (string-token (location 1 3 4) (location 1 8 9) "cript")))
  (check-it
   "End tag has whitespace and attribute"
   "x;</script a=b>"
   (list
    (string-token (location 1 0 1) (location 1 2 3) "x;")
    (end-tag-token
     (location 1 2 3)
     (location 1 15 16)
     (character-token (location 1 2 3) (location 1 3 4) #\<)
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\s)
      (character-token (location 1 5 6) (location 1 6 7) #\c)
      (character-token (location 1 6 7) (location 1 7 8) #\r)
      (character-token (location 1 7 8) (location 1 8 9) #\i)
      (character-token (location 1 8 9) (location 1 9 10) #\p)
      (character-token (location 1 9 10) (location 1 10 11) #\t))
     (list
      (attribute-token
       (location 1 10 11)
       (location 1 15 16)
       (list (character-token (location 1 11 12) (location 1 12 13) #\a))
       (character-token (location 1 12 13) (location 1 13 14) #\=)
       (quoted-attr-value
        #f
        (list (character-token (location 1 13 14) (location 1 14 15) #\b))
        #f)))
     #f
     (character-token (location 1 14 15) (location 1 15 16) #\>)
     (list (character-token (location 1 10 11) (location 1 11 12) #\space))
     (character-token (location 1 3 4) (location 1 4 5) #\/))))
  (check-it
   "End tag has whitespace"
   "z;<script </script>"
   (list
    (string-token (location 1 0 1) (location 1 2 3) "z;")
    (character-token (location 1 2 3) (location 1 3 4) #\<)
    (character-token (location 1 3 4) (location 1 4 5) #\s)
    (string-token (location 1 4 5) (location 1 10 11) "cript ")
    (end-tag-token
     (location 1 10 11)
     (location 1 19 20)
     (character-token (location 1 10 11) (location 1 11 12) #\<)
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\s)
      (character-token (location 1 13 14) (location 1 14 15) #\c)
      (character-token (location 1 14 15) (location 1 15 16) #\r)
      (character-token (location 1 15 16) (location 1 16 17) #\i)
      (character-token (location 1 16 17) (location 1 17 18) #\p)
      (character-token (location 1 17 18) (location 1 18 19) #\t))
     '()
     #f
     (character-token (location 1 18 19) (location 1 19 20) #\>)
     '()
     (character-token (location 1 11 12) (location 1 12 13) #\/))))
  (check-it
   "Uppercase in end tag"
   "y;</SCRIPT>"
   (list
    (string-token (location 1 0 1) (location 1 2 3) "y;")
    (end-tag-token
     (location 1 2 3)
     (location 1 11 12)
     (character-token (location 1 2 3) (location 1 3 4) #\<)
     (list
      (character-token (location 1 4 5) (location 1 5 6) '(#\S . #\s))
      (character-token (location 1 5 6) (location 1 6 7) '(#\C . #\c))
      (character-token (location 1 6 7) (location 1 7 8) '(#\R . #\r))
      (character-token (location 1 7 8) (location 1 8 9) '(#\I . #\i))
      (character-token (location 1 8 9) (location 1 9 10) '(#\P . #\p))
      (character-token (location 1 9 10) (location 1 10 11) '(#\T . #\t)))
     '()
     #f
     (character-token (location 1 10 11) (location 1 11 12) #\>)
     '()
     (character-token (location 1 3 4) (location 1 4 5) #\/))))
  (check-it
   "End tag doesn't quite end"
   "</script"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)
    (character-token (location 1 2 3) (location 1 3 4) #\s)
    (character-token (location 1 3 4) (location 1 4 5) #\c)
    (character-token (location 1 4 5) (location 1 5 6) #\r)
    (character-token (location 1 5 6) (location 1 6 7) #\i)
    (character-token (location 1 6 7) (location 1 7 8) #\p)
    (character-token (location 1 7 8) (location 1 8 9) #\t)))
  (check-it
   "Whitespace after inappropriate end tag"
   "</skript >"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)
    (character-token (location 1 2 3) (location 1 3 4) #\s)
    (character-token (location 1 3 4) (location 1 4 5) #\k)
    (character-token (location 1 4 5) (location 1 5 6) #\r)
    (character-token (location 1 5 6) (location 1 6 7) #\i)
    (character-token (location 1 6 7) (location 1 7 8) #\p)
    (character-token (location 1 7 8) (location 1 8 9) #\t)
    (string-token (location 1 8 9) (location 1 10 11) " >")))
  (check-it
   "Self-closing, but otherwise appropriate, end tag"
   "</script/>"
   (list
    (end-tag-token
     (location 1 0 1)
     (location 1 10 11)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\s)
      (character-token (location 1 3 4) (location 1 4 5) #\c)
      (character-token (location 1 4 5) (location 1 5 6) #\r)
      (character-token (location 1 5 6) (location 1 6 7) #\i)
      (character-token (location 1 6 7) (location 1 7 8) #\p)
      (character-token (location 1 7 8) (location 1 8 9) #\t))
     '()
     (character-token (location 1 8 9) (location 1 9 10) #\/)
     (character-token (location 1 9 10) (location 1 10 11) #\>)
     '()
     (character-token (location 1 1 2) (location 1 2 3) #\/))))
  (check-it
   "Self-closing inappropriate end tag"
   "</skript/>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)
    (character-token (location 1 2 3) (location 1 3 4) #\s)
    (character-token (location 1 3 4) (location 1 4 5) #\k)
    (character-token (location 1 4 5) (location 1 5 6) #\r)
    (character-token (location 1 5 6) (location 1 6 7) #\i)
    (character-token (location 1 6 7) (location 1 7 8) #\p)
    (character-token (location 1 7 8) (location 1 8 9) #\t)
    (string-token (location 1 8 9) (location 1 10 11) "/>")))
  (check-it
   "Non-alphabetic in ending script"
   "</scr1pt>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)
    (character-token (location 1 2 3) (location 1 3 4) #\s)
    (character-token (location 1 3 4) (location 1 4 5) #\c)
    (character-token (location 1 4 5) (location 1 5 6) #\r)
    (string-token (location 1 5 6) (location 1 9 10) "1pt>")))
  (check-it
   "EOF in escaped content"
   "<!"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)))
  (check-it
   "Escape dash, then EOF"
   "<!-"
   (list
   (character-token (location 1 0 1) (location 1 1 2) #\<)
   (character-token (location 1 1 2) (location 1 2 3) #\!)
   (character-token (location 1 2 3) (location 1 3 4) #\-)))
  (check-it
   "Escape dash dash, then EOF"
   "<!--"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (eof-in-script-html-comment-like-text (location 1 4 5) #f)))
  (check-it
   "Three dashes in escaped content"
   "<!---</script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\-)
    (end-tag-token
     (location 1 5 6)
     (location 1 14 15)
     (character-token (location 1 5 6) (location 1 6 7) #\<)
     (list
      (character-token (location 1 7 8) (location 1 8 9) #\s)
      (character-token (location 1 8 9) (location 1 9 10) #\c)
      (character-token (location 1 9 10) (location 1 10 11) #\r)
      (character-token (location 1 10 11) (location 1 11 12) #\i)
      (character-token (location 1 11 12) (location 1 12 13) #\p)
      (character-token (location 1 12 13) (location 1 13 14) #\t))
     '()
     #f
     (character-token (location 1 13 14) (location 1 14 15) #\>)
     '()
     (character-token (location 1 6 7) (location 1 7 8) #\/))))
  (check-it
   "Escaped <"
   "<!--<</script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (end-tag-token
     (location 1 5 6)
     (location 1 14 15)
     (character-token (location 1 5 6) (location 1 6 7) #\<)
     (list
      (character-token (location 1 7 8) (location 1 8 9) #\s)
      (character-token (location 1 8 9) (location 1 9 10) #\c)
      (character-token (location 1 9 10) (location 1 10 11) #\r)
      (character-token (location 1 10 11) (location 1 11 12) #\i)
      (character-token (location 1 11 12) (location 1 12 13) #\p)
      (character-token (location 1 12 13) (location 1 13 14) #\t))
     '()
     #f
     (character-token (location 1 13 14) (location 1 14 15) #\>)
     '()
     (character-token (location 1 6 7) (location 1 7 8) #\/))))
  (check-it
   "Escape, but nothing there"
   "<!--></script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\>)
    (end-tag-token
     (location 1 5 6)
     (location 1 14 15)
     (character-token (location 1 5 6) (location 1 6 7) #\<)
     (list
      (character-token (location 1 7 8) (location 1 8 9) #\s)
      (character-token (location 1 8 9) (location 1 9 10) #\c)
      (character-token (location 1 9 10) (location 1 10 11) #\r)
      (character-token (location 1 10 11) (location 1 11 12) #\i)
      (character-token (location 1 11 12) (location 1 12 13) #\p)
      (character-token (location 1 12 13) (location 1 13 14) #\t))
     '()
     #f
     (character-token (location 1 13 14) (location 1 14 15) #\>)
     '()
     (character-token (location 1 6 7) (location 1 7 8) #\/))))
  (check-it
   "Normal escaped content"
   "<!-- no --></script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 4 5) (location 1 5 6) #\space)
    (character-token (location 1 5 6) (location 1 6 7) #\n)
    (character-token (location 1 6 7) (location 1 7 8) #\o)
    (character-token (location 1 7 8) (location 1 8 9) #\space)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 8 9) (location 1 9 10) #\-)
    (character-token (location 1 9 10) (location 1 10 11) #\-)
    (character-token (location 1 10 11) (location 1 11 12) #\>)
    (end-tag-token
     (location 1 11 12)
     (location 1 20 21)
     (character-token (location 1 11 12) (location 1 12 13) #\<)
     (list
      (character-token (location 1 13 14) (location 1 14 15) #\s)
      (character-token (location 1 14 15) (location 1 15 16) #\c)
      (character-token (location 1 15 16) (location 1 16 17) #\r)
      (character-token (location 1 16 17) (location 1 17 18) #\i)
      (character-token (location 1 17 18) (location 1 18 19) #\p)
      (character-token (location 1 18 19) (location 1 19 20) #\t))
     '()
     #f
     (character-token (location 1 19 20) (location 1 20 21) #\>)
     '()
     (character-token (location 1 12 13) (location 1 13 14) #\/))))
  (check-it
   "Escape dash, then something else"
   "<!-x</script>"
   (list
   (character-token (location 1 0 1) (location 1 1 2) #\<)
   (character-token (location 1 1 2) (location 1 2 3) #\!)
   (character-token (location 1 2 3) (location 1 3 4) #\-)
   (string-token (location 1 3 4) (location 1 4 5) "x")
   (end-tag-token
    (location 1 4 5)
    (location 1 13 14)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (list
     (character-token (location 1 6 7) (location 1 7 8) #\s)
     (character-token (location 1 7 8) (location 1 8 9) #\c)
     (character-token (location 1 8 9) (location 1 9 10) #\r)
     (character-token (location 1 9 10) (location 1 10 11) #\i)
     (character-token (location 1 10 11) (location 1 11 12) #\p)
     (character-token (location 1 11 12) (location 1 12 13) #\t))
    '()
    #f
    (character-token (location 1 12 13) (location 1 13 14) #\>)
    '()
    (character-token (location 1 5 6) (location 1 6 7) #\/))))
  (check-it
   "Null begin to dash dash escaped content"
   (format "<!--~a></script>" #\nul)
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (unexpected-null-character (location 1 4 5) #f)
    (character-token (location 1 4 5) (location 1 5 6) '(#\nul . #\�))
    (character-token (location 1 5 6) (location 1 6 7) #\>)
    (end-tag-token
     (location 1 6 7)
     (location 1 15 16)
     (character-token (location 1 6 7) (location 1 7 8) #\<)
     (list
      (character-token (location 1 8 9) (location 1 9 10) #\s)
      (character-token (location 1 9 10) (location 1 10 11) #\c)
      (character-token (location 1 10 11) (location 1 11 12) #\r)
      (character-token (location 1 11 12) (location 1 12 13) #\i)
      (character-token (location 1 12 13) (location 1 13 14) #\p)
      (character-token (location 1 13 14) (location 1 14 15) #\t))
     '()
     #f
     (character-token (location 1 14 15) (location 1 15 16) #\>)
     '()
     (character-token (location 1 7 8) (location 1 8 9) #\/))))
  (check-it
   "Null after begin of dash dash escaped content"
   (format "<!-- ~a></script>" #\nul)
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 4 5) (location 1 5 6) #\space)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (unexpected-null-character (location 1 5 6) #f)
    (character-token (location 1 5 6) (location 1 6 7) '(#\nul . #\�))
    (character-token (location 1 6 7) (location 1 7 8) #\>)
    (end-tag-token
     (location 1 7 8)
     (location 1 16 17)
     (character-token (location 1 7 8) (location 1 8 9) #\<)
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\s)
      (character-token (location 1 10 11) (location 1 11 12) #\c)
      (character-token (location 1 11 12) (location 1 12 13) #\r)
      (character-token (location 1 12 13) (location 1 13 14) #\i)
      (character-token (location 1 13 14) (location 1 14 15) #\p)
      (character-token (location 1 14 15) (location 1 15 16) #\t))
     '()
     #f
     (character-token (location 1 15 16) (location 1 16 17) #\>)
     '()
     (character-token (location 1 8 9) (location 1 9 10) #\/))))
  (check-it
   "EOF in dash-dash < escaped content"
   "<!--<"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (eof-in-script-html-comment-like-text (location 1 5 6) #f)))
  (check-it
   "Double escape script"
   "<!--<script><!--<script>--></script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\!)
    (character-token (location 1 14 15) (location 1 15 16) #\-)
    (character-token (location 1 15 16) (location 1 16 17) #\-)
    (character-token (location 1 16 17) (location 1 17 18) #\<)
    (character-token (location 1 17 18) (location 1 18 19) #\s)
    (character-token (location 1 18 19) (location 1 19 20) #\c)
    (character-token (location 1 19 20) (location 1 20 21) #\r)
    (character-token (location 1 20 21) (location 1 21 22) #\i)
    (character-token (location 1 21 22) (location 1 22 23) #\p)
    (character-token (location 1 22 23) (location 1 23 24) #\t)
    (character-token (location 1 23 24) (location 1 24 25) #\>)
    (character-token (location 1 24 25) (location 1 25 26) #\-)
    (character-token (location 1 25 26) (location 1 26 27) #\-)
    (character-token (location 1 26 27) (location 1 27 28) #\>)
    (end-tag-token
     (location 1 27 28)
     (location 1 36 37)
     (character-token (location 1 27 28) (location 1 28 29) #\<)
     (list
      (character-token (location 1 29 30) (location 1 30 31) #\s)
      (character-token (location 1 30 31) (location 1 31 32) #\c)
      (character-token (location 1 31 32) (location 1 32 33) #\r)
      (character-token (location 1 32 33) (location 1 33 34) #\i)
      (character-token (location 1 33 34) (location 1 34 35) #\p)
      (character-token (location 1 34 35) (location 1 35 36) #\t))
     '()
     #f
     (character-token (location 1 35 36) (location 1 36 37) #\>)
     '()
     (character-token (location 1 28 29) (location 1 29 30) #\/))))
  (check-it
   "Double escape end script"
   "<!--</script>--></script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (end-tag-token
     (location 1 4 5)
     (location 1 13 14)
     (character-token (location 1 4 5) (location 1 5 6) #\<)
     (list
      (character-token (location 1 6 7) (location 1 7 8) #\s)
      (character-token (location 1 7 8) (location 1 8 9) #\c)
      (character-token (location 1 8 9) (location 1 9 10) #\r)
      (character-token (location 1 9 10) (location 1 10 11) #\i)
      (character-token (location 1 10 11) (location 1 11 12) #\p)
      (character-token (location 1 11 12) (location 1 12 13) #\t))
     '()
     #f
     (character-token (location 1 12 13) (location 1 13 14) #\>)
     '()
     (character-token (location 1 5 6) (location 1 6 7) #\/))
    (character-token (location 1 13 14) (location 1 14 15) #\-)
    (character-token (location 1 14 15) (location 1 15 16) #\-)
    (character-token (location 1 15 16) (location 1 16 17) #\>)
    (end-tag-token
     (location 1 16 17)
     (location 1 25 26)
     (character-token (location 1 16 17) (location 1 17 18) #\<)
     (list
      (character-token (location 1 18 19) (location 1 19 20) #\s)
      (character-token (location 1 19 20) (location 1 20 21) #\c)
      (character-token (location 1 20 21) (location 1 21 22) #\r)
      (character-token (location 1 21 22) (location 1 22 23) #\i)
      (character-token (location 1 22 23) (location 1 23 24) #\p)
      (character-token (location 1 23 24) (location 1 24 25) #\t))
     '()
     #f
     (character-token (location 1 24 25) (location 1 25 26) #\>)
     '()
     (character-token (location 1 17 18) (location 1 18 19) #\/))))
  (check-it
   "Double escape end script, EOF"
   "<!--</script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (end-tag-token
     (location 1 4 5)
     (location 1 13 14)
     (character-token (location 1 4 5) (location 1 5 6) #\<)
     (list
      (character-token (location 1 6 7) (location 1 7 8) #\s)
      (character-token (location 1 7 8) (location 1 8 9) #\c)
      (character-token (location 1 8 9) (location 1 9 10) #\r)
      (character-token (location 1 9 10) (location 1 10 11) #\i)
      (character-token (location 1 10 11) (location 1 11 12) #\p)
      (character-token (location 1 11 12) (location 1 12 13) #\t))
     '()
     #f
     (character-token (location 1 12 13) (location 1 13 14) #\>)
     '()
     (character-token (location 1 5 6) (location 1 6 7) #\/))))
  (check-it
   "Escaped open tag"
   "<!--</skrip></script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) #\/)
    (character-token (location 1 6 7) (location 1 7 8) #\s)
    (character-token (location 1 7 8) (location 1 8 9) #\k)
    (character-token (location 1 8 9) (location 1 9 10) #\r)
    (character-token (location 1 9 10) (location 1 10 11) #\i)
    (character-token (location 1 10 11) (location 1 11 12) #\p)
    (character-token (location 1 11 12) (location 1 12 13) #\>)
    (end-tag-token
     (location 1 12 13)
     (location 1 21 22)
     (character-token (location 1 12 13) (location 1 13 14) #\<)
     (list
      (character-token (location 1 14 15) (location 1 15 16) #\s)
      (character-token (location 1 15 16) (location 1 16 17) #\c)
      (character-token (location 1 16 17) (location 1 17 18) #\r)
      (character-token (location 1 17 18) (location 1 18 19) #\i)
      (character-token (location 1 18 19) (location 1 19 20) #\p)
      (character-token (location 1 19 20) (location 1 20 21) #\t))
     '()
     #f
     (character-token (location 1 20 21) (location 1 21 22) #\>)
     '()
     (character-token (location 1 13 14) (location 1 14 15) #\/))))
  (check-it
   "EOF in escaped open tag"
   "<!--</p"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) #\/)
    (character-token (location 1 6 7) (location 1 7 8) #\p)
    (eof-in-script-html-comment-like-text (location 1 7 8) #f)))
  (check-it
   "Non-alphabetic in escaped tag"
   "<!--<1</script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) #\1)
    (end-tag-token
     (location 1 6 7)
     (location 1 15 16)
     (character-token (location 1 6 7) (location 1 7 8) #\<)
     (list
      (character-token (location 1 8 9) (location 1 9 10) #\s)
      (character-token (location 1 9 10) (location 1 10 11) #\c)
      (character-token (location 1 10 11) (location 1 11 12) #\r)
      (character-token (location 1 11 12) (location 1 12 13) #\i)
      (character-token (location 1 12 13) (location 1 13 14) #\p)
      (character-token (location 1 13 14) (location 1 14 15) #\t))
     '()
     #f
     (character-token (location 1 14 15) (location 1 15 16) #\>)
     '()
     (character-token (location 1 7 8) (location 1 8 9) #\/))))
  (check-it
   "Escaped dash, then EOF"
   "<!-- - --></script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 4 5) (location 1 5 6) #\space)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 6 7) (location 1 7 8) #\space)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 5 6) (location 1 6 7) #\-)
    (character-token (location 1 7 8) (location 1 8 9) #\-)
    (character-token (location 1 8 9) (location 1 9 10) #\-)
    (character-token (location 1 9 10) (location 1 10 11) #\>)
    (end-tag-token
     (location 1 10 11)
     (location 1 19 20)
     (character-token (location 1 10 11) (location 1 11 12) #\<)
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\s)
      (character-token (location 1 13 14) (location 1 14 15) #\c)
      (character-token (location 1 14 15) (location 1 15 16) #\r)
      (character-token (location 1 15 16) (location 1 16 17) #\i)
      (character-token (location 1 16 17) (location 1 17 18) #\p)
      (character-token (location 1 17 18) (location 1 18 19) #\t))
     '()
     #f
     (character-token (location 1 18 19) (location 1 19 20) #\>)
     '()
     (character-token (location 1 11 12) (location 1 12 13) #\/))))
  (check-it
   "Escaped dash script"
   "<!-- -<wat</script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 4 5) (location 1 5 6) #\space)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 5 6) (location 1 6 7) #\-)
    (character-token (location 1 6 7) (location 1 7 8) #\<)
    (character-token (location 1 7 8) (location 1 8 9) #\w)
    (character-token (location 1 8 9) (location 1 9 10) #\a)
    (character-token (location 1 9 10) (location 1 10 11) #\t)
    (end-tag-token
     (location 1 10 11)
     (location 1 19 20)
     (character-token (location 1 10 11) (location 1 11 12) #\<)
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\s)
      (character-token (location 1 13 14) (location 1 14 15) #\c)
      (character-token (location 1 14 15) (location 1 15 16) #\r)
      (character-token (location 1 15 16) (location 1 16 17) #\i)
      (character-token (location 1 16 17) (location 1 17 18) #\p)
      (character-token (location 1 17 18) (location 1 18 19) #\t))
     '()
     #f
     (character-token (location 1 18 19) (location 1 19 20) #\>)
     '()
     (character-token (location 1 11 12) (location 1 12 13) #\/))))
  (check-it
   "EOF in dash-dash < escaped content"
   "<!--</"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) #\/)
    (eof-in-script-html-comment-like-text (location 1 6 7) #f)))
  (check-it
   "Non-alphabetic in escaped tag"
   "<!--</1</script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) #\/)
    (character-token (location 1 6 7) (location 1 7 8) #\1)
    (end-tag-token
     (location 1 7 8)
     (location 1 16 17)
     (character-token (location 1 7 8) (location 1 8 9) #\<)
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\s)
      (character-token (location 1 10 11) (location 1 11 12) #\c)
      (character-token (location 1 11 12) (location 1 12 13) #\r)
      (character-token (location 1 12 13) (location 1 13 14) #\i)
      (character-token (location 1 13 14) (location 1 14 15) #\p)
      (character-token (location 1 14 15) (location 1 15 16) #\t))
     '()
     #f
     (character-token (location 1 15 16) (location 1 16 17) #\>)
     '()
     (character-token (location 1 8 9) (location 1 9 10) #\/))))
  (check-it
   "Escaped appropriate end tag"
   "<!--</script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (end-tag-token
     (location 1 4 5)
     (location 1 13 14)
     (character-token (location 1 4 5) (location 1 5 6) #\<)
     (list
      (character-token (location 1 6 7) (location 1 7 8) #\s)
      (character-token (location 1 7 8) (location 1 8 9) #\c)
      (character-token (location 1 8 9) (location 1 9 10) #\r)
      (character-token (location 1 9 10) (location 1 10 11) #\i)
      (character-token (location 1 10 11) (location 1 11 12) #\p)
      (character-token (location 1 11 12) (location 1 12 13) #\t))
     '()
     #f
     (character-token (location 1 12 13) (location 1 13 14) #\>)
     '()
     (character-token (location 1 5 6) (location 1 6 7) #\/))))
  (check-it
   "Escaped inappropriate end tag"
   "<!--</skript>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) #\/)
    (character-token (location 1 6 7) (location 1 7 8) #\s)
    (character-token (location 1 7 8) (location 1 8 9) #\k)
    (character-token (location 1 8 9) (location 1 9 10) #\r)
    (character-token (location 1 9 10) (location 1 10 11) #\i)
    (character-token (location 1 10 11) (location 1 11 12) #\p)
    (character-token (location 1 11 12) (location 1 12 13) #\t)
    (eof-in-script-html-comment-like-text (location 1 13 14) #f)
    (character-token (location 1 12 13) (location 1 13 14) '(#\> . #f))))
  (check-it
   "Escaped self-closing appropriate end tag"
   "<!--</script/>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (end-tag-token
     (location 1 4 5)
     (location 1 14 15)
     (character-token (location 1 4 5) (location 1 5 6) #\<)
     (list
      (character-token (location 1 6 7) (location 1 7 8) #\s)
      (character-token (location 1 7 8) (location 1 8 9) #\c)
      (character-token (location 1 8 9) (location 1 9 10) #\r)
      (character-token (location 1 9 10) (location 1 10 11) #\i)
      (character-token (location 1 10 11) (location 1 11 12) #\p)
      (character-token (location 1 11 12) (location 1 12 13) #\t))
     '()
     (character-token (location 1 12 13) (location 1 13 14) #\/)
     (character-token (location 1 13 14) (location 1 14 15) #\>)
     '()
     (character-token (location 1 5 6) (location 1 6 7) #\/))))
  (check-it
   "Escaped self-closing inappropriate end tag"
   "<!--</foo/>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) #\/)
    (character-token (location 1 6 7) (location 1 7 8) #\f)
    (character-token (location 1 7 8) (location 1 8 9) #\o)
    (character-token (location 1 8 9) (location 1 9 10) #\o)
    (eof-in-script-html-comment-like-text (location 1 11 12) #f)
    (character-token (location 1 9 10) (location 1 10 11) '(#\/ . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\> . #f))))
  (check-it
   "Escaped end tag with upper"
   "<!--</SCRIPT>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (end-tag-token
     (location 1 4 5)
     (location 1 13 14)
     (character-token (location 1 4 5) (location 1 5 6) #\<)
     (list
      (character-token (location 1 6 7) (location 1 7 8) '(#\S . #\s))
      (character-token (location 1 7 8) (location 1 8 9) '(#\C . #\c))
      (character-token (location 1 8 9) (location 1 9 10) '(#\R . #\r))
      (character-token (location 1 9 10) (location 1 10 11) '(#\I . #\i))
      (character-token (location 1 10 11) (location 1 11 12) '(#\P . #\p))
      (character-token (location 1 11 12) (location 1 12 13) '(#\T . #\t)))
     '()
     #f
     (character-token (location 1 12 13) (location 1 13 14) #\>)
     '()
     (character-token (location 1 5 6) (location 1 6 7) #\/))))
  (check-it
   "Escaped end tag with non-alphabetic"
   "<!--</scr1pt>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) #\/)
    (character-token (location 1 6 7) (location 1 7 8) #\s)
    (character-token (location 1 7 8) (location 1 8 9) #\c)
    (character-token (location 1 8 9) (location 1 9 10) #\r)
    (eof-in-script-html-comment-like-text (location 1 13 14) #f)
    (character-token (location 1 9 10) (location 1 10 11) '(#\1 . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\p . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\t . #f))
    (character-token (location 1 12 13) (location 1 13 14) '(#\> . #f))))
  (check-it
   "Whitespace in escaped appropriate end tag"
   "<!--</script >"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (end-tag-token
     (location 1 4 5)
     (location 1 14 15)
     (character-token (location 1 4 5) (location 1 5 6) #\<)
     (list
      (character-token (location 1 6 7) (location 1 7 8) #\s)
      (character-token (location 1 7 8) (location 1 8 9) #\c)
      (character-token (location 1 8 9) (location 1 9 10) #\r)
      (character-token (location 1 9 10) (location 1 10 11) #\i)
      (character-token (location 1 10 11) (location 1 11 12) #\p)
      (character-token (location 1 11 12) (location 1 12 13) #\t))
     '()
     #f
     (character-token (location 1 13 14) (location 1 14 15) #\>)
     (list (character-token (location 1 12 13) (location 1 13 14) #\space))
     (character-token (location 1 5 6) (location 1 6 7) #\/))))
  (check-it
   "Whitespace in escaped inappropriate end tag"
   "<!--</foo >"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) #\/)
    (character-token (location 1 6 7) (location 1 7 8) #\f)
    (character-token (location 1 7 8) (location 1 8 9) #\o)
    (character-token (location 1 8 9) (location 1 9 10) #\o)
    (eof-in-script-html-comment-like-text (location 1 11 12) #f)
    (character-token (location 1 9 10) (location 1 10 11) '(#\space . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\> . #f))))
  (check-it
   "EOF in escaped dash (1)"
   "<!---"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\-)
    (eof-in-script-html-comment-like-text (location 1 5 6) #f)))
  (check-it
   "Null in escaped dash"
   (format "<!---~ab" #\nul)
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\-)
    (unexpected-null-character (location 1 5 6) #f)
    (character-token (location 1 5 6) (location 1 6 7) '(#\nul . #\�))
    (eof-in-script-html-comment-like-text (location 1 7 8) #f)
    (character-token (location 1 6 7) (location 1 7 8) '(#\b . #f))))
  (check-it
   "Uppercase in double escape (1)"
   "<!--<script><!--<A></script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\!)
    (character-token (location 1 14 15) (location 1 15 16) #\-)
    (character-token (location 1 15 16) (location 1 16 17) #\-)
    (character-token (location 1 16 17) (location 1 17 18) #\<)
    (character-token (location 1 17 18) (location 1 18 19) #\A)
    (character-token (location 1 18 19) (location 1 19 20) #\>)
    (character-token (location 1 19 20) (location 1 20 21) #\<)
    (character-token (location 1 20 21) (location 1 21 22) #\/)
    (character-token (location 1 21 22) (location 1 22 23) #\s)
    (character-token (location 1 22 23) (location 1 23 24) #\c)
    (character-token (location 1 23 24) (location 1 24 25) #\r)
    (character-token (location 1 24 25) (location 1 25 26) #\i)
    (character-token (location 1 25 26) (location 1 26 27) #\p)
    (character-token (location 1 26 27) (location 1 27 28) #\t)
    (character-token (location 1 27 28) (location 1 28 29) #\>)
    (eof-in-script-html-comment-like-text (location 1 28 29) #f)))
  (check-it
   "EOF in double escaped"
   "<!--<script><!--"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\!)
    (character-token (location 1 14 15) (location 1 15 16) #\-)
    (character-token (location 1 15 16) (location 1 16 17) #\-)
    (eof-in-script-html-comment-like-text (location 1 16 17) #f)))
  (check-it
   "EOF in double escaped dash <"
   "<!--<script><!--<a"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\!)
    (character-token (location 1 14 15) (location 1 15 16) #\-)
    (character-token (location 1 15 16) (location 1 16 17) #\-)
    (character-token (location 1 16 17) (location 1 17 18) #\<)
    (character-token (location 1 17 18) (location 1 18 19) #\a)
    (eof-in-script-html-comment-like-text (location 1 18 19) #f)))
  (check-it
   "Null in double-escaped dash <"
   (format "<!--<script><!--~a" #\nul)
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\!)
    (character-token (location 1 14 15) (location 1 15 16) #\-)
    (character-token (location 1 15 16) (location 1 16 17) #\-)
    (unexpected-null-character (location 1 16 17) #f)
    (character-token (location 1 16 17) (location 1 17 18) '(#\nul . #\�))
    (eof-in-script-html-comment-like-text (location 1 17 18) #f)))
  (check-it
   "Double-escaped dash dash"
   "<!--<script><--"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\-)
    (character-token (location 1 14 15) (location 1 15 16) #\-)
    (eof-in-script-html-comment-like-text (location 1 15 16) #f)))
  (check-it
   "EOF in escaped dash (2)"
   "<!--<--"
   (list
   (character-token (location 1 0 1) (location 1 1 2) #\<)
   (character-token (location 1 1 2) (location 1 2 3) #\!)
   (character-token (location 1 2 3) (location 1 3 4) #\-)
   (character-token (location 1 3 4) (location 1 4 5) #\-)
   (character-token (location 1 4 5) (location 1 5 6) #\<)
   (character-token (location 1 5 6) (location 1 6 7) #\-)
   (character-token (location 1 6 7) (location 1 7 8) #\-)
   (eof-in-script-html-comment-like-text (location 1 7 8) #f)))
  (check-it
   "Null in escaped dash"
   (format "<!--<--~ab" #\nul)
   (list
   (character-token (location 1 0 1) (location 1 1 2) #\<)
   (character-token (location 1 1 2) (location 1 2 3) #\!)
   (character-token (location 1 2 3) (location 1 3 4) #\-)
   (character-token (location 1 3 4) (location 1 4 5) #\-)
   (character-token (location 1 4 5) (location 1 5 6) #\<)
   (character-token (location 1 5 6) (location 1 6 7) #\-)
   (character-token (location 1 6 7) (location 1 7 8) #\-)
   (unexpected-null-character (location 1 7 8) #f)
   (character-token (location 1 7 8) (location 1 8 9) '(#\nul . #\�))
   (eof-in-script-html-comment-like-text (location 1 9 10) #f)
   (character-token (location 1 8 9) (location 1 9 10) '(#\b . #f))))
  (check-it
   "EOF after double escape start"
   "<!--<script><"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (eof-in-script-html-comment-like-text (location 1 13 14) #f)))
  (check-it
   "Uppercase in double escaped"
   "<!--<script><A>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\A)
    (character-token (location 1 14 15) (location 1 15 16) #\>)
    (eof-in-script-html-comment-like-text (location 1 15 16) #f)))
  (check-it
   "Null in double escaped"
   (format "<!--<script><~a" #\nul)
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (unexpected-null-character (location 1 13 14) #f)
    (character-token (location 1 13 14) (location 1 14 15) '(#\nul . #\�))
    (eof-in-script-html-comment-like-text (location 1 14 15) #f)))
  (check-it
   "EOF in double escaped dash"
   "<!--<script><-"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\-)
    (eof-in-script-html-comment-like-text (location 1 14 15) #f)))
  (check-it
   "Double-escaped less than"
   "<!--<script><-<"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\-)
    (character-token (location 1 14 15) (location 1 15 16) #\<)
    (eof-in-script-html-comment-like-text (location 1 15 16) #f)))
  (check-it
   "Null in double-escaped dash"
   (format "<!--<script><-~ax" #\nul)
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\-)
    (unexpected-null-character (location 1 14 15) #f)
    (character-token (location 1 14 15) (location 1 15 16) '(#\nul . #\�))
    (character-token (location 1 15 16) (location 1 16 17) #\x)
    (eof-in-script-html-comment-like-text (location 1 16 17) #f)))
  (check-it
   "Double escape reverts to single escape"
   "<!--<script><!--</script>--></script>"
   (list
   (character-token (location 1 0 1) (location 1 1 2) #\<)
   (character-token (location 1 1 2) (location 1 2 3) #\!)
   (character-token (location 1 2 3) (location 1 3 4) #\-)
   (character-token (location 1 3 4) (location 1 4 5) #\-)
   (character-token (location 1 4 5) (location 1 5 6) #\<)
   (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
   (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
   (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
   (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
   (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
   (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
   (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
   (character-token (location 1 12 13) (location 1 13 14) #\<)
   (character-token (location 1 13 14) (location 1 14 15) #\!)
   (character-token (location 1 14 15) (location 1 15 16) #\-)
   (character-token (location 1 15 16) (location 1 16 17) #\-)
   (character-token (location 1 16 17) (location 1 17 18) #\<)
   (character-token (location 1 17 18) (location 1 18 19) #\/)
   (character-token (location 1 18 19) (location 1 19 20) #\s)
   (character-token (location 1 19 20) (location 1 20 21) #\c)
   (character-token (location 1 20 21) (location 1 21 22) #\r)
   (character-token (location 1 21 22) (location 1 22 23) #\i)
   (character-token (location 1 22 23) (location 1 23 24) #\p)
   (character-token (location 1 23 24) (location 1 24 25) #\t)
   (character-token (location 1 24 25) (location 1 25 26) #\>)
   (character-token (location 1 25 26) (location 1 26 27) #\-)
   (character-token (location 1 26 27) (location 1 27 28) #\-)
   (character-token (location 1 27 28) (location 1 28 29) #\>)
   (end-tag-token
    (location 1 28 29)
    (location 1 37 38)
    (character-token (location 1 28 29) (location 1 29 30) #\<)
    (list
     (character-token (location 1 30 31) (location 1 31 32) #\s)
     (character-token (location 1 31 32) (location 1 32 33) #\c)
     (character-token (location 1 32 33) (location 1 33 34) #\r)
     (character-token (location 1 33 34) (location 1 34 35) #\i)
     (character-token (location 1 34 35) (location 1 35 36) #\p)
     (character-token (location 1 35 36) (location 1 36 37) #\t))
    '()
    #f
    (character-token (location 1 36 37) (location 1 37 38) #\>)
    '()
    (character-token (location 1 29 30) (location 1 30 31) #\/))))
  (check-it
   "Double escape triple dash"
   "<!--<script><---"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\-)
    (character-token (location 1 14 15) (location 1 15 16) #\-)
    (character-token (location 1 15 16) (location 1 16 17) #\-)
    (eof-in-script-html-comment-like-text (location 1 16 17) #f)))
  (check-it
   "Double escape dash dash reverts to double escape"
   "<!--<script><!-- xyz </script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\!)
    (character-token (location 1 14 15) (location 1 15 16) #\-)
    (character-token (location 1 15 16) (location 1 16 17) #\-)
    (character-token (location 1 16 17) (location 1 17 18) #\space)
    (character-token (location 1 17 18) (location 1 18 19) #\x)
    (character-token (location 1 18 19) (location 1 19 20) #\y)
    (character-token (location 1 19 20) (location 1 20 21) #\z)
    (character-token (location 1 20 21) (location 1 21 22) #\space)
    (character-token (location 1 21 22) (location 1 22 23) #\<)
    (character-token (location 1 22 23) (location 1 23 24) #\/)
    (character-token (location 1 23 24) (location 1 24 25) #\s)
    (character-token (location 1 24 25) (location 1 25 26) #\c)
    (character-token (location 1 25 26) (location 1 26 27) #\r)
    (character-token (location 1 26 27) (location 1 27 28) #\i)
    (character-token (location 1 27 28) (location 1 28 29) #\p)
    (character-token (location 1 28 29) (location 1 29 30) #\t)
    (character-token (location 1 29 30) (location 1 30 31) #\>)
    (eof-in-script-html-comment-like-text (location 1 30 31) #f)))
  (check-it
   "Inapprpriate tag in double escape"
   "<!--<script><!--<foo>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\!)
    (character-token (location 1 14 15) (location 1 15 16) #\-)
    (character-token (location 1 15 16) (location 1 16 17) #\-)
    (character-token (location 1 16 17) (location 1 17 18) #\<)
    (character-token (location 1 17 18) (location 1 18 19) #\f)
    (character-token (location 1 18 19) (location 1 19 20) #\o)
    (character-token (location 1 19 20) (location 1 20 21) #\o)
    (character-token (location 1 20 21) (location 1 21 22) #\>)
    (eof-in-script-html-comment-like-text (location 1 21 22) #f)))
  (check-it
   "Uppercase in double escape (2)"
   "<!--<script><!--<SCRIPT></script>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\!)
    (character-token (location 1 14 15) (location 1 15 16) #\-)
    (character-token (location 1 15 16) (location 1 16 17) #\-)
    (character-token (location 1 16 17) (location 1 17 18) #\<)
    (character-token (location 1 17 18) (location 1 18 19) #\S)
    (character-token (location 1 18 19) (location 1 19 20) #\C)
    (character-token (location 1 19 20) (location 1 20 21) #\R)
    (character-token (location 1 20 21) (location 1 21 22) #\I)
    (character-token (location 1 21 22) (location 1 22 23) #\P)
    (character-token (location 1 22 23) (location 1 23 24) #\T)
    (character-token (location 1 23 24) (location 1 24 25) #\>)
    (character-token (location 1 24 25) (location 1 25 26) #\<)
    (character-token (location 1 25 26) (location 1 26 27) #\/)
    (character-token (location 1 26 27) (location 1 27 28) #\s)
    (character-token (location 1 27 28) (location 1 28 29) #\c)
    (character-token (location 1 28 29) (location 1 29 30) #\r)
    (character-token (location 1 29 30) (location 1 30 31) #\i)
    (character-token (location 1 30 31) (location 1 31 32) #\p)
    (character-token (location 1 31 32) (location 1 32 33) #\t)
    (character-token (location 1 32 33) (location 1 33 34) #\>)
    (eof-in-script-html-comment-like-text (location 1 33 34) #f)))
  (check-it
   "Double escape ends"
   "<!--<script><!--</"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\!)
    (character-token (location 1 14 15) (location 1 15 16) #\-)
    (character-token (location 1 15 16) (location 1 16 17) #\-)
    (character-token (location 1 16 17) (location 1 17 18) #\<)
    (character-token (location 1 17 18) (location 1 18 19) #\/)
    (eof-in-script-html-comment-like-text (location 1 18 19) #f)))
  (check-it
   "Escape dash, then EOF"
   "<!-- -"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 4 5) (location 1 5 6) #\space)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 5 6) (location 1 6 7) #\-)
    (eof-in-script-html-comment-like-text (location 1 6 7) #f)))
  (check-it
   "Escape dash with null"
   (format "<!-- -~a" #\nul)
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 4 5) (location 1 5 6) #\space)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 5 6) (location 1 6 7) #\-)
    (unexpected-null-character (location 1 6 7) #f)
    (character-token (location 1 6 7) (location 1 7 8) '(#\nul . #\�))
    (eof-in-script-html-comment-like-text (location 1 7 8) #f)))
  (check-it
   "Escaped inappropriate tag"
   "<!--<script><--<FOO>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\-)
    (character-token (location 1 14 15) (location 1 15 16) #\-)
    (character-token (location 1 15 16) (location 1 16 17) #\<)
    (character-token (location 1 16 17) (location 1 17 18) #\F)
    (character-token (location 1 17 18) (location 1 18 19) #\O)
    (character-token (location 1 18 19) (location 1 19 20) #\O)
    (character-token (location 1 19 20) (location 1 20 21) #\>)
    (eof-in-script-html-comment-like-text (location 1 20 21) #f)))
  (check-it
   "ending non-</script>"
   "<!--</a>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) #\/)
    (character-token (location 1 6 7) (location 1 7 8) #\a)
    (eof-in-script-html-comment-like-text (location 1 8 9) #f)
    (character-token (location 1 7 8) (location 1 8 9) '(#\> . #f))))
  (check-it
   "uppercase double escape"
   "<!--<SCRIPT>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\S . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\C . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\R . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\I . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\P . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\T . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (eof-in-script-html-comment-like-text (location 1 12 13) #f)))
  (check-it
   "another double escape"
   "<!--<script></html>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\/)
    (character-token (location 1 14 15) (location 1 15 16) #\h)
    (character-token (location 1 15 16) (location 1 16 17) #\t)
    (character-token (location 1 16 17) (location 1 17 18) #\m)
    (character-token (location 1 17 18) (location 1 18 19) #\l)
    (character-token (location 1 18 19) (location 1 19 20) #\>)
    (eof-in-script-html-comment-like-text (location 1 19 20) #f)))
  (check-it
   "closing double escape"
   "<!--<script></HTML>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\/)
    (character-token (location 1 14 15) (location 1 15 16) '(#\H . #\h))
    (character-token (location 1 15 16) (location 1 16 17) '(#\T . #\t))
    (character-token (location 1 16 17) (location 1 17 18) '(#\M . #\m))
    (character-token (location 1 17 18) (location 1 18 19) '(#\L . #\l))
    (character-token (location 1 18 19) (location 1 19 20) #\>)
    (eof-in-script-html-comment-like-text (location 1 19 20) #f)))
  (check-it
   "Double escape bails out"
   "<!--<script></1"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\!)
    (character-token (location 1 2 3) (location 1 3 4) #\-)
    (character-token (location 1 3 4) (location 1 4 5) #\-)
    (character-token (location 1 4 5) (location 1 5 6) #\<)
    (character-token (location 1 5 6) (location 1 6 7) '(#\s . #f))
    (character-token (location 1 6 7) (location 1 7 8) '(#\c . #f))
    (character-token (location 1 7 8) (location 1 8 9) '(#\r . #f))
    (character-token (location 1 8 9) (location 1 9 10) '(#\i . #f))
    (character-token (location 1 9 10) (location 1 10 11) '(#\p . #f))
    (character-token (location 1 10 11) (location 1 11 12) '(#\t . #f))
    (character-token (location 1 11 12) (location 1 12 13) '(#\> . #f))
    (character-token (location 1 12 13) (location 1 13 14) #\<)
    (character-token (location 1 13 14) (location 1 14 15) #\/)
    (character-token (location 1 14 15) (location 1 15 16) #\1)
    (eof-in-script-html-comment-like-text (location 1 15 16) #f))))
