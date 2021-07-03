#lang racket/base

(require laramie/tokenize)

(module+ test
  (require rackunit
           syntax/parse/define))

(module+ test
  (define-simple-macro (check-it test-name subject value)
    (let ([tokens (parameterize ([current-tag-name (list (character-token (location 1 0 0) (location 1 1 1) #\p))])
                    (tokenize subject
                              #:include-dropped? #t
                              #:include-errors? #t
                              #:initial-tokenizer RCDATA))])
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
   "RCDATA ends immediately"
   ""
   (list))
  (check-it
   "Null, then EOF"
   (format "~a" #\nul)
   (list
    (unexpected-null-character (location 1 0 1) #f)
    (character-token (location 1 0 1) (location 1 1 2) '(#\nul . #\�))))
  (check-it
   "Character reference"
   "&amp;"
   (list
    (character-reference-token
     (location 1 0 1)
     (location 1 5 6)
     (list
      (character-token (location 1 0 1) (location 1 1 2) #\&)
      (character-token (location 1 1 2) (location 1 2 3) #\a)
      (character-token (location 1 2 3) (location 1 3 4) #\m)
      (character-token (location 1 3 4) (location 1 4 5) #\p)
      (character-token (location 1 4 5) (location 1 5 6) #\;))
     #\&
     #f)))
  (check-it
   "Orphaned <"
   "<"
   (list (character-token (location 1 0 1) (location 1 1 2) #\<)))
  (check-it
   "ASCII text"
   "foo"
   (list (string-token (location 1 0 1) (location 1 3 4) "foo")))
  (check-it
   "Inappropriate closing tag"
   "jumbo</foo>"
   (list
    (string-token (location 1 0 1) (location 1 5 6) "jumbo")
    (character-token (location 1 5 6) (location 1 6 7) #\<)
    (character-token (location 1 6 7) (location 1 7 8) #\/)
    (character-token (location 1 7 8) (location 1 8 9) #\f)
    (character-token (location 1 8 9) (location 1 9 10) #\o)
    (character-token (location 1 9 10) (location 1 10 11) #\o)
    (character-token (location 1 10 11) (location 1 11 12) #\>)))
  (check-it
   "Appropriate closing tag"
   "what?</p>"
   (list
    (string-token (location 1 0 1) (location 1 5 6) "what?")
    (end-tag-token
     (location 1 5 6)
     (location 1 9 10)
     (character-token (location 1 5 6) (location 1 6 7) #\<)
     (list (character-token (location 1 7 8) (location 1 8 9) #\p))
     '()
     #f
     (character-token (location 1 8 9) (location 1 9 10) #\>)
     '()
     (character-token (location 1 6 7) (location 1 7 8) #\/))))
  (check-it
   "Appropriate closing tag (need to downcase)"
   "</P>"
   (list
    (end-tag-token
     (location 1 0 1)
     (location 1 4 5)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 2 3) (location 1 3 4) '(#\P . #\p)))
     '()
     #f
     (character-token (location 1 3 4) (location 1 4 5) #\>)
     '()
     (character-token (location 1 1 2) (location 1 2 3) #\/))))
  (check-it
   "Slash, then non-tag"
   "/wat"
   (list (string-token (location 1 0 1) (location 1 4 5) "/wat")))
  (check-it
   "Look like end tag, but isn't"
   "</1p>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)
    (string-token (location 1 2 3) (location 1 5 6) "1p>")))
  (check-it
   "Appropriate closing tag with whitespace"
   "</p >"
   (list
    (end-tag-token
     (location 1 0 1)
     (location 1 5 6)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 2 3) (location 1 3 4) #\p))
     '()
     #f
     (character-token (location 1 4 5) (location 1 5 6) #\>)
     (list (character-token (location 1 3 4) (location 1 4 5) #\space))
     (character-token (location 1 1 2) (location 1 2 3) #\/))))
  (check-it
   "Inappropriate closing tag with whitespace"
   "</q >"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)
    (character-token (location 1 2 3) (location 1 3 4) #\q)
    (character-token (location 1 3 4) (location 1 4 5) #\space)
    (string-token (location 1 4 5) (location 1 5 6) ">")))
  (check-it
   "Appropriate self-closing tag"
   "</p/>"
   (list
    (end-tag-token
     (location 1 0 1)
     (location 1 5 6)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (list (character-token (location 1 2 3) (location 1 3 4) #\p))
     '()
     (character-token (location 1 3 4) (location 1 4 5) #\/)
     (character-token (location 1 4 5) (location 1 5 6) #\>)
     '()
     (character-token (location 1 1 2) (location 1 2 3) #\/))))
  (check-it
   "Inappropriate self-closing tag"
   "</q/>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)
    (character-token (location 1 2 3) (location 1 3 4) #\q)
    (character-token (location 1 3 4) (location 1 4 5) #\/)
    (string-token (location 1 4 5) (location 1 5 6) ">")))
  (check-it
   "Weird closing tag"
   "</p1>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)
    (character-token (location 1 2 3) (location 1 3 4) #\p)
    (string-token (location 1 3 4) (location 1 5 6) "1>"))))
