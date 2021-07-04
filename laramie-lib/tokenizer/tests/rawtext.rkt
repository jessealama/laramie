#lang racket/base

(require racket/require
         (multi-in ".."
                   ("types.rkt"
                    "tokenize.rkt"
                    "tokens.rkt"
                    "rawtext.rkt"
                    "parameters.rkt")))

(module+ test
  (require rackunit
           racket/format
           syntax/parse/define))

(module+ test
  (define-simple-macro (check-it test-name subject value)
    (let ([tokens (parameterize ([current-tag-name (list (character-token (location 1 0 0) (location 1 0 1) #\p))])
                    (tokenize subject
                              #:include-dropped? #t
                              #:include-errors? #t
                              #:initial-tokenizer RAWTEXT))])
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
   "Empty rawtext"
   ""
   (list))
  (check-it
   "Orphaned <"
   "<"
   (list (character-token (location 1 0 1) (location 1 1 2) #\<)))
  (check-it
   "Null"
   (format "~a" #\nul)
   (list
    (unexpected-null-character (location 1 0 1) #f)
    (character-token (location 1 0 1) (location 1 1 2) '(#\nul . #\�))))
  (check-it
   "Simple string"
   "hey"
   (list (string-token (location 1 0 1) (location 1 3 4) "hey")))
  (check-it
   "Simple tag, appropriate"
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
   "Simple tag, inappropriate"
   "</q>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)
    (character-token (location 1 2 3) (location 1 3 4) #\q)
    (string-token (location 1 3 4) (location 1 4 5) ">")))
  (check-it
   "< space"
   "< x"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (string-token (location 1 1 2) (location 1 3 4) " x")))
  (check-it
   "Non-alpha in tag"
   "<a1>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (string-token (location 1 1 2) (location 1 4 5) "a1>")))
  (check-it
   "EOF on end tag open"
   "</"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)))
  (check-it
   "Non-tag"
   "</1>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)
    (string-token (location 1 2 3) (location 1 4 5) "1>")))
  (check-it
   "Whitespace after tag name (appropriate)"
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
   "Whitespace after tag name (not appropriate)"
   "</q >"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)
    (character-token (location 1 2 3) (location 1 3 4) #\q)
    (string-token (location 1 3 4) (location 1 5 6) " >")))
  (check-it
   "Self-closing end tag (appropriate)"
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
   "Self-closing end tag (not appropriate)"
   "</q/>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)
    (character-token (location 1 2 3) (location 1 3 4) #\q)
    (string-token (location 1 3 4) (location 1 5 6) "/>")))
  (check-it
   "Uppercase in end tag"
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
   "Non-alpha in end tag"
   "</a1>"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\<)
    (character-token (location 1 1 2) (location 1 2 3) #\/)
    (character-token (location 1 2 3) (location 1 3 4) #\a)
    (string-token (location 1 3 4) (location 1 5 6) "1>"))))
