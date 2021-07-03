#lang racket/base

(require laramie)

(module+ test
  (require rackunit
           syntax/parse/define))

(module+ test
  (define-simple-macro (check-it test-name subject value)
    (let ([tokens (tokenize subject
                            #:include-dropped? #t
                            #:include-errors? #t
                            #:initial-tokenizer CDATA)])
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
  (check-it "CDATA ends immediately"
            ""
            (list (eof-in-cdata (location 1 0 1) #f)))
  (check-it "Null, then EOF"
            (format "~a" #\nul)
            (list
             (string-token (location 1 0 1) (location 1 1 2) "\u0000")
             (eof-in-cdata (location 1 1 2) #f)))
  (check-it "] as CDATA content"
            "]what]]>"
            (list
             (character-token (location 1 0 1) (location 1 1 2) #\])
             (string-token (location 1 1 2) (location 1 5 6) "what")
             (character-token (location 1 5 6) (location 1 6 7) '(#\] . #f))
             (character-token (location 1 6 7) (location 1 7 8) '(#\] . #f))
             (character-token (location 1 7 8) (location 1 8 9) '(#\> . #f))))
  (check-it "Ends with ]"
            "]"
            (list
             (character-token (location 1 0 1) (location 1 1 2) #\])
             (eof-in-cdata (location 1 1 2) #f)))
  (check-it "Simple CDATA"
            "hi!]]>x"
            (list
             (string-token (location 1 0 1) (location 1 3 4) "hi!")
             (character-token (location 1 3 4) (location 1 4 5) '(#\] . #f))
             (character-token (location 1 4 5) (location 1 5 6) '(#\] . #f))
             (character-token (location 1 5 6) (location 1 6 7) '(#\> . #f))
             (character-token (location 1 6 7) (location 1 7 8) #\x)))
  (check-it "Two ]'s, then EOF"
            "]]"
            (list
             (character-token (location 1 0 1) (location 1 1 2) #\])
             (character-token (location 1 1 2) (location 1 2 3) #\])
             (eof-in-cdata (location 1 2 3) #f)))
  (check-it "Three ]'s"
            "]]]>"
            (list
             (character-token (location 1 0 1) (location 1 1 2) #\])
             (character-token (location 1 1 2) (location 1 2 3) '(#\] . #f))
             (character-token (location 1 2 3) (location 1 3 4) '(#\] . #f))
             (character-token (location 1 3 4) (location 1 4 5) '(#\> . #f))))
  (check-it "Two ]'s, then more stuff"
            "]]hey]]>z"
            (list
             (character-token (location 1 0 1) (location 1 1 2) #\])
             (character-token (location 1 1 2) (location 1 2 3) #\])
             (string-token (location 1 2 3) (location 1 5 6) "hey")
             (character-token (location 1 5 6) (location 1 6 7) '(#\] . #f))
             (character-token (location 1 6 7) (location 1 7 8) '(#\] . #f))
             (character-token (location 1 7 8) (location 1 8 9) '(#\> . #f))
             (character-token (location 1 8 9) (location 1 9 10) #\z))))
