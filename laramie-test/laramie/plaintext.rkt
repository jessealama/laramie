#lang racket/base

(require laramie/tokenize)

(module+ test
  (require rackunit
           syntax/parse/define))

(module+ test
  (define-simple-macro (check-it test-name subject value)
    (let ([tokens (tokenize subject
                            #:include-dropped? #t
                            #:include-errors? #t
                            #:initial-tokenizer PLAINTEXT)])
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
   "Simple plaintext"
   "hi!"
   (list
    (character-token (location 1 0 1) (location 1 1 2) #\h)
    (character-token (location 1 1 2) (location 1 2 3) #\i)
    (character-token (location 1 2 3) (location 1 3 4) #\!)))
  (check-it
   "Null char in plaintext"
   (format "h~a!" #\nul)
   (list
    (unexpected-null-character (location 1 1 2) #f)
    (character-token (location 1 0 1) (location 1 1 2) #\h)
    (character-token (location 1 1 2) (location 1 2 3) '(#\nul . #\�))
    (character-token (location 1 2 3) (location 1 3 4) #\!))))
