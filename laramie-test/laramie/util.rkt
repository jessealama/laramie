#lang typed/racket/base

(provide check-tokenization
         check-length)

(require laramie/tokenize
         syntax/parse/define
         typed/rackunit
         racket/list
         racket/format)

(define-simple-macro (check-tokenization tokenizer test-name s expected)
  (let* ([result (tokenizer s)]
         [tokens (tokenizer-result-tokens result)]
         [errors (tokenizer-result-errors result)]
         [dropped (tokenizer-result-dropped result)]
         [everything (append tokens errors dropped)])
    (test-case
        test-name
      (check-equal? tokens expected))
    (test-case
        (format "~a (no gaps)" test-name)
      (check-false (find-gap everything)))
    (test-case
        (format "~a (no repetitions)" test-name)
      (check-false (find-repetition everything)))))

(define-simple-macro (check-length lst len)
  (check-= (length lst)
           len
           0
           (~a lst)))
