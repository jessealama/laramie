#lang typed/racket/base

(provide PLAINTEXT)

(require (file "parameters.rkt")
         (file "stream.rkt")
         (file "tokens.rkt")
         (file "types.rkt"))

; https://html.spec.whatwg.org/multipage/parsing.html#plaintext-state

(: keep-peeking (->* ()
                     ((Listof character-token))
                     (Listof Token)))
(define (keep-peeking [buffer (list)])
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (reverse buffer)]
        [(char=? c #\u0000)
         (read-char)
         (cons (unexpected-null-character loc #f)
               (keep-peeking (cons (replace-character (make-character-token/after-read c))
                                   buffer)))]
        [else
         (read-char)
         (keep-peeking (cons (make-character-token/after-read c)
                             buffer))]))

(define PLAINTEXT
  (tokenizer keep-peeking))
