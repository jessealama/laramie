#lang typed/racket/base

(provide tokenize
         next-token
         peek-token
         consume-token
         set-next-token!)

(require (file "types.rkt")
         (file "tokens.rkt")
         (file "data.rkt")
         (file "stream.rkt")
         (file "predicates.rkt")
         (file "parameters.rkt"))

(: read-chars (-> (Listof Char)
                  Exact-Nonnegative-Integer))
(define (read-chars chars)
  (: n Exact-Nonnegative-Integer)
  (define n 0)
  (for ([c : Char chars])
    (for ([i (in-range (char-utf-8-length c))])
      (read-byte)
      (set! n (add1 n))))
  n)

(: next-token (-> (Option Token)))
(define (next-token)
  (define buffer (current-token-buffer))
  (cond [(null? buffer)
         (log-error "next-token: token buffer empty!")
         #f]
        [else
         (current-token-buffer (cdr buffer))
         (define t (car buffer))
         (cond [(tokenizer? t)
                (define scanner (tokenizer-scanner t))
                (define new-tokens (scanner))
                (current-token-buffer (append new-tokens (cdr buffer)))
                (next-token)]
               [else
                (previous-tokens (cons t (previous-tokens)))
                t])]))

(: consume-token (-> Token
                     Exact-Nonnegative-Integer))
(define (consume-token t)
  (cond [(pair? t)
         (define input (enumerate-input-characters t))
         (read-chars input)]
        [(tokenizer-error? t)
         (read-chars (enumerate-input-characters t))]
        [(string? t)
         (read-chars (string->list t))]
        [(or (char? t)
             (doctype-token? t)
             (comment-token? t)
             (tag-token? t))
         (read-chars (enumerate-input-characters t))]
        [else
         (error (format "consume-token: Unhandled token: ~a" t))]))

(: set-next-token! (-> Token
                       Void))
(define (set-next-token! token)
  (current-token-buffer (cons token (current-token-buffer))))

(: keep-tokenizing (->* ()
                        ((Listof (U Token tokenizer)))
                        (Listof Token)))
(define (keep-tokenizing [previous-tokens (list)])
  (cond [(null? previous-tokens)
         (define result (scan))
         (cond [(null? result)
                (list)]
               [else
                (keep-tokenizing result)])]
        [(tokenizer? (car previous-tokens))
         (current-tokenizer-stack (cons (car previous-tokens)
                                        (current-tokenizer-stack)))
         (define result (scan))
         (keep-tokenizing (append result
                                  (cdr previous-tokens)))]
        [else
         (define token (car previous-tokens))
         (cons token (keep-tokenizing (cdr previous-tokens)))]))

; use the next scanner to peek the next tokens
(: scan (-> (Listof (U Token tokenizer))))
(define (scan)
  (define tokenizers (current-tokenizer-stack))
  (cond [(null? tokenizers)
         (current-tokenizer-stack (list data))
         (scan)]
        [else
         (define toker (car tokenizers))
         (current-tokenizer-stack (cdr tokenizers))
         (define scanner (tokenizer-scanner toker))
         (scanner)]))

(: keep-peeking (-> (Listof (U Token tokenizer))
                    (Option Token)))
(define (keep-peeking tokens)
  (cond [(null? tokens) #f]
        [(token? (car tokens))
         (define t (car tokens))
         (cond [(pair? t)
                (cond [(eq? #f (cdr t))
                       (cond [(include-dropped-chars?) t]
                             [else (keep-peeking (cdr tokens))])]
                      [else t])]
               [else t])]
        [(tokenizer-error? (car tokens))
         (cond [(include-tokenizer-errors?)
                (car tokens)]
               [else
                (keep-peeking (cdr tokens))])]
        [(tokenizer? (car tokens))
         (define toker (car tokens))
         (define peeked (parameterize ([current-tokenizer-stack (cons toker (current-tokenizer-stack))])
                          (peek-token)))
         (cond [(eq? #f peeked)
                (keep-peeking (cdr tokens))]
               [else
                peeked])]
        [else
         (error "Don't know how to peek with ~a" (car tokens))]))

(: peek-token (-> (Option Token)))
(define (peek-token)
  (define tokens (current-token-buffer))
  (define tokenizers (current-tokenizer-stack))
  (cond [(null? tokens)
         (cond [(null? tokenizers)
                (current-tokenizer-stack (list data))
                (peek-token)]
               [else
                (define toker (car tokenizers))
                (define scanner (tokenizer-scanner toker))
                (current-tokenizer-stack (cdr tokenizers))
                (define new-tokens (scanner))
                (cond [(null? new-tokens)
                       #f]
                      [else
                       (current-token-buffer new-tokens)
                       (peek-token)])])]
        [(tokenizer? (car tokens))
         (define toker (car tokens))
         (define scanner (tokenizer-scanner toker))
         (define new-tokens (scanner))
         (current-token-buffer (append new-tokens (cdr tokens)))
         (peek-token)]
        [else (car tokens)]))

(: tokenize (->* ((U Bytes String))
                 (#:include-dropped? Boolean
                  #:include-errors? Boolean
                  #:initial-tokenizer (Option tokenizer))
                 (Listof Token)))
(define (tokenize s
                  #:include-dropped? [include-dropped? #f]
                  #:include-errors? [include-errors? #f]
                  #:initial-tokenizer [initial-tokenizer #f])
  (define in (cond [(string? s) (open-input-string s)]
                   [else (open-input-bytes s)]))
  (port-count-lines! in)
  (parameterize ([current-input-port in]
                 [include-dropped-chars? include-dropped?]
                 [include-tokenizer-errors? include-errors?]
                 [current-tokenizer-stack (list (or initial-tokenizer data))])
    (begin0
        (keep-tokenizing)
      (unless (eof-object? (peek-char))
        (log-warning "Tokenization complete, but we're not at EOF!")))))
