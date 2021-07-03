#lang typed/racket/base

(provide character-reference)

(require (only-in racket/list
                  take
                  last)
         (file "../private/todo.rkt")
         (file "predicates.rkt")
         (file "types.rkt")
         (file "infrastructure.rkt")
         (file "character-data.rkt")
         (file "tokens.rkt")
         (file "stream.rkt")
         (file "parameters.rkt"))

(module+ test
  (require typed/rackunit))

(: ensure-character-list (-> (Option (U Char
                                        (Listof Char)))
                             (Listof Char)))
(define (ensure-character-list thing)
  (cond
    [(char? thing)
     (list thing)]
    [(list? thing)
     thing]
    [else
     (list)]))

(: match-string-to-strings (-> String (Listof String) (Listof String)))
(define (match-string-to-strings s ss)
  (: matches? (-> String Boolean))
  (define (matches? another-s)
    (string=? another-s s))
  (filter matches? ss))

(: match-string-to-character-ref (-> String (Listof named-character-ref)))
(define (match-string-to-character-ref s)
  (cond [(string=? "" s)
         (list)]
        [else
         (define first-char (string-ref s 0))
         (define existing-matches (hash-ref characters-index
                                            first-char
                                            (lambda () (list))))
         (define matches (match-string-to-strings
                          s
                          existing-matches))
         (: handle-match (-> String named-character-ref))
         (define (handle-match m)
           (hash-ref characters-by-name m))
         (map handle-match matches)]))

(module+ test
  (test-case
      "Matching empty string doesn't work"
    (check-equal? (match-string-to-character-ref "")
                  (list)))
  (test-case
      "Cannot look up character"
    (check-equal? (match-string-to-character-ref "$")
                  (list))))

(: all-but-last (-> (Pairof Char (Listof Char)) (Listof Char)))
(define (all-but-last chars)
  (take chars (sub1 (length chars))))

(: bounded-character-reference (-> (Listof Char) (Option named-character-ref)))
(define (bounded-character-reference chars)
  (cond [(null? chars) #f]
        [else
         (define s (list->string chars))
         (define matches (match-string-to-character-ref s))
         (cond [(null? matches)
                (bounded-character-reference (all-but-last chars))]
               [else
                (car matches)])]))

(: starting-location (Parameter location))
(define starting-location (make-parameter (location 1 0 0)))

(: character-reference (-> character-token
                           (Listof (U character-token
                                      tokenizer-error
                                      character-reference-token))))
(define (character-reference ampersand)
  (starting-location (span-start ampersand))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list ampersand)]
        [(ascii-alphanumeric? c)
         (named-character-reference ampersand)]
        [(char=? c #\#)
         (read-char)
         (numeric-character-reference ampersand (make-character-token/after-read c))]
        [else
         (list ampersand)]))

; https://html.spec.whatwg.org/multipage/parsing.html#numeric-character-reference-state
(: numeric-character-reference (-> character-token
                                   character-token
                                   (Listof (U character-token
                                              tokenizer-error
                                              character-reference-token))))
(define (numeric-character-reference ampersand pound)
  (define c (peek-char))
  (cond
    [(and (char? c)
          (or (char=? c #\x)
              (char=? c #\X)))
     (read-char)
     (hexadecimal-character-reference-start ampersand pound (make-character-token/after-read c))]
    [else
     (decimal-character-reference-start ampersand pound)]))

; https://html.spec.whatwg.org/multipage/parsing.html#hexadecimal-character-reference-start-state
(: hexadecimal-character-reference-start (-> character-token
                                             character-token
                                             character-token
                                             (Listof (U character-token
                                                        tokenizer-error
                                                        character-reference-token))))
(define (hexadecimal-character-reference-start ampersand pound x)
  (define c (peek-char))
  (cond [(and (char? c)
              (hexadecimal-digit? c))
         (hexadecimal-character-reference ampersand pound x)]
        [else
         (when (char? c)
           (read-char))
         (append (list ampersand pound x)
                 (cond [(char? c) (list (make-character-token/after-read c))]
                       [else (list)]))]))

; https://html.spec.whatwg.org/multipage/parsing.html#hexadecimal-character-reference-state
(: hexadecimal-character-reference (->* (character-token
                                         character-token
                                         character-token)
                                        ((Listof character-token)
                                         Exact-Nonnegative-Integer)
                                        (Listof (U character-token
                                                   tokenizer-error
                                                   character-reference-token))))
(define (hexadecimal-character-reference ampersand pound x [chars (list)] [sum 0])
  (define (fallback)
    (cons (missing-semicolon-after-character-reference (starting-location)
                                                       #f)
          (numeric-character-reference-end ampersand pound x chars sum #f)))
  (define c (peek-char))
  (cond
    [(eof-object? c)
     (fallback)]
    [(hexadecimal-digit? c)
     (read-char)
     (hexadecimal-character-reference ampersand
                                      pound
                                      x
                                      (cons (make-character-token/after-read c) chars)
                                      (+ (* sum 16)
                                         (hexadecimal-digit->integer c)))]
    [(char=? #\; c)
     (read-char)
     (numeric-character-reference-end ampersand pound x chars sum (make-character-token/after-read c))]
    [else
     (fallback)]))

; https://html.spec.whatwg.org/multipage/parsing.html#decimal-character-reference-start-state
(: decimal-character-reference-start (-> character-token
                                         character-token
                                         (Listof (U character-token
                                                    tokenizer-error
                                                    character-reference-token))))
(define (decimal-character-reference-start ampersand pound)
  (define c (peek-char))
  (cond [(and (char? c)
              (ascii-digit? c))
         (decimal-character-reference ampersand pound)]
        [else
         (cons (absence-of-digits-in-numeric-character-reference (starting-location)
                                                                 #f)
               (list ampersand pound))]))

; https://html.spec.whatwg.org/multipage/parsing.html#decimal-character-reference-state
(: decimal-character-reference (->* (character-token character-token)
                                    ((Listof character-token)
                                     Exact-Nonnegative-Integer)
                                    (Listof (U character-token
                                               tokenizer-error
                                               character-reference-token))))
(define (decimal-character-reference ampersand pound [chars (list)] [sum 0])
  (define (fallback)
    (cons (missing-semicolon-after-character-reference (starting-location)
                                                       #f)
          (numeric-character-reference-end ampersand pound #f chars sum #f)))
  (define c (peek-char))
  (cond
    [(eof-object? c)
     (fallback)]
    [(ascii-digit? c)
     (read-char)
     (decimal-character-reference ampersand
                                  pound
                                  (cons (make-character-token/after-read c) chars)
                                  (+ (* sum 10) (digit->integer c)))]
    [(char=? #\; c)
     (read-char)
     (numeric-character-reference-end ampersand pound #f chars sum (make-character-token/after-read c))]
    [else
     (fallback)]))

(define codepoint-upgrades
  (hash #x80	#x20ac
        #x82	#x201a
        #x83	#x0192
        #x84	#x201e
        #x85	#x2026
        #x86	#x2020
        #x87	#x2021
        #x88	#x02c6
        #x89	#x2030
        #x8a	#x0160
        #x8b	#x2039
        #x8c	#x0152
        #x8e	#x017d
        #x91	#x2018
        #x92	#x2019
        #x93	#x201c
        #x94	#x201d
        #x95	#x2022
        #x96	#x2013
        #x97	#x2014
        #x98	#x02dc
        #x99	#x2122
        #x9a	#x0161
        #x9b	#x203a
        #x9c	#x0153
        #x9e	#x017e
        #x9f	#x0178))

(: numeric-character-reference-end (-> character-token
                                       character-token
                                       (Option character-token)
                                       (Listof character-token)
                                       Exact-Nonnegative-Integer
                                       (Option character-token)
                                       (Listof (U character-token
                                                  tokenizer-error
                                                  character-reference-token))))
(define (numeric-character-reference-end ampersand pound x chars character-reference-code semicolon)
  (: all-chars (Listof character-token))
  (define all-chars (append (list ampersand pound)
                            (cond [(eq? #f x) (list)]
                                  [else (list x)])
                            (reverse chars)
                            (cond [(eq? #f semicolon) (list)]
                                  [else (list semicolon)])))
  (define all-chars/chars (enumerate-input-characters all-chars))
  (cond
    [(= #x0 character-reference-code)
     (cons (null-character-reference (starting-location)
                                     (reverse chars))
           (numeric-character-reference-end ampersand pound x (list) #xfffd semicolon))]
    [(> character-reference-code #x10ffff)
     (list (character-reference-outside-unicode-range (starting-location)
                                                      all-chars)
           (character-token (starting-location)
                            (get-current-location)
                            (cons all-chars/chars #\ufffd)))]
    [(surrogate-code-point? character-reference-code)
     (list (surrogate-character-reference (starting-location)
                                          all-chars)
           (character-token (starting-location)
                            (get-current-location)
                            (cons all-chars/chars #\ufffd)))]
    [(noncharacter-code-point? character-reference-code)
     (list (noncharacter-character-reference (starting-location)
                                             all-chars))]
    [(= #x0d character-reference-code)
     (list (control-character-reference (starting-location)
                                        all-chars))]
    [(and (control-code-point? character-reference-code)
          (not (ascii-whitespace-code-point? character-reference-code)))
     (list (control-character-reference (starting-location) all-chars))]
    [(hash-has-key? codepoint-upgrades character-reference-code)
     (define upgrade (hash-ref codepoint-upgrades
                               character-reference-code))
     (list (character-reference-token (starting-location)
                                      (current-location)
                                      all-chars
                                      (integer->char upgrade)
                                      #t))]
    [else
     (define new-char (integer->char character-reference-code))
     (define token (character-reference-token (starting-location)
                                              (current-location)
                                              all-chars
                                              new-char
                                              #f))
     (list token)]))

(: butlast (-> (Listof Char)
               (Listof Char)))
(define (butlast l)
  (reverse (cdr (reverse l))))

(: consume-characters (-> (Listof Char)
                          (Listof character-token)))
(define (consume-characters chars)
  (cond [(null? chars)
         (list)]
        [else
         (read-char)
         (cons (make-character-token/after-read (car chars))
               (consume-characters (cdr chars)))]))

; https://html.spec.whatwg.org/multipage/parsing.html#named-character-reference-state
(: named-character-reference (-> character-token
                                 (Listof (U character-token
                                            tokenizer-error
                                            character-reference-token))))
(define (named-character-reference ampersand)
  (define peeked (peek-several length-of-longest-character-ref))
  (define result (bounded-character-reference peeked))
  (cond [(named-character-ref? result)
         (define codepoints (named-character-ref-codepoints result))
         (define name (named-character-ref-name result))
         (define semicolon (current-semicolon))
         (: to-be-consumed (Listof Char))
         (define to-be-consumed (peek-several (string-length name)))
         (define consumed (consume-characters to-be-consumed))
         (: last-consumed Char)
         (define last-consumed (last to-be-consumed))
         (define all-but-last-consumed (butlast to-be-consumed))
         (define d (peek-char))
         (cond [(and (currently-in-attribute-value?)
                     (not (char=? last-consumed #\;))
                     (char? d)
                     (or (char=? d #\=)
                         (ascii-alphanumeric? d)))
                (append (cons ampersand consumed)
                        (cond [(eq? #f semicolon) (list)]
                              [else (list semicolon)]))]
               [(char=? last-consumed #\;)
                (list (character-reference-token (starting-location)
                                                 (get-current-location)
                                                 (cons ampersand consumed)
                                                 (cond [(null? (cdr codepoints)) (car codepoints)]
                                                       [else codepoints])
                                                 #f))]
               [else
                (list (missing-semicolon-after-character-reference (get-current-location) #f)
                      (character-reference-token (starting-location)
                                                 (get-current-location)
                                                 (cons ampersand consumed)
                                                 (cond [(null? (cdr codepoints)) (car codepoints)]
                                                       [else codepoints])
                                                 #f))])]
        [else
         (ambiguous-ampersand ampersand)]))

(: ambiguous-ampersand (->* (character-token)
                            ((Listof character-token))
                            (Listof (U tokenizer-error character-token))))
(define (ambiguous-ampersand ampersand [buffer (list)])
  (define (fallback)
    (cons ampersand (reverse buffer)))
  (define c (peek-char))
  (cond [(eof-object? c)
         (fallback)]
        [(ascii-alphanumeric? c)
         (read-char)
         (ambiguous-ampersand ampersand
                              (cons (make-character-token/after-read c) buffer))]
        [(char=? #\; c)
         (read-char)
         (define ct (make-character-token/after-read c))
         (define chars-so-far (reverse (cons ct buffer)))
         (define all-chars (cons ampersand chars-so-far))
         (list (unknown-named-character-reference (get-current-location)
                                                  all-chars))]
        [else
         (fallback)]))
