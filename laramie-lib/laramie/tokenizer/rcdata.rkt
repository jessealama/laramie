#lang typed/racket/base/optional

(provide RCDATA)

(require (file "tokens.rkt")
         (file "types.rkt")
         (file "parameters.rkt")
         (file "characters.rkt")
         (file "comment.rkt")
         (prefix-in tag: (file "tag.rkt"))
         (file "stream.rkt")
         (file "predicates.rkt")
         (file "infrastructure.rkt"))

; https://html.spec.whatwg.org/multipage/parsing.html#rcdata-state

(: RCDATA/loc (-> (Listof (U Token tokenizer))))
(define (RCDATA/loc)
  (: keep-going (-> (Listof (U character-token string-token character-reference-token))
                    (Listof (U Token tokenizer))))
  (define (keep-going buffer)
    (define loc (get-current-location))
    (define m (regexp-match-peek #px"^[^&<\u0000]+"
                                 (current-input-port)))
    (cond
      [(eq? #f m)
       (define c (peek-char))
       (cond
         [(eof-object? c)
          (reverse buffer)]
         [else
          (read-char)
          (define ct (make-character-token/after-read c))
          (case c
            [(#\&)
             (define cr-tokens
               (parameterize ([currently-in-attribute-value? #f])
                 (character-reference ct)))
             (define acceptable (filter (lambda (x)
                                          (or (character-token? x)
                                              (character-reference-token? x)))
                                        cr-tokens))
             (define errors (filter tokenizer-error? cr-tokens))
             (append errors
                     (keep-going (append (reverse acceptable) buffer)))]
            [(#\<)
             (append (reverse buffer)
                     (RCDATA-less-than-sign ct))]
            [(#\u0000)
             (cons (unexpected-null-character loc #f)
                   (keep-going (cons (replace-character ct)
                                     buffer)))]
            [else
             (keep-going (cons ct buffer))])])]
      [else
       (define bs (car m))
       (for ([b : Byte (in-bytes bs)])
         (read-byte))
       (define token (string-token loc
                                   (get-current-location)
                                   (bytes->string/utf-8 bs)))
       (keep-going (cons token buffer))]))
  (keep-going (list)))

; https://html.spec.whatwg.org/multipage/parsing.html#rcdata-less-than-sign-state
(: RCDATA-less-than-sign (-> character-token
                             (Listof (U Token tokenizer))))
(define (RCDATA-less-than-sign less-than-sign)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(and (char? c)
              (char=? c #\/))
         (read-char)
         (RCDATA-end-tag-open less-than-sign (make-character-token/after-read c))]
        [else
         (cons less-than-sign (RCDATA/loc))]))

; https://html.spec.whatwg.org/multipage/parsing.html#rcdata-end-tag-open-state
(: RCDATA-end-tag-open (-> character-token
                           character-token
                           (Listof (U Token tokenizer))))
(define (RCDATA-end-tag-open less-than solidus)
  (define l (get-current-location))
  (define c (peek-char))
  (cond [(or (eof-object? c)
             (not (ascii-alpha? c)))
         (append (list less-than solidus)
                 (RCDATA/loc))]
        [else
         (RCDATA-end-tag-name less-than
                              solidus)]))

; https://html.spec.whatwg.org/multipage/parsing.html#rcdata-end-tag-name-state
(: RCDATA-end-tag-name (->* (character-token character-token)
                            ((Listof character-token))
                            (Listof (U Token tokenizer))))
(define (RCDATA-end-tag-name less-than solidus [buffer (list)])
  (: anything-else (-> (Option character-token)
                       (Listof (U Token tokenizer))))
  (define (anything-else c)
    (append (list less-than solidus)
            (reverse buffer)
            (cond [(character-token? c) (list c)]
                  [else (list)])))
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (anything-else #f)]
        [(html5-whitespace? c)
         (read-char)
         (define ct (make-character-token/after-read c))
         (cond [(tag:appropriate-tag? (reverse buffer))
                (define scanner (tokenizer-scanner tag:before-attribute-name))
                (parameterize ([current-tag-ending? #t]
                               [current-less-than less-than]
                               [current-end-tag-slash solidus]
                               [current-tag-name (reverse buffer)]
                               [current-misc (list ct)])
                  (scanner))]
               [else
                (append (anything-else ct)
                        (RCDATA/loc))])]
        [(char=? c #\/)
         (read-char)
         (define ct (make-character-token/after-read c))
         (cond [(tag:appropriate-tag? (reverse buffer))
                (define scanner (tokenizer-scanner tag:self-closing-start-tag))
                (parameterize ([current-tag-ending? #t]
                               [current-less-than less-than]
                               [current-tag-name (reverse buffer)]
                               [current-end-tag-slash solidus]
                               [current-tag-self-closing-char ct])
                  (scanner))]
               [else
                (append (anything-else ct)
                        (RCDATA/loc))])]
        [(char=? c #\>)
         (read-char)
         (define ct (make-character-token/after-read c))
         (cond [(tag:appropriate-tag? (reverse buffer))
                (list
                 (end-tag-token (span-start less-than)
                                (get-current-location)
                                less-than
                                (reverse buffer)
                                (list)
                                #f
                                ct
                                (list)
                                solidus))]
               [else
                (append (anything-else ct)
                        (RCDATA/loc))])]
        [(ascii-uppercase? c)
         (read-char)
         (RCDATA-end-tag-name less-than
                              solidus
                              (cons (downcase-character (make-character-token/after-read c))
                                    buffer))]
        [(ascii-lowercase? c)
         (read-char)
         (RCDATA-end-tag-name less-than
                              solidus
                              (cons (make-character-token/after-read c) buffer))]
        [else
         (append (anything-else #f)
                 (RCDATA/loc))]))

(define RCDATA
  (tokenizer RCDATA/loc))
