#lang typed/racket/base/optional

(provide RAWTEXT)

(require (file "parameters.rkt")
         (prefix-in tag: (file "tag.rkt"))
         (file "types.rkt")
         (file "stream.rkt")
         (file "tokens.rkt")
         (file "infrastructure.rkt")
         (file "predicates.rkt"))

(: RAWTEXT/loc (->* ()
                    ((Listof (U character-token
                                string-token)))
                    (Listof (U Token tokenizer))))
(define (RAWTEXT/loc [buffer (list)])
  (define loc (get-current-location))
  (define m (regexp-match-peek #px"^[^<\u0000]+"
                               (current-input-port)))
  (cond [(eq? #f m)
         (define c (peek-char))
         (cond [(eof-object? c)
                (reverse buffer)]
               [else
                (read-char)
                (define ct (make-character-token/after-read c))
                (case c
                  [(#\<)
                   (append (reverse buffer)
                           (less-than-sign ct))]
                  [(#\u0000)
                   (cons (unexpected-null-character loc #f)
                         (RAWTEXT/loc (cons (replace-character ct) buffer)))]
                  [else
                   (log-error "RAWTEXT: encountered ~a (not supposed to happen)" c)
                   (RAWTEXT/loc (cons ct buffer))])])]
        [else
         (define bs (car m))
         (for ([b : Byte (in-bytes bs)])
           (read-byte))
         (define token (string-token loc
                                     (get-current-location)
                                     (bytes->string/utf-8 bs)))
         (RAWTEXT/loc (cons token buffer))]))

; https://html.spec.whatwg.org/multipage/parsing.html#rawtext-state

(define RAWTEXT (tokenizer RAWTEXT/loc))

; https://html.spec.whatwg.org/multipage/parsing.html#rawtext-less-than-sign-state
(: less-than-sign (-> character-token
                      (Listof (U Token tokenizer))))
(define (less-than-sign less-than)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(and (char? c)
              (char=? #\/ c))
         (read-char)
         (end-tag-open less-than (make-character-token/after-read c))]
        [else
         (cons less-than (RAWTEXT/loc))]))

; https://html.spec.whatwg.org/multipage/parsing.html#rawtext-end-tag-open-state
(: end-tag-open (-> character-token
                    character-token
                    (Listof (U Token tokenizer))))
(define (end-tag-open less-than slash)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(and (char? c)
              (ascii-alpha? c))
         (end-tag-name less-than slash)]
        [else
         (append (list less-than slash)
                 (RAWTEXT/loc))]))

; https://html.spec.whatwg.org/multipage/parsing.html#rawtext-end-tag-name-state
(: end-tag-name (->* (character-token character-token)
                     ((Listof character-token))
                     (Listof (U Token tokenizer))))
(define (end-tag-name less-than slash [buffer (list)])
  (define (anything-else)
    (append (list less-than slash)
            (reverse buffer)
            (RAWTEXT/loc)))
  (define loc (get-current-location))
  (define c (peek-char))
  (cond
    [(eof-object? c)
     (anything-else)]
    [(html5-whitespace? c)
     (cond [(tag:appropriate-tag? (reverse buffer))
            (read-char)
            (define scanner (tokenizer-scanner tag:before-attribute-name))
            (parameterize ([current-tag-name (reverse buffer)]
                           [current-tag-ending? #t]
                           [current-less-than less-than]
                           [current-end-tag-slash slash]
                           [current-misc (list (make-character-token/after-read c))])
              (scanner))]
           [else
            (anything-else)])]
    [(char=? #\/ c)
     (define another-slash (cons c loc))
     (cond [(tag:appropriate-tag? (reverse buffer))
            (read-char)
            (define scanner (tokenizer-scanner tag:self-closing-start-tag))
            (parameterize ([current-tag-name (reverse buffer)]
                           [current-tag-ending? #t]
                           [current-less-than less-than]
                           [current-end-tag-slash slash]
                           [current-tag-self-closing-char (make-character-token/after-read c)])
              (scanner))]
           [else
            (anything-else)])]
    [(char=? #\> c)
     (cond [(tag:appropriate-tag? (reverse buffer))
            (read-char)
            (list
             (end-tag-token (span-start less-than)
                            (get-current-location)
                            less-than
                            (reverse buffer)
                            (list)
                            #f
                            (make-character-token/after-read c)
                            (list)
                            slash))]
           [else
            (anything-else)])]
    [(ascii-uppercase? c)
     (read-char)
     (end-tag-name less-than
                   slash
                   (cons (downcase-character (make-character-token/after-read c))
                         buffer))]
    [(ascii-lowercase? c)
     (read-char)
     (end-tag-name less-than
                   slash
                   (cons (make-character-token/after-read c) buffer))]
    [else
     (anything-else)]))
