#lang typed/racket/base/optional

(provide SCRIPT)

(require (file "tokens.rkt")
         (file "parameters.rkt")
         (prefix-in tag: (file "tag.rkt"))
         (file "types.rkt")
         (file "predicates.rkt")
         (file "stream.rkt")
         (file "infrastructure.rkt"))

(module+ test
  (require typed/rackunit))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-state
(: data/loc (->* ()
                 ((Listof (U character-token string-token)))
                 (Listof (U Token tokenizer))))
(define (data/loc [buffer (list)])
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
                         (data/loc (cons (replace-character ct) buffer)))]
                  [else
                   (data/loc (cons ct buffer))])])]
        [else
         (define bs (car m))
         (for ([b : Byte (in-bytes bs)])
           (read-byte))
         (define token (string-token loc
                                     (get-current-location)
                                     (bytes->string/utf-8 bs)))
         (data/loc (cons token buffer))]))

(define SCRIPT (tokenizer data/loc))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-less-than-sign-state
(: less-than-sign (-> character-token
                      (Listof (U Token tokenizer))))
(define (less-than-sign less-than)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (cons less-than (data/loc))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\/)
            (end-tag-open less-than ct)]
           [(#\!)
            (append (list less-than ct)
                    (escape-start))]
           [else
            (cons less-than (data/loc (list ct)))])]))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-end-tag-open-state
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
                 (data/loc))]))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-end-tag-name-state
(: end-tag-name (->* (character-token character-token)
                     ((Listof character-token))
                     (Listof (U Token tokenizer))))
(define (end-tag-name less-than slash [buffer (list)])
  (define (anything-else)
    (append (list less-than slash)
            (reverse buffer)
            (data/loc)))
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (anything-else)]
        [(html5-whitespace? c)
         (cond [(tag:appropriate-tag? (reverse buffer))
                (read-char)
                (define scanner (tokenizer-scanner tag:before-attribute-name))
                (parameterize ([current-tag-ending? #t]
                               [current-tag-name (reverse buffer)]
                               [current-less-than less-than]
                               [current-end-tag-slash slash]
                               [current-misc (list (make-character-token/after-read c))])
                  (scanner))]
               [else
                (anything-else)])]
        [(char=? #\/ c)
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
                (list (end-tag-token (span-start less-than)
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

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-escape-start-state
(: escape-start (-> (Listof (U Token tokenizer))))
(define (escape-start)
  (define c (peek-char))
  (cond [(and (char? c)
              (char=? #\- c))
         (read-char)
         (escape-start-dash (make-character-token/after-read c))]
        [else
         (data/loc)]))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-escape-start-dash-state
(: escape-start-dash (-> character-token
                         (Listof (U Token tokenizer))))
(define (escape-start-dash dash)
  (define c (peek-char))
  (cond [(and (char? c)
              (char=? #\- c))
         (read-char)
         (escaped-dash-dash dash (make-character-token/after-read c))]
        [else
         (cons dash (data/loc))]))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-escaped-dash-dash-state
(: escaped-dash-dash (-> character-token
                         character-token
                         (Listof (U Token tokenizer))))
(define (escaped-dash-dash dash-1 dash-2)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (append (list dash-1 dash-2)
                 (escaped))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\-)
            (cons dash-1 (escaped-dash-dash dash-2 ct))]
           [(#\<)
            (append (list dash-1 dash-2)
                    (escaped-less-than-sign ct))]
           [(#\>)
            (append (list dash-1 dash-2 ct)
                    (data/loc))]
           [(#\u0000)
            (append (list dash-1 dash-2
                          (unexpected-null-character loc #f)
                          (replace-character ct))
                    (escaped))]
           [else
            (cons ct (escaped-dash-dash dash-1 dash-2))])]))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-escaped-state
(: escaped (->* ()
                ((Listof character-token))
                (Listof (U Token tokenizer))))
(define (escaped [buffer (list)])
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (cons (eof-in-script-html-comment-like-text loc #f)
               (drop-characters (reverse buffer)))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\-)
            (append (reverse buffer)
                    (escaped-dash ct))]
           [(#\<)
            (append (reverse buffer)
                    (escaped-less-than-sign ct))]
           [(#\u0000)
            (cons (unexpected-null-character loc #f)
                  (escaped (cons (replace-character ct) buffer)))]
           [else
            (escaped (cons ct buffer))])]))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-escaped-less-than-sign-state
(: escaped-less-than-sign (-> character-token
                              (Listof (U Token tokenizer))))
(define (escaped-less-than-sign less-than)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (cons less-than (escaped))]
        [(char=? #\/ c)
         (read-char)
         (escaped-end-tag-open less-than
                               (make-character-token/after-read c))]
        [(ascii-alpha? c)
         (cons less-than (double-escape-start))]
        [else
         (cons less-than (escaped))]))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-escaped-end-tag-open-state
(: escaped-end-tag-open (-> character-token
                            character-token
                            (Listof (U Token tokenizer))))
(define (escaped-end-tag-open less-than solidus)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(and (char? c)
              (ascii-alpha? c))
         (escaped-end-tag-name less-than solidus)]
        [else
         (append (list less-than solidus)
                 (escaped))]))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-escaped-end-tag-name-state
(: escaped-end-tag-name (->* (character-token character-token)
                             ((Listof character-token))
                             (Listof (U Token tokenizer))))
(define (escaped-end-tag-name less-than solidus [buffer (list)])
  (define (anything-else)
    (append (list less-than solidus)
            (reverse buffer)
            (escaped)))
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (anything-else)]
        [(html5-whitespace? c)
         (cond [(tag:appropriate-tag? (reverse buffer))
                (read-char)
                (define scanner (tokenizer-scanner tag:before-attribute-name))
                (parameterize ([current-tag-name (reverse buffer)]
                               [current-less-than less-than]
                               [current-tag-ending? #t]
                               [current-end-tag-slash solidus]
                               [current-misc (list (make-character-token/after-read c))])
                  (scanner))]
               [else
                (anything-else)])]
        [(char=? #\/ c)
         (cond [(tag:appropriate-tag? (reverse buffer))
                (read-char)
                (define scanner (tokenizer-scanner tag:self-closing-start-tag))
                (parameterize ([current-tag-name (reverse buffer)]
                               [current-tag-ending? #t]
                               [current-less-than less-than]
                               [current-end-tag-slash solidus]
                               [current-tag-self-closing-char (make-character-token/after-read c)])
                  (scanner))]
               [else
                (anything-else)])]
        [(char=? #\> c)
         (cond [(tag:appropriate-tag? (reverse buffer))
                (read-char)
                (list (end-tag-token (span-start less-than)
                                     (get-current-location)
                                     less-than
                                     (reverse buffer)
                                     (list)
                                     #f
                                     (make-character-token/after-read c)
                                     (list)
                                     solidus))]
               [else
                (anything-else)])]
        [(ascii-uppercase? c)
         (read-char)
         (escaped-end-tag-name less-than
                               solidus
                               (cons (downcase-character (make-character-token/after-read c))
                                     buffer))]
        [(ascii-lowercase? c)
         (read-char)
         (escaped-end-tag-name less-than
                               solidus
                               (cons (make-character-token/after-read c) buffer))]
        [else
         (anything-else)]))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-escaped-dash-state
(: escaped-dash (-> character-token (Listof (U Token tokenizer))))
(define (escaped-dash dash)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-script-html-comment-like-text loc #f))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\-)
            (escaped-dash-dash dash ct)]
           [(#\<)
            (cons dash (escaped-less-than-sign ct))]
           [(#\u0000)
            (cons (unexpected-null-character loc #f)
                  (cons (replace-character ct)
                        (escaped-dash dash)))]
           [else
            (append (list dash ct)
                    (escaped))])]))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-double-escape-start-state
(: double-escape-start (->* ()
                            ((Listof character-token))
                            (Listof (U Token tokenizer))))
(define (double-escape-start [buffer (list)])
  (define loc (get-current-location))
  (define c (peek-char))
  (cond
    [(eof-object? c)
     (reverse buffer)]
    [(or (html5-whitespace? c)
         (char=? #\/ c)
         (char=? #\> c))
     (read-char)
     (append (drop-characters (reverse (cons (make-character-token/after-read c) buffer)))
             (cond [(tag:appropriate-tag? (reverse buffer))
                    (double-escaped)]
                   [else
                    (escaped)]))]
    [(ascii-uppercase? c)
     (read-char)
     (double-escape-start (cons (downcase-character (make-character-token/after-read c))
                                buffer))]
    [(ascii-lowercase? c)
     (read-char)
     (double-escape-start (cons (make-character-token/after-read c)
                                buffer))]
    [else
     (append (reverse buffer)
             (escaped))]))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-double-escaped-state
(: double-escaped (->* ()
                       ((Listof character-token))
                       (Listof (U Token tokenizer))))
(define (double-escaped [buffer (list)])
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (append (reverse buffer)
                 (list (eof-in-script-html-comment-like-text loc #f)))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\-)
            (append (reverse (cons ct buffer))
                    (double-escaped-dash))]
           [(#\<)
            (append (reverse buffer)
                    (double-escaped-less-than ct))]
           [(#\u0000)
            (cons (unexpected-null-character loc #f)
                  (double-escaped (cons (replace-character ct) buffer)))]
           [else
            (double-escaped (cons ct buffer))])]))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-double-escaped-dash-state
(: double-escaped-dash (-> (Listof (U Token tokenizer))))
(define (double-escaped-dash)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-script-html-comment-like-text loc #f))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\-)
            (cons ct (double-escaped-dash-dash))]
           [(#\<)
            (double-escaped-less-than ct)]
           [(#\u0000)
            (cons (unexpected-null-character loc #f)
                  (cons (replace-character ct)
                        (double-escaped-dash)))]
           [else
            (cons ct (double-escaped))])]))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-double-escaped-dash-dash-state
(: double-escaped-dash-dash (-> (Listof (U Token tokenizer))))
(define (double-escaped-dash-dash)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-script-html-comment-like-text loc #f))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\-)
            (cons ct (double-escaped-dash-dash))]
           [(#\<)
            (double-escaped-less-than ct)]
           [(#\>)
            (cons ct (data/loc))]
           [(#\u0000)
            (cons (unexpected-null-character loc #f)
                  (cons (replace-character ct)
                        (double-escaped-dash-dash)))]
           [else
            (cons ct (double-escaped))])]))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-double-escaped-less-than-sign-state
(: double-escaped-less-than (-> character-token
                                (Listof (U Token tokenizer))))
(define (double-escaped-less-than less-than)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(and (char? c)
              (char=? #\/ c))
         (read-char)
         (append (list less-than (make-character-token/after-read c))
                 (double-escape-end))]
        [else
         (cons less-than (double-escaped))]))

; https://html.spec.whatwg.org/multipage/parsing.html#script-data-double-escape-end-state
(: double-escape-end (->* ()
                          ((Listof character-token))
                          (Listof (U Token tokenizer))))
(define (double-escape-end [buffer (list)])
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (append (drop-characters (reverse buffer))
                 (double-escaped))]
        [(or (html5-whitespace? c)
             (char=? #\/ c)
             (char=? #\> c))
         (read-char)
         (define tokens (append (reverse buffer)
                                (list (make-character-token/after-read c))))
         (append tokens
                 (cond [(tag:appropriate-tag? (reverse buffer))
                        (escaped)]
                       [else
                        (double-escaped)]))]
        [(ascii-uppercase? c)
         (read-char)
         (double-escape-end (cons (downcase-character (make-character-token/after-read c))
                                  buffer))]
        [(ascii-lowercase? c)
         (read-char)
         (double-escape-end (cons (make-character-token/after-read c)
                                  buffer))]
        [else
         (append (reverse buffer)
                 (double-escaped))]))
