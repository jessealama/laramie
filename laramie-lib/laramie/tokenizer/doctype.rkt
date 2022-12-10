#lang typed/racket/base/optional

(provide DOCTYPE)

(require (file "types.rkt")
         (file "tokens.rkt")
         (file "stream.rkt")
         (file "predicates.rkt")
         (file "parameters.rkt")
         (file "infrastructure.rkt"))

(module+ test
  (require typed/rackunit))

(: current-doctype-quirky? (Parameter Boolean))
(define current-doctype-quirky? (make-parameter #f))

(: current-doctype-name (Parameter (Listof character-token)))
(define current-doctype-name (make-parameter (list)))

(: current-public-identifier (Parameter (Option (Listof character-token))))
(define current-public-identifier (make-parameter #f))

(: current-public-keyword (Parameter (Option (Listof character-token))))
(define current-public-keyword (make-parameter #f))

(: current-system-identifier (Parameter (Option (Listof character-token))))
(define current-system-identifier (make-parameter #f))

(: current-system-keyword (Parameter (Option (Listof character-token))))
(define current-system-keyword (make-parameter #f))

(: make-doctype (-> doctype-token))
(define (make-doctype)
  (define less-than (current-less-than))
  (define bang (current-bang))
  (cond [(and (character-token? less-than)
              (character-token? bang))
         (make-doctype-token #:start (span-start less-than)
                             #:stop (get-current-location)
                             #:less-than less-than
                             #:bang bang
                             #:doctype (current-doctype-opener)
                             #:name (current-doctype-name)
                             #:public-keyword (current-public-keyword)
                             #:public-identifier (current-public-identifier)
                             #:system-keyword (current-system-keyword)
                             #:system-identifier (current-system-identifier)
                             #:force-quirks? (current-doctype-quirky?)
                             #:greater-than (current-greater-than)
                             #:misc (reverse (current-misc)))]
        [(character-token? less-than)
         (error "Cannot make doctype token without a bang")]
        [else
         (error "Cannot make doctype token without a less-than")]))

; https://html.spec.whatwg.org/multipage/parsing.html#doctype-state
(: DOCTYPE (-> (Listof Token)))
(define (DOCTYPE)
  (current-misc (list))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-doctype (get-current-location) #f)
               (parameterize ([current-doctype-quirky? #t])
                 (make-doctype)))]
        [(html5-whitespace? c)
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (before-DOCTYPE-name)]
        [(char=? #\> c)
         (before-DOCTYPE-name)]
        [else
         (cons (missing-whitespace-before-doctype-name (get-current-location) #f)
               (before-DOCTYPE-name))]))

(: before-DOCTYPE-name (-> (Listof Token)))
(define (before-DOCTYPE-name)
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-doctype (get-current-location) #f)
               (parameterize ([current-doctype-quirky? #t])
                 (make-doctype)))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (cond
           [(html5-whitespace? c)
            (add-to-misc-pile ct)
            (before-DOCTYPE-name)]
           [(ascii-uppercase? c)
            (DOCTYPE-name (list (downcase-character ct)))]
           [(char=? c #\u0000)
            (cons (unexpected-null-character (get-current-location) #f)
                  (DOCTYPE-name (list (replace-character ct))))]
           [(char=? c #\>)
            (cons (missing-doctype-name (get-current-location) #f)
                  (parameterize ([current-greater-than ct]
                                 [current-doctype-quirky? #t])
                    (list (make-doctype))))]
           [else
            (DOCTYPE-name (list ct))])]))

(: DOCTYPE-name (-> (Listof character-token)
                    (Listof Token)))
(define (DOCTYPE-name chars)
  (define c (peek-char))
  (cond [(eof-object? c)
         (parameterize ([current-doctype-quirky? #t]
                        [current-doctype-name (reverse chars)])
           (list (eof-in-doctype (get-current-location) #f)
                 (make-doctype)))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (cond
           [(html5-whitespace? c)
            (add-to-misc-pile ct)
            (after-DOCTYPE-name chars)]
           [(ascii-uppercase? c)
            (DOCTYPE-name (cons (downcase-character ct) chars))]
           [(char=? c #\>)
            (parameterize ([current-doctype-name (reverse chars)]
                           [current-greater-than ct])
              (list (make-doctype)))]
           [(char=? c #\u0000)
            (cons (unexpected-null-character (get-current-location) #f)
                  (DOCTYPE-name (cons (replace-character ct) chars)))]
           [else
            (DOCTYPE-name (cons ct chars))])]))

(: looking-at-PUBLIC? (-> Boolean))
(define (looking-at-PUBLIC?)
  (not (eq? #f (regexp-match-peek #rx"(?i:public)" (current-input-port)))))

(: looking-at-SYSTEM? (-> Boolean))
(define (looking-at-SYSTEM?)
  (not (eq? #f (regexp-match-peek #rx"(?i:system)" (current-input-port)))))

(: after-DOCTYPE-name (-> (Listof character-token)
                          (Listof Token)))
(define (after-DOCTYPE-name name)
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-doctype (get-current-location) #f)
               (parameterize ([current-doctype-quirky? #t]
                              [current-doctype-name (reverse name)])
                 (make-doctype)))]
        [(html5-whitespace? c)
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (after-DOCTYPE-name name)]
        [(char=? c #\>)
         (read-char)
         (list (parameterize ([current-greater-than (make-character-token/after-read c)]
                              [current-doctype-name (reverse name)])
                 (make-doctype)))]
        [(looking-at-PUBLIC?)
         (define chars (read-several (string-length "public")))
         (parameterize ([current-public-keyword chars]
                        [current-doctype-name (reverse name)])
           (after-DOCTYPE-public-keyword))]
        [(looking-at-SYSTEM?)
         (define chars (read-several (string-length "system")))
         (parameterize ([current-system-keyword chars]
                        [current-doctype-name (reverse name)])
           (after-DOCTYPE-system-keyword))]
        [else
         (cons (invalid-character-sequence-after-doctype-name (get-current-location) #f)
               (parameterize ([current-doctype-quirky? #t]
                              [current-doctype-name (reverse name)])
                 (bogus-DOCTYPE)))]))

(: bogus-DOCTYPE (-> (Listof Token)))
(define (bogus-DOCTYPE)
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (make-doctype))]
        [else
         (define loc (get-current-location))
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\>)
            (list (parameterize ([current-greater-than ct])
                    (make-doctype)))]
           [(#\u0000)
            (add-to-misc-pile (drop-character ct))
            (cons (unexpected-null-character loc #f)
                  (bogus-DOCTYPE))]
           [else
            (add-to-misc-pile (drop-character ct))
            (bogus-DOCTYPE)])]))


(: after-DOCTYPE-system-keyword (-> (Listof Token)))
(define (after-DOCTYPE-system-keyword)
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-doctype (get-current-location) #f)
               (parameterize ([current-doctype-quirky? #t])
                 (make-doctype)))]
        [(html5-whitespace? c)
         (read-char)
         (define ct (make-character-token/after-read c))
         (add-to-misc-pile ct)
         (before-DOCTYPE-system-identifier)]
        [(char=? c #\")
         (define loc (get-current-location))
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (cons (missing-whitespace-after-doctype-system-keyword loc #f)
               (DOCTYPE-system-identifier-double-quoted))]
        [(char=? c #\')
         (define loc (get-current-location))
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (cons (missing-whitespace-after-doctype-system-keyword loc #f)
               (DOCTYPE-system-identifier-single-quoted))]
        [(char=? c #\>)
         (define loc (get-current-location))
         (read-char)
         (list (missing-doctype-system-identifier loc #f)
               (parameterize ([current-doctype-quirky? #t]
                              [current-greater-than (make-character-token/after-read c)])
                 (make-doctype)))]
        [else
         (cons (missing-quote-before-doctype-system-identifier (get-current-location) #f)
               (parameterize ([current-doctype-quirky? #t])
                 (bogus-DOCTYPE)))]))


; https://html.spec.whatwg.org/multipage/parsing.html#after-doctype-public-keyword-state
(: after-DOCTYPE-public-keyword (-> (Listof Token)))
(define (after-DOCTYPE-public-keyword)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-doctype loc #f)
               (parameterize ([current-doctype-quirky? #t])
                 (make-doctype)))]
        [(html5-whitespace? c)
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (before-DOCTYPE-public-identifier)]
        [(char=? c #\")
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (cons (missing-whitespace-after-doctype-public-keyword loc #f)
               (DOCTYPE-public-identifier-double-quoted))]
        [(char=? c #\')
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (cons (missing-whitespace-after-doctype-public-keyword loc #f)
               (DOCTYPE-public-identifier-single-quoted))]
        [(char=? c #\>)
         (read-char)
         (list (missing-doctype-public-identifier loc #f)
               (parameterize ([current-doctype-quirky? #t]
                              [current-greater-than (make-character-token/after-read c)])
                 (make-doctype)))]
        [else
         (cons (missing-quote-before-doctype-public-identifier loc #f)
               (parameterize ([current-doctype-quirky? #t])
                 (bogus-DOCTYPE)))]))

(: DOCTYPE-public-identifier-double-quoted (->* ()
                                                ((Listof character-token))
                                                (Listof Token)))
(define (DOCTYPE-public-identifier-double-quoted [chars (list)])
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-doctype loc #f)
               (parameterize ([current-public-identifier (reverse chars)]
                              [current-doctype-quirky? #t])
                 (make-doctype)))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\")
            (add-to-misc-pile ct)
            (parameterize ([current-public-identifier (reverse chars)])
              (after-DOCTYPE-public-identifier))]
           [(#\u0000)
            (cons (unexpected-null-character loc #f)
                  (DOCTYPE-public-identifier-double-quoted (cons (replace-character ct) chars)))]
           [(#\>)
            (list (abrupt-doctype-public-identifier loc #f)
                  (parameterize ([current-public-identifier (reverse chars)]
                                 [current-doctype-quirky? #t]
                                 [current-greater-than (make-character-token/after-read c)])
                    (make-doctype)))]
           [else
            (DOCTYPE-public-identifier-double-quoted (cons ct chars))])]))

(: DOCTYPE-public-identifier-single-quoted (->* ()
                                                ((Listof character-token))
                                                (Listof Token)))
(define (DOCTYPE-public-identifier-single-quoted [chars (list)])
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-doctype loc #f)
               (parameterize ([current-public-identifier (reverse chars)]
                              [current-doctype-quirky? #t])
                 (make-doctype)))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\')
            (add-to-misc-pile ct)
            (parameterize ([current-public-identifier (reverse chars)])
              (after-DOCTYPE-public-identifier))]
           [(#\u0000)
            (cons (unexpected-null-character loc #f)
                  (DOCTYPE-public-identifier-single-quoted (cons (replace-character ct) chars)))]
           [(#\>)
            (list (abrupt-doctype-public-identifier loc #f)
                  (parameterize ([current-doctype-quirky? #t]
                                 [current-greater-than ct]
                                 [current-public-identifier (reverse chars)])
                    (make-doctype)))]
           [else
            (DOCTYPE-public-identifier-single-quoted (cons ct chars))])]))

(: DOCTYPE-system-identifier-double-quoted (->* ()
                                                ((Listof character-token))
                                                (Listof Token)))
(define (DOCTYPE-system-identifier-double-quoted [chars (list)])
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-doctype loc #f)
               (parameterize ([current-system-identifier (reverse chars)]
                              [current-doctype-quirky? #t])
                 (make-doctype)))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\")
            (add-to-misc-pile ct)
            (parameterize ([current-system-identifier (reverse chars)])
              (after-DOCTYPE-system-identifier))]
           [(#\u0000)
            (cons (unexpected-null-character loc #f)
                  (DOCTYPE-system-identifier-double-quoted (cons (replace-character ct) chars)))]
           [(#\>)
            (list (abrupt-doctype-public-identifier loc #f)
                  (parameterize ([current-doctype-quirky? #t]
                                 [current-greater-than ct]
                                 [current-system-identifier (reverse chars)])
                    (make-doctype)))]
           [else
            (DOCTYPE-system-identifier-double-quoted (cons ct chars))])]))

(: after-DOCTYPE-system-identifier (-> (Listof Token)))
(define (after-DOCTYPE-system-identifier)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-doctype loc #f)
               (parameterize ([current-doctype-quirky? #t])
                 (make-doctype)))]
        [(html5-whitespace? c)
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (after-DOCTYPE-system-identifier)]
        [(char=? #\> c)
         (read-char)
         (list (parameterize ([current-greater-than (make-character-token/after-read c)])
                 (make-doctype)))]
        [else
         (cons (unexpected-character-after-doctype-system-identifier loc (make-character-token c #:start loc))
               (bogus-DOCTYPE))]))

(: DOCTYPE-system-identifier-single-quoted (->* ()
                                                ((Listof character-token))
                                                (Listof Token)))
(define (DOCTYPE-system-identifier-single-quoted [chars (list)])
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-doctype loc #f)
               (parameterize ([current-doctype-quirky? #t]
                              [current-system-identifier (reverse chars)])
                 (make-doctype)))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\')
            (add-to-misc-pile ct)
            (parameterize ([current-system-identifier (reverse chars)])
              (after-DOCTYPE-system-identifier))]
           [(#\u0000)
            (cons (unexpected-null-character loc #f)
                  (DOCTYPE-system-identifier-single-quoted (cons (replace-character ct) chars)))]
           [(#\>)
            (list (abrupt-doctype-system-identifier loc #f)
                  (parameterize ([current-doctype-quirky? #t]
                                 [current-greater-than ct]
                                 [current-system-identifier (reverse chars)])
                    (make-doctype)))]
           [else
            (DOCTYPE-system-identifier-single-quoted (cons ct chars))])]))

(: before-DOCTYPE-public-identifier (-> (Listof Token)))
(define (before-DOCTYPE-public-identifier)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-doctype loc #f)
               (parameterize ([current-doctype-quirky? #t])
                 (make-doctype)))]
        [(html5-whitespace? c)
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (before-DOCTYPE-public-identifier)]
        [(char=? c #\")
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (DOCTYPE-public-identifier-double-quoted)]
        [(char=? c #\')
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (DOCTYPE-public-identifier-single-quoted)]
        [(char=? c #\>)
         (read-char)
         (list (missing-doctype-public-identifier loc #f)
               (parameterize ([current-doctype-quirky? #t]
                              [current-greater-than (make-character-token/after-read c)])
                 (make-doctype)))]
        [else
         (cons (missing-quote-before-doctype-public-identifier loc #f)
               (parameterize ([current-doctype-quirky? #t])
                 (bogus-DOCTYPE)))]))

(: after-DOCTYPE-public-identifier (-> (Listof Token)))
(define (after-DOCTYPE-public-identifier)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-doctype loc #f)
               (parameterize ([current-doctype-quirky? #t])
                 (make-doctype)))]
        [(html5-whitespace? c)
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (between-DOCTYPE-public-and-system-identifiers)]
        [(char=? c #\>)
         (read-char)
         (list (parameterize ([current-greater-than (make-character-token/after-read c)])
                 (make-doctype)))]
        [(char=? c #\")
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (cons (missing-whitespace-between-doctype-public-and-system-identifiers loc #f)
               (DOCTYPE-system-identifier-double-quoted))]
        [(char=? c #\')
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (cons (missing-whitespace-between-doctype-public-and-system-identifiers loc #f)
               (DOCTYPE-system-identifier-single-quoted))]
        [else
         (cons (missing-quote-before-doctype-system-identifier loc #f)
               (parameterize ([current-doctype-quirky? #t])
                 (bogus-DOCTYPE)))]))

(: between-DOCTYPE-public-and-system-identifiers (-> (Listof Token)))
(define (between-DOCTYPE-public-and-system-identifiers)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-doctype loc #f)
               (parameterize ([current-doctype-quirky? #t])
                 (make-doctype)))]
        [(html5-whitespace? c)
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (between-DOCTYPE-public-and-system-identifiers)]
        [(char=? c #\>)
         (read-char)
         (list (parameterize ([current-greater-than (make-character-token/after-read c)])
                 (make-doctype)))]
        [(char=? c #\")
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (DOCTYPE-system-identifier-double-quoted)]
        [(char=? c #\')
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (DOCTYPE-system-identifier-single-quoted)]
        [else
         (cons (missing-quote-before-doctype-system-identifier loc #f)
               (parameterize ([current-doctype-quirky? #t])
                 (bogus-DOCTYPE)))]))

(: before-DOCTYPE-system-identifier (-> (Listof Token)))
(define (before-DOCTYPE-system-identifier)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-doctype loc #f)
               (parameterize ([current-doctype-quirky? #t])
                 (make-doctype)))]
        [(html5-whitespace? c)
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (before-DOCTYPE-system-identifier)]
        [(char=? c #\")
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (DOCTYPE-system-identifier-double-quoted)]
        [(char=? c #\')
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (DOCTYPE-system-identifier-single-quoted)]
        [(char=? c #\>)
         (read-char)
         (list (missing-doctype-system-identifier loc #f)
               (parameterize ([current-doctype-quirky? #t]
                              [current-greater-than (make-character-token/after-read c)])
                 (make-doctype)))]
        [else
         (cons (missing-quote-before-doctype-system-identifier loc #f)
               (parameterize ([current-doctype-quirky? #t])
                 (bogus-DOCTYPE)))]))
