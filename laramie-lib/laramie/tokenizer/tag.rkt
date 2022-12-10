#lang typed/racket/base/optional

(provide tag-name
         end-tag-open
         before-attribute-name
         self-closing-start-tag
         appropriate-tag?)

(require racket/match
         racket/port
         syntax/parse/define
         (file "tokens.rkt")
         (file "../private/todo.rkt")
         (file "parameters.rkt")
         (file "stream.rkt")
         (file "types.rkt")
         (file "predicates.rkt")
         (file "characters.rkt")
         (file "comment.rkt")
         (file "predicates.rkt")
         (file "infrastructure.rkt"))

(module+ test
  (require typed/rackunit))

(define-simple-macro (with-additional-attribute a step ...)
  (parameterize ([current-attributes (cons a (current-attributes))]
                 [current-attr-name (list)]
                 [current-attr-separator #f]
                 [current-attr-value #f]
                 [current-attr-value-opener #f])
    step ...))

(: has-current-attr-name? (-> Boolean))
(define (has-current-attr-name?)
  (not (null? (current-attr-name))))

(: flatten-character-references (-> (Listof (U character-reference-token
                                               character-token))
                                    (Listof character-token)))
(define (flatten-character-references stuff)
  (cond [(null? stuff)
         (list)]
        [(character-reference-token? (car stuff))
         (append (character-reference-token-characters (car stuff))
                 (flatten-character-references (cdr stuff)))]
        [else
         (cons (car stuff)
               (flatten-character-references (cdr stuff)))]))

(: enumerate-attr-state-characters (-> (Listof character-token)))
(define (enumerate-attr-state-characters)
  (: value (Listof (U character-token character-reference-token)))
  (define value (or (current-attr-value) (list)))
  (define name (or (current-attr-name) (list)))
  (: opener (Listof character-token))
  (define o (current-attr-value-opener))
  (define opener (cond [(eq? #f o) (list)]
                       [else (list o)]))
  (define s (current-attr-separator))
  (define separator (cond [(eq? #f s) (list)]
                          [else (list s)]))
  (append opener
          name
          separator
          (flatten-character-references value)))

(: ensure-character-list (-> (Option (U Char
                                        (Listof Char)))
                             (Listof Char)))
(define (ensure-character-list thing)
  (cond [(eq? #f thing)
         (list)]
        [(char? thing)
         (list thing)]
        [(list? thing)
         thing]))

(: input-char (-> (U Char (Pairof Char Char))
                  Char))
(define (input-char c)
  (cond [(char? c) c]
        [else (car c)]))

(: output-char (-> (U Char (Pairof Char Char))
                   Char))
(define (output-char c)
  (cond [(char? c) c]
        [else (cdr c)]))

(: enumerate-tag-state-characters (-> (Listof (U character-token
                                                 character-reference-token))))
(define (enumerate-tag-state-characters)
  (define lt (current-less-than))
  (define opener (cond [(eq? #f lt) (list)]
                       [else (list lt)]))
  (define slash (current-end-tag-slash))
  (define closer (cond [(eq? #f slash) (list)]
                       [else (list slash)]))
  (define scc (current-tag-self-closing-char))
  (define closing-char (cond [(eq? #f scc) (list)]
                             [else (list scc)]))
  (define attrs (attrs->tokens (current-attributes)))
  (define name (or (current-tag-name) (list)))
  (append opener
          name
          attrs
          closing-char
          closer))

(: available-characters (-> (Listof character-token)))
(define (available-characters)
  (sort-character-tokens
   (flatten-character-references
    (append (enumerate-tag-state-characters)
            (enumerate-attr-state-characters)
            (current-misc)))))

(: drop-available-characters (-> (Listof character-token)))
(define (drop-available-characters)
  (map drop-character (available-characters)))

(: make-tag (-> (U start-tag-token end-tag-token)))
(define (make-tag)
  (define name (or (current-tag-name) (list)))
  (define less-than (current-less-than))
  (cond [(null? name)
         (error "Cannot make tag with empty name!")]
        [(not (character-token? less-than))
         (error "Cannnot make tag with no less-than")]
        [else
         (define attrs (reverse (current-attributes)))
         (define self-closing-char (current-tag-self-closing-char))
         (define closer (current-greater-than))
         (define slash (current-end-tag-slash))
         (if (current-tag-ending?)
             (end-tag-token (span-start less-than)
                            (get-current-location)
                            less-than
                            name
                            attrs
                            self-closing-char
                            closer
                            (current-misc)
                            slash)
             (start-tag-token (span-start less-than)
                              (get-current-location)
                              less-than
                              name
                              attrs
                              self-closing-char
                              closer
                              (current-misc)))]))

(: make-attribute (-> (Option character-token)
                      attribute-token))
(define (make-attribute closing-quote)
  (define start (current-attr-starter))
  (define vo (current-attr-value-opener))
  (define v (current-attr-value))
  (unless (character-token? start)
    (error "Cannot make attribute without known starting location"))
  (cond [(and (eq? #f vo)
              (eq? #f v)
              (eq? #f closing-quote))
         (attribute-token (span-start start)
                   (get-current-location)
                   (current-attr-name)
                   (current-attr-separator)
                   #f)]
        [else
         (attribute-token (span-start start)
                   (get-current-location)
                   (current-attr-name)
                   (current-attr-separator)
                   (quoted-attr-value vo
                                      v
                                      closing-quote))]))

(: make-attribute/no-value (-> attribute-token))
(define (make-attribute/no-value)
  (define start (current-attr-starter))
  (unless (character-token? start)
    (error "Cannot make attribute without known starting location"))
  (attribute-token (span-start start)
                   (get-current-location)
                   (current-attr-name)
                   (current-attr-separator)
                   #f))

(: appropriate-tag? (-> (U String
                           (Listof character-token))
                        Boolean))
(define (appropriate-tag? name)
  (define s (list->string (enumerate-output-characters (current-tag-name))))
  (cond [(string? name)
         (string=? name s)]
        [else
         (string=? (list->string (enumerate-output-characters name))
                   s)]))

; https://html.spec.whatwg.org/multipage/parsing.html#tag-name-state
(: tag-name/loc (-> (Listof Token)))
(define (tag-name/loc)
  (: helper (-> (Listof character-token)
                (Listof Token)))
  (define (helper buffer)
    (define c (peek-char))
    (cond [(eof-object? c)
           (parameterize ([current-tag-name (reverse buffer)])
             (drop-available-characters))]
          [else
           (define loc (get-current-location))
           (read-char)
           (define ct (make-character-token/after-read c))
           (cond
             [(html5-whitespace? c)
              (add-to-misc-pile ct)
              (parameterize ([current-tag-name (reverse buffer)])
                (before-attribute-name/loc))]
             [(ascii-uppercase? c)
              (helper (cons (downcase-character ct)
                            buffer))]
             [(char=? c #\/)
              (parameterize ([current-tag-name (reverse buffer)]
                             [current-tag-self-closing-char ct])
                (self-closing-start-tag/loc))]
             [(char=? c #\>)
              (parameterize ([current-tag-name (reverse buffer)]
                             [current-greater-than ct])
               (list (make-tag)))]
             [(char=? c #\u0000)
              (cons (unexpected-null-character loc #f)
                    (helper (cons (replace-character ct) buffer)))]
             [else
              (helper (cons ct buffer))])]))
  (helper (list)))

(define tag-name (tokenizer tag-name/loc))

; https://html.spec.whatwg.org/multipage/parsing.html#self-closing-start-tag-state
(: self-closing-start-tag/loc (-> (Listof Token)))
(define (self-closing-start-tag/loc)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (cons (eof-in-tag loc #f)
               (drop-available-characters))]
        [else
         (case c
           [(#\>)
            (read-char)
            (list (parameterize ([current-greater-than (make-character-token/after-read c)])
                    (cond [(has-current-attr-name?)
                           (with-additional-attribute (make-attribute #f)
                             (make-tag))]
                          [else
                           (make-tag)])))]
           [else
            (cons (unexpected-solidus-in-tag loc #f)
                  (before-attribute-name/loc))])]))

(define self-closing-start-tag
  (tokenizer self-closing-start-tag/loc))

; https://html.spec.whatwg.org/multipage/parsing.html#before-attribute-name-state
(: before-attribute-name/loc (-> (Listof Token)))
(define (before-attribute-name/loc)
  (define c (peek-char))
  (cond [(eof-object? c)
         (after-attribute-name)]
        [(html5-whitespace? c)
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (before-attribute-name/loc)]
        [else
         (define loc (get-current-location))
         (case c
           [(#\/ #\>)
            (after-attribute-name)]
           [(#\=)
            (read-char)
            (cons (unexpected-equals-sign-before-attribute-name loc #f)
                  (parameterize ([current-attr-starter (make-character-token/after-read c)])
                    (attribute-name (list (make-character-token/after-read c)))))]
           [else
            (parameterize ([current-attr-starter (make-character-token/after-read c)])
              (attribute-name))])]))

(define before-attribute-name
  (tokenizer before-attribute-name/loc))

; https://html.spec.whatwg.org/multipage/parsing.html#attribute-name-state
(: attribute-name (->* ()
                       ((Listof character-token))
                       (Listof Token)))
(define (attribute-name [buffer (list)])
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (parameterize ([current-attr-name (reverse buffer)])
           (after-attribute-name))]
        [(ascii-uppercase? c)
         (read-char)
         (attribute-name (cons (downcase-character (make-character-token/after-read c))
                               buffer))]
        [(html5-whitespace? c)
         (parameterize ([current-attr-name (reverse buffer)])
           (after-attribute-name))]
        [else
         (case c
           [(#\/)
            (parameterize ([current-attr-name (reverse buffer)]
                           [current-tag-self-closing-char (make-character-token c #:start loc)])
              (after-attribute-name))]
           [(#\>)
            (parameterize ([current-attr-name (reverse buffer)]
                           [current-greater-than (make-character-token c #:start loc)])
             (after-attribute-name))]
           [(#\=)
            (read-char)
            (define ct (make-character-token/after-read c))
            (parameterize ([current-attr-name (reverse buffer)]
                           [current-attr-separator ct])
              (before-attribute-value))]
           [(#\u0000)
            (read-char)
            (define ct (make-character-token/after-read c))
            (cons (unexpected-null-character loc #f)
                  (attribute-name (cons (replace-character ct) buffer)))]
           [(#\" #\' #\<)
            (read-char)
            (define ct (make-character-token/after-read c))
            (cons (unexpected-character-in-attribute-name loc ct)
                  (attribute-name (cons ct buffer)))]
           [else
            (read-char)
            (define ct (make-character-token/after-read c))
            (attribute-name (cons ct buffer))])]))

; https://html.spec.whatwg.org/multipage/parsing.html#after-attribute-name-state
(: after-attribute-name (-> (Listof Token)))
(define (after-attribute-name)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (cons (eof-in-tag loc #f)
               (drop-available-characters))]
        [(html5-whitespace? c)
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (after-attribute-name)]
        [else
         (case c
           [(#\/)
            (read-char)
            (parameterize ([current-end-tag-slash (make-character-token/after-read c)])
              (self-closing-start-tag/loc))]
           [(#\=)
            (before-attribute-value)]
           [(#\>)
            (read-char)
            (list
             (parameterize ([current-greater-than (make-character-token/after-read c)])
               (cond ([has-current-attr-name?]
                      (with-additional-attribute (make-attribute #f)
                        (make-tag)))
                     [else
                      (make-tag)])))]
           [else
            (with-additional-attribute (make-attribute #f)
              (attribute-name))])]))

; https://html.spec.whatwg.org/multipage/parsing.html#before-attribute-value-state
(: before-attribute-value (-> (Listof Token)))
(define (before-attribute-value)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (attribute-value-unquoted)]
        [(html5-whitespace? c)
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (before-attribute-value)]
        [else
         (case c
           [(#\")
            (read-char)
            (parameterize ([current-attr-value-opener (make-character-token/after-read c)])
              (attribute-value-double-quoted))]
           [(#\')
            (read-char)
            (parameterize ([current-attr-value-opener (make-character-token/after-read c)])
              (attribute-value-single-quoted))]
           [(#\>)
            (read-char)
            (parameterize ([current-greater-than (make-character-token/after-read c)])
              (with-additional-attribute (make-attribute/no-value)
                (list (missing-attribute-value loc #f)
                      (make-tag))))]
           [else
            (attribute-value-unquoted)])]))

(: attr-value->tokens (-> quoted-attr-value
                          (Listof (U character-token
                                     character-reference-token))))
(define (attr-value->tokens value)
  (define opener (quoted-attr-value-opener value))
  (define closer (quoted-attr-value-closer value))
  (append (cond [(eq? #f opener) (list)]
                [else (list opener)])
          (or (quoted-attr-value-content value)
              (list))
          (cond [(eq? #f closer) (list)]
                [else (list closer)])))

(: attr->tokens (-> attribute-token
                    (Listof (U character-token
                               character-reference-token))))
(define (attr->tokens a)
  (define equals (attribute-token-equals a))
  (append (attribute-token-name a)
          (cond [(eq? #f equals) (list)]
                [else (list equals)])
          (match (attribute-token-value a)
            [(? quoted-attr-value? qv)
             (attr-value->tokens qv)]
            [else (list)])))

(: attrs->tokens (-> (Listof attribute-token)
                     (Listof (U character-token
                                character-reference-token))))
(define (attrs->tokens attrs)
  (cond [(null? attrs)
         (list)]
        [else
         (append (attr->tokens (car attrs))
                 (attrs->tokens (cdr attrs)))]))

; https://html.spec.whatwg.org/multipage/parsing.html#attribute-value-(unquoted)-state
(: attribute-value-unquoted (->* ()
                                 ((Listof (U character-token character-reference-token)))
                                 (Listof Token)))
(define (attribute-value-unquoted [buffer (list)])
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (parameterize ([current-attr-value (reverse buffer)])
           (cons (eof-in-tag loc #f)
                 (parameterize ([current-attr-value (reverse buffer)])
                   (drop-available-characters))))]
        [(html5-whitespace? c)
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (parameterize ([current-attr-value (reverse buffer)])
           (with-additional-attribute (make-attribute #f)
             (before-attribute-name/loc)))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\&)
            (define cr-tokens
              (parameterize ([currently-in-attribute-value? #t])
                (character-reference ct)))
            (define acceptable (filter (lambda (x)
                                         (or (character-token? x)
                                             (character-reference-token? x)))
                                       cr-tokens))
            (define errors (filter tokenizer-error? cr-tokens))
            (append errors
                    (attribute-value-unquoted (append (reverse acceptable) buffer)))]
           [(#\>)
            (parameterize ([current-greater-than ct]
                           [current-attr-value (reverse buffer)])
              (with-additional-attribute (make-attribute #f)
                (list (make-tag))))]
           [(#\" #\' #\< #\= #\`)
            (cons (unexpected-character-in-unquoted-attribute-value loc ct)
                  (attribute-value-unquoted (cons ct buffer)))]
           [else
            (attribute-value-unquoted (cons ct buffer))])]))

; https://html.spec.whatwg.org/multipage/parsing.html#attribute-value-(single-quoted)-state
(: attribute-value-single-quoted (->* ()
                                      ((Listof (U character-token
                                                  character-reference-token)))
                                      (Listof Token)))
(define (attribute-value-single-quoted [buffer (list)])
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (cons (eof-in-tag loc #f)
               (parameterize ([current-attr-value (reverse buffer)])
                 (drop-available-characters)))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\')
            (parameterize ([current-attr-value (reverse buffer)])
              (after-attribute-value-quoted ct))]
           [(#\&)
            (define cr-tokens (parameterize ([currently-in-attribute-value? #t])
                                (character-reference ct)))
            (define acceptable (filter (lambda (x)
                                         (or (character-token? x)
                                             (character-reference-token? x)))
                                       cr-tokens))
            (define errors (filter tokenizer-error? cr-tokens))
            (append errors
                    (attribute-value-single-quoted (append (reverse acceptable) buffer)))]
           [(#\u0000)
            (cons (unexpected-null-character loc #f)
                  (attribute-value-single-quoted (cons (replace-character ct)
                                                       buffer)))]
           [else
            (attribute-value-single-quoted (cons ct buffer))])]))

; https://html.spec.whatwg.org/multipage/parsing.html#attribute-value-(double-quoted)-state
(: attribute-value-double-quoted (->* ()
                                      ((Listof (U character-token
                                                  character-reference-token)))
                                      (Listof Token)))
(define (attribute-value-double-quoted [buffer (list)])
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (cons (eof-in-tag loc #f)
               (parameterize ([current-attr-value (reverse buffer)])
                 (drop-available-characters)))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\")
            (parameterize ([current-attr-value (reverse buffer)])
              (after-attribute-value-quoted ct))]
           [(#\&)
            (define cr-tokens (parameterize ([currently-in-attribute-value? #t])
                                (character-reference ct)))
            (define acceptable (filter (lambda (x)
                                         (or (character-token? x)
                                             (character-reference-token? x)))
                                       cr-tokens))
            (define errors (filter tokenizer-error? cr-tokens))
            (append errors
                    (attribute-value-double-quoted (append (reverse acceptable) buffer)))]
           [(#\u0000)
            (cons (unexpected-null-character loc #f)
                  (attribute-value-double-quoted (cons (replace-character ct) buffer)))]
           [else
            (attribute-value-double-quoted (cons ct buffer))])]))

; https://html.spec.whatwg.org/multipage/parsing.html#end-tag-open-state
(: end-tag-open (-> character-token
                    character-token
                    (Listof (U Token tokenizer))))
(define (end-tag-open less-than solidus)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list less-than solidus)]
        [(ascii-alpha? c)
         (parameterize ([current-tag-name (list)]
                        [current-tag-ending? #t]
                        [current-less-than less-than]
                        [current-end-tag-slash solidus])
           (tag-name/loc))]
        [(char=? c #\>)
         (read-char)
         (cons (missing-end-tag-name loc (make-character-token/after-read c))
               (drop-characters (list less-than solidus)))]
        [else
         (define bogus-scan (tokenizer-scanner bogus-comment))
         (define parameterized-scanner
           (lambda ()
             (parameterize ([current-less-than less-than]
                            [current-bang solidus])
               (bogus-scan))))
         (list (invalid-first-character-of-tag-name loc #f)
               (tokenizer parameterized-scanner))]))

; https://html.spec.whatwg.org/multipage/parsing.html#after-attribute-value-(quoted)-state
(: after-attribute-value-quoted (-> character-token
                                    (Listof Token)))
(define (after-attribute-value-quoted quote-char)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (append (list (eof-in-tag loc #f))
                 (drop-available-characters)
                 (list (drop-character quote-char)))]
        [(html5-whitespace? c)
         (read-char)
         (add-to-misc-pile (make-character-token/after-read c))
         (with-additional-attribute (make-attribute quote-char)
           (before-attribute-name/loc))]
        [(char=? c #\/)
         (read-char)
         (parameterize ([current-tag-self-closing-char (make-character-token/after-read c)])
           (with-additional-attribute (make-attribute quote-char)
             (self-closing-start-tag/loc)))]
        [(char=? c #\>)
         (read-char)
         (parameterize ([current-greater-than (make-character-token/after-read c)])
           (with-additional-attribute (make-attribute quote-char)
             (list (make-tag))))]
        [else
         (with-additional-attribute (make-attribute quote-char)
           (cons (missing-whitespace-between-attributes loc #f)
                 (before-attribute-name/loc)))]))
