#lang typed/racket/base

(provide make-character-token
         make-character-token/after-read
         make-string-token
         make-doctype-token
         make-comment-token
         tag-attrs
         doctype-token-is-quirky?
         ensure-character-list
         characters-equal?
         #;switch-to
         #;prepend-token
         quoted-attr->string
         tag-name-equals?
         tag-token-attr-value
         tag-token-self-closing?
         end-tag->start-tag
         strip-start-tag-attributes
         tag-token-attr-ref
         enumerate-input-characters
         enumerate-output-characters
         drop-character
         drop-characters
         replace-character
         downcase-character
         #;substitute-characters
         extend-characters-with-location
         sort-character-tokens
         whitespace-token?
         null-character?
         deleted-character?
         newline-character?
         empty-string?
         string-starts-with-whitespace?
         string-starts-with-null-character?
         extract-first-character
         drop-first-character
         string-token->character-tokens
         character-tokens->string)

(require syntax/parse/define
         (file "types.rkt")
         (file "../private/todo.rkt")
         (file "predicates.rkt")
         (file "stream.rkt")
         (file "parameters.rkt")
         (file "infrastructure.rkt"))

(module+ test
  (require racket/format
           typed/rackunit))

(: make-character-token (->* ((U Char
                                 (Pairof Char (Option Char))
                                 (Pairof (Listof Char) Char)))
                             (#:start location)
                             character-token))
(define (make-character-token c
                              #:start [start (current-location)])
  (character-token start
                   (add-to-location start (enumerate-input-characters c))
                   c))

(: make-character-token/after-read (-> Char
                                       character-token))
(define (make-character-token/after-read c)
  (define start (subtract-from-location (get-current-location) c))
  (make-character-token c #:start start))

(: make-string-token (->* (String)
                          (#:start location)
                          string-token))
(define (make-string-token s
                           #:start [start (current-location)])
  (string-token start
                (add-to-location start (bytes-length (string->bytes/utf-8 s)))
                s))

(: make-doctype-token (->* (#:start location
                            #:stop location
                            #:less-than character-token
                            #:bang character-token)
                           (#:doctype (Listof character-token)
                            #:force-quirks? Boolean
                            #:name (Listof character-token)
                            #:public-keyword (Option (Listof character-token))
                            #:public-identifier (Option (Listof character-token))
                            #:system-keyword (Option (Listof character-token))
                            #:system-identifier (Option (Listof character-token))
                            #:greater-than (Option character-token)
                            #:misc (Listof character-token))
                           doctype-token))
(define (make-doctype-token #:start start
                            #:stop stop
                            #:less-than less-than
                            #:bang bang
                            #:doctype [doctype (list)]
                            #:force-quirks? [force-quirks? #f]
                            #:name [name (list)]
                            #:public-keyword [public-keyword #f]
                            #:public-identifier [public-identifier #f]
                            #:system-keyword [system-keyword #f]
                            #:system-identifier [system-identifier #f]
                            #:greater-than [greater-than #f]
                            #:misc [misc (list)])
  (doctype-token start
                 stop
                 less-than
                 bang
                 doctype
                 name
                 public-keyword
                 public-identifier
                 system-keyword
                 system-identifier
                 greater-than
                 force-quirks?
                 misc))

; TODO: Needs to be expanded with the exceptions listed at:
;
; https://html.spec.whatwg.org/multipage/parsing.html#the-initial-insertion-mode
;
(: doctype-token-is-quirky? (-> doctype-token Boolean))
(define (doctype-token-is-quirky? token)
  (doctype-token-quirky? token))

(: tag-attrs (-> (U tag-token end-tag-token)
                 (Listof attribute-token)))
(define (tag-attrs token)
  (tag-token-attrs token))

(: make-comment-token (->* (#:start location
                            #:stop location
                            #:less-than character-token
                            #:bang character-token)
                           (#:opening-dashes (Option (List character-token character-token))
                            #:content (Listof character-token)
                            #:bogus? Boolean
                            #:closing-dashes (Option (U (List character-token)
                                                        (List character-token character-token)))
                            #:greater-than (Option character-token))
                           comment-token))
(define (make-comment-token #:start start
                            #:stop stop
                            #:less-than less-than
                            #:bang bang
                            #:opening-dashes [opening-dashes #f]
                            #:content [content (list)]
                            #:bogus? [bogus? #f]
                            #:closing-dashes [closing-dashes #f]
                            #:greater-than [greater-than #f])
  (comment-token start
                 stop
                 less-than
                 bang
                 opening-dashes
                 content
                 bogus?
                 closing-dashes
                 greater-than))

(: ensure-character-list (-> (Option (U Char
                                        (Listof Char)
                                        String))
                             (Listof Char)))
(define (ensure-character-list thing)
  (cond
    [(eq? #f thing)
     (list)]
    [(char? thing)
     (list thing)]
    [(string? thing)
     (string->list thing)]
    [else
     thing]))

(: enumerate-input-characters (-> (Option
                                   (U Char
                                      character-token
                                      character-reference-token
                                      (Pairof Char (Option Char))
                                      (Pairof (Listof Char) (Option Char))
                                      String
                                      Token
                                      attribute-token
                                      quoted-attr-value
                                      location
                                      tokenizer
                                      (Listof (U character-token
                                                 character-reference-token
                                                 Char
                                                 (Pairof Char (Option Char))
                                                 String
                                                 Token
                                                 attribute-token
                                                 tokenizer))))
                                  (Listof Char)))
(define (enumerate-input-characters token)
  (cond [(eq? #f token)
         (list)]
        [(char? token)
         (list token)]
        [(string-token? token)
         (string->list (string-token-content token))]
        [(character-token? token)
         (define content (character-token-content token))
         (cond [(char? content)
                (list content)]
               [(char? (car content))
                (list (car content))]
               [else
                (car content)])]
        [(character-reference-token? token)
         (enumerate-input-characters (character-reference-token-characters token))]
        [(and (pair? token)
              (not (list? token)))
         (cond [(char? (car token))
                (list (car token))]
               [else
                (car token)])]
        [(string? token)
         (string->list token)]
        [(tokenizer? token)
         (list)]
        [(and (pair? token)
              (not (list? token)))
         (cond [(char? (car token))
                (list (car token))]
               [(list? (car token))
                (car token)])]
        [(null? token)
         (list)]
        [(location? token)
         ; character missing; returning empty list
         (list)]
        [(quoted-attr-value? token)
         (define opener (quoted-attr-value-opener token))
         (define closer (quoted-attr-value-closer token))
         (append (enumerate-input-characters opener)
                 (enumerate-input-characters (quoted-attr-value-content token))
                 (enumerate-input-characters closer))]
        [(attribute-token? token)
         (append (enumerate-input-characters (attribute-token-name token))
                 (cond [(eq? #f (attribute-token-equals token)) (list)]
                       [else (list #\=)])
                 (let ([v (attribute-token-value token)])
                   (cond [(eq? #f v)
                          (list)]
                         [else
                          (enumerate-input-characters v)])))]
        [(list? token)
         (append (enumerate-input-characters (car token))
                 (enumerate-input-characters (cdr token)))]
        [(start-tag-token? token)
         (append (list #\<)
                 (enumerate-input-characters (tag-token-name token))
                 (enumerate-input-characters (tag-token-attrs token))
                 (ensure-character-list (cond [(eq? #f (tag-token-self-closing-char token))
                                               (list)]
                                              [(list #\/)]))
                 (ensure-character-list (cond [(eq? #f (tag-token-greater-than token))
                                               (list)]
                                              (list #\>)))
                 (enumerate-input-characters (tag-token-misc token)))]
        [(end-tag-token? token)
         (append (list #\<)
                 (enumerate-input-characters (tag-token-name token))
                 (enumerate-input-characters (tag-token-attrs token))
                 (ensure-character-list (cond [(eq? #f (tag-token-self-closing-char token))
                                               (list)]
                                              [else #\/]))
                 (list #\/)
                 (ensure-character-list (cond [(eq? #f (tag-token-greater-than token))
                                               (list)]
                                              [else (list #\>)]))
                 (enumerate-input-characters (tag-token-misc token)))]
        [(tokenizer-error? token)
         (cond [(or (unexpected-character-in-unquoted-attribute-value? token)
                    (character-reference-outside-unicode-range? token)
                    (missing-semicolon-after-character-reference? token)
                    (surrogate-character-reference? token)
                    (absence-of-digits-in-numeric-character-reference? token)
                    (unexpected-character-after-doctype-system-identifier? token)
                    (unexpected-character-in-attribute-name? token)
                    (unexpected-question-mark-instead-of-tag-name? token)
                    (invalid-character-sequence-after-doctype-name? token))
                ; these errors contains data that gets repeated elsewhere; their
                ; content is thus merely informational
                (list)]
               [else
                (define v (tokenizer-error-content token))
                (cond [(character-token? v)
                       (enumerate-input-characters v)]
                      [(and (list? v)
                            (andmap character-token? v))
                       (enumerate-input-characters v)]
                      [else ; too much information has been lost
                       (list)])])]
        [(comment-token? token)
         (append (enumerate-input-characters (list (comment-token-less-than token)
                                                   (comment-token-bang token)))
                 (enumerate-input-characters (comment-token-opening-dashes token))
                 (enumerate-input-characters (comment-token-content token))
                 (enumerate-input-characters (comment-token-closing-dashes token))
                 (enumerate-input-characters (comment-token-greater-than token)))]
        [(doctype-token? token)
         (append (enumerate-input-characters (doctype-token-less-than token))
                 (enumerate-input-characters (doctype-token-bang token))
                 (enumerate-input-characters (doctype-token-doctype token))
                 (enumerate-input-characters (doctype-token-name token))
                 (enumerate-input-characters (doctype-token-public-keyword token))
                 (enumerate-input-characters (doctype-token-public token))
                 (enumerate-input-characters (doctype-token-system-keyword token))
                 (enumerate-input-characters (doctype-token-system token))
                 (enumerate-input-characters (doctype-token-greater-than token))
                 (enumerate-input-characters (doctype-token-misc token)))]
        [else
         (error (format "Unable to handle token ~a" token))]))

(: enumerate-output-characters (-> (Option
                                    (U Char
                                       (Pairof Char (Option Char))
                                       String
                                       Token
                                       attribute-token
                                       quoted-attr-value
                                       location
                                       tokenizer
                                       (Listof (U Char
                                                  (Pairof Char (Option Char))
                                                  String
                                                  Token
                                                  attribute-token
                                                  tokenizer))))
                                   (Listof Char)))
(define (enumerate-output-characters token)
  (cond [(eq? #f token)
         (list)]
        [(char? token)
         (list token)]
        [(character-token? token)
         (define content (character-token-content token))
         (cond [(char? content) (list content)]
               [(char? (cdr content)) (list (cdr content))]
               [else (list)])]
        [(and (pair? token)
              (not (list? token)))
         (cond [(char? (cdr token))
                (list (cdr token))]
               [else (list)])]
        [(string? token)
         (string->list token)]
        [(tokenizer? token)
         (list)]
        [(and (pair? token)
              (not (list? token)))
         (cond [(char? (cdr token))
                (list (cdr token))]
               [(list? (cdr token))
                (cdr token)]
               [(eq? #f (cdr token))
                (list)])]
        [(string? token)
         (string->list token)]
        [(null? token)
         (list)]
        [(location? token)
         ; character missing; returning empty list
         (list)]
        [(stream-error? token)
         ; not enough information available
         (list)]
        [(quoted-attr-value? token)
         (define opener (quoted-attr-value-opener token))
         (define closer (quoted-attr-value-closer token))
         (append (enumerate-output-characters opener)
                 (enumerate-output-characters (or (quoted-attr-value-content token) (list)))
                 (enumerate-output-characters closer))]
        [(attribute-token? token)
         (append (enumerate-output-characters (attribute-token-name token))
                 (cond [(eq? #f (attribute-token-equals token)) (list)]
                       [else (list #\=)])
                 (let ([v (attribute-token-value token)])
                   (cond [(eq? #f v)
                          (list)]
                         [else
                          (enumerate-output-characters v)])))]
        [(list? token)
         (append (enumerate-output-characters (car token))
                 (enumerate-output-characters (cdr token)))]
        [(start-tag-token? token)
         (append (list #\<)
                 (enumerate-output-characters (tag-token-name token))
                 (enumerate-output-characters (tag-token-attrs token))
                 (ensure-character-list (cond [(eq? #f (tag-token-self-closing-char token))
                                               (list)]
                                              [(list #\/)]))
                 (ensure-character-list (cond [(eq? #f (tag-token-greater-than token))
                                               (list)]
                                              [else (list #\>)])))]
        [(end-tag-token? token)
         (append (list #\<)
                 (enumerate-output-characters (tag-token-name token))
                 (enumerate-output-characters (tag-token-attrs token))
                 (ensure-character-list (cond [(eq? #f (tag-token-self-closing-char token))
                                               (list)]
                                              [else #\/]))
                 (list #\/)
                 (ensure-character-list (cond [(eq? #f (tag-token-greater-than token))
                                               (list)]
                                              [else (list #\>)])))]
        [(tokenizer-error? token)
         (define v (tokenizer-error-content token))
         (cond [(char? v)
                (list v)]
               [(and (list? v)
                     (andmap char? v))
                v]
               [else ; too much information has been lost
                (list)])]
        [(comment-token? token)
         (append (list #\< #\!)
                 (cond [(eq? #f (comment-token-opening-dashes token)) (list)]
                       [else (list #\- #\-)])
                 (enumerate-output-characters (comment-token-content token))
                 (cond [(eq? #f (comment-token-closing-dashes token)) (list)]
                       [else (list #\- #\-)])
                 (cond [(eq? #f (comment-token-greater-than token)) (list)]
                       [else (list #\>)]))]
        [(doctype-token? token)
         (append (list #\< #\!)
                 (enumerate-output-characters (doctype-token-doctype token))
                 (enumerate-output-characters (doctype-token-name token))
                 (enumerate-output-characters (doctype-token-public token))
                 (enumerate-output-characters (doctype-token-system token))
                 (cond [(eq? #f (doctype-token-greater-than token)) (list) ]
                       [else (list #\>)]))]
        [(stream-error? token)
         ; character missing
         (list)]
        [else
         (error (format "Unable to handle token ~a" token))]))

(: maybe-location< (-> (Option location)
                       (Option location)
                       Boolean))
(define (maybe-location< loc-1 loc-2)
  (cond [(eq? #f loc-1)
         #f]
        [(eq? #f loc-2)
         #t]
        [else
         (location< loc-1 loc-2)]))

(: maybe-location=? (-> location
                        location
                        Boolean))
(define (maybe-location=? loc-1 loc-2)
  (cond [(eq? #f loc-1)
         (eq? #f loc-2)]
        [(eq? #f loc-2)
         #f]
        [else
         (location=? loc-1 loc-2)]))

(: location< (-> location location Boolean))
(define (location< loc-1 loc-2)
  (define line-1 (location-line loc-1))
  (define line-2 (location-line loc-2))
  (define col-1 (location-column loc-1))
  (define col-2 (location-column loc-2))
  (cond [(or (eq? line-1 #f)
             (eq? line-2 #f)
             (eq? col-1 #f)
             (eq? col-2 #f))
         #f]
        [(< line-1 line-2)
         #t]
        [(< line-2 line-1)
         #f]
        [else
         (< col-1 col-2)]))

(: location=? (-> location location Boolean))
(define (location=? loc-1 loc-2)
  (define line-1 (location-line loc-1))
  (define line-2 (location-line loc-2))
  (define col-1 (location-column loc-1))
  (define col-2 (location-column loc-2))
  (cond [(or (eq? line-1 #f)
             (eq? line-2 #f)
             (eq? col-1 #f)
             (eq? col-2 #f))
         #f]
        [(= line-1 line-2)
         (= col-1 col-2)]
        [else #f]))

(: location-gap? (-> location location Boolean))
(define (location-gap? loc-1 loc-2)
  (define line-1 (location-line loc-1))
  (define line-2 (location-line loc-2))
  (define col-1 (location-column loc-1))
  (define col-2 (location-column loc-2))
  (cond [(eq? line-1 #f) #f]
        [(eq? line-2 #f) #f]
        [(eq? col-1 #f) #f]
        [(eq? col-2 #f) #f]
        [(= line-1 line-2)
         (> col-2 (add1 col-1))]
        [(= line-2 (add1 line-1))
         (> col-2 0)]
        [else #t]))

(: characters-equal? (-> (Listof Char)
                         (Listof Char) Boolean))
(define (characters-equal? tokens-1 tokens-2)
  (cond [(null? tokens-1)
         (null? tokens-2)]
        [(null? tokens-2)
         #f]
        [else
         (and (char=? (car tokens-1)
                      (car tokens-2))
              (characters-equal? (cdr tokens-1)
                                 (cdr tokens-2)))]))

#;
(define (switch-to next-tokenizer)
  (tokenizer-step (list) next-tokenizer))

#;(: prepend-token (-> tokenizer-step Token tokenizer-step))
#;
(define (prepend-token step token)
  (define old-tokens (tokenizer-step-tokens step))
  (define new-tokens (cond [(eof-object? old-tokens)
                            (list token eof)]
                           [else
                            (cons token old-tokens)]))
  (struct-copy tokenizer-step
               step
               [tokens new-tokens]))

(: quoted-attr->string (-> quoted-attr-value String))
(define (quoted-attr->string qa)
  (define v (quoted-attr-value-content qa))
  (cond [(eq? #f v)
         ""]
        [else
         (list->string (enumerate-output-characters v))]))

(: tag-token-attr-value (-> tag-token String (Option String)))
(define (tag-token-attr-value token attribute)
  (: find-it (-> (Listof attribute-token)
                 (Option String)))
  (define (find-it attrs)
    (cond [(null? attrs) #f]
          [(string=? attribute (list->string (enumerate-output-characters (attribute-token-name (car attrs)))))
           (define v (attribute-token-value (car attrs)))
           (cond [(eq? #f v) #f]
                 [else (quoted-attr->string v)])]
          [else (find-it (cdr attrs))]))
  (find-it (tag-token-attrs token)))

(: tag-name-equals? (-> tag-token
                        (U String (Listof String))
                        Boolean))
(define (tag-name-equals? token s)
  (define n (list->string (enumerate-output-characters (tag-token-name token))))
  (cond [(string? s)
         (string=? s n)]
        [else
         (and (member n s string=?) #t)]))

(: tag-token-self-closing? (-> tag-token Boolean))
(define (tag-token-self-closing? token)
  (not (eq? #f (tag-token-self-closing-char token))))

(: end-tag->start-tag (-> end-tag-token start-tag-token))
(define (end-tag->start-tag token)
  (start-tag-token (span-start token)
                   (span-stop token)
                   (tag-token-less-than token)
                   (tag-token-name token)
                   (tag-token-attrs token)
                   (tag-token-self-closing-char token)
                   (tag-token-greater-than token)
                   (tag-token-misc token)))

(: strip-start-tag-attributes (-> start-tag-token start-tag-token))
(define (strip-start-tag-attributes token)
  (start-tag-token (span-start token)
                   (span-stop token)
                   (tag-token-less-than token)
                   (tag-token-name token)
                   (list)
                   (tag-token-self-closing-char token)
                   (tag-token-greater-than token)
                   (tag-token-misc token)))

(: tag-token-attr-ref (-> tag-token String (Option String)))
(define (tag-token-attr-ref token name)
  (define maybe-attr
    (findf (lambda ([another-attr : attribute-token])
             (string=? name (list->string (enumerate-output-characters (attribute-token-name another-attr)))))
           (tag-token-attrs token)))
  (cond [(eq? #f maybe-attr) #f]
        [else
         (define v (attribute-token-value maybe-attr))
         (cond [(eq? #f v) #f]
               [else (quoted-attr->string v)])]))

(: drop-character (-> character-token
                      character-token))
(define (drop-character ct)
  (define c (character-token-content ct))
  (define dropped-c (cond [(char? c) (cons c #f)]
                          [(char? (car c)) (cons (car c) #f)]
                          [else (cons (car c) #f)]))
  (struct-copy character-token
               ct
               [content dropped-c]))

(: drop-characters (-> (Listof character-token)
                       (Listof character-token)))
(define (drop-characters cs)
  (map drop-character cs))

(: replace-character (-> (U character-token
                            character-reference-token)
                         character-token))
(define (replace-character ct)
  (cond [(character-token? ct)
         (define c (character-token-content ct))
         (struct-copy character-token
                      ct
                      [content (cond [(char? c)
                                      (cons c #\ufffd)]
                                     [(char? (car c))
                                      (cons (car c) #\ufffd)]
                                     [else
                                      (cons (car c) #\ufffd)])])]
        [else
         (define chars (enumerate-input-characters ct))
         (character-token (span-start ct)
                          (span-stop ct)
                          (cons chars #f))]))

(: downcase-character (-> character-token
                          character-token))
(define (downcase-character ct)
  (define c (character-token-content ct))
  (struct-copy character-token
               ct
               [content (cond [(char? c)
                               (cons c (char-downcase c))]
                              [else c])]))

#;
(: substitute-characters (-> (Listof Char)
                             (U Char (List Char Char))
                             ReplacementChar))
#;
(define (substitute-characters original replacement)
  (cons original replacement))

(: extend-characters-with-location (-> location
                                       (Listof Char)
                                       (Listof character-token)))
(define (extend-characters-with-location loc chars)
  (cond [(null? chars)
         (list)]
        [else
         (define c (car chars))
         (cons (make-character-token c #:start loc)
               (extend-characters-with-location (add-to-location loc c)
                                                (cdr chars)))]))

(: location-< (-> location
                  location
                  Boolean))
(define (location-< loc-1 loc-2)
  (< (location-position loc-1)
     (location-position loc-2)))

(: character-token-< (-> character-token
                         character-token
                         Boolean))
(define (character-token-< ct-1 ct-2)
  (location-< (span-start ct-1)
              (span-start ct-2)))

(: sort-character-tokens (-> (Listof character-token)
                             (Listof character-token)))
(define (sort-character-tokens tokens)
  (sort tokens character-token-<))

(: whitespace-token? (-> (U character-token
                            character-reference-token)
                         Boolean))
(define (whitespace-token? token)
  (define c (cond [(character-token? token)
                   (character-token-content token)]
                  [else
                   (character-reference-token-result token)]))
  (and (char? c)
       (html5-whitespace? c)))

(: null-character? (-> (U character-token
                          character-reference-token)
                       Boolean))
(define (null-character? token)
  (define c (cond [(character-token? token)
                   (character-token-content token)]
                  [else
                   (character-reference-token-result token)]))
  (and (char? c)
       (char=? c #\u0000)))

(: deleted-character? (-> character-token
                          Boolean))
(define (deleted-character? token)
  (define c (character-token-content token))
  (and (pair? c)
       (eq? #f (cdr c))))

(: newline-character? (-> (U character-token
                             character-reference-token)
                          Boolean))
(define (newline-character? token)
  (define c (cond [(character-token? token)
                   (character-token-content token)]
                  [else
                   (character-reference-token-result token)]))
  (and (char? c)
       (char=? c #\u000a)))

(: empty-string? (-> string-token
                     Boolean))
(define (empty-string? token)
  (string=? "" (string-token-content token)))

(: string-starts-with-whitespace? (-> string-token
                                      Boolean))
(define (string-starts-with-whitespace? token)
  (define s (string-token-content token))
  (cond [(string=? "" s)
         #f]
        [else
         (html5-whitespace? (string-ref s 0))]))

(: string-starts-with-null-character? (-> string-token
                                          Boolean))
(define (string-starts-with-null-character? token)
  (define s (string-token-content token))
  (cond [(string=? "" s)
         #f]
        [else
         (char=? #\u0000 (string-ref s 0))]))

(: extract-first-character (-> string-token
                               (Option character-token)))
(define (extract-first-character token)
  (define s (string-token-content token))
  (cond [(string=? "" s)
         #f]
        [else
         (define c (string-ref s 0))
         (define start (span-start token))
         (define stop (add-to-location start c))
         (character-token start
                          stop
                          c)]))

(: drop-first-character (-> string-token
                            string-token))
(define (drop-first-character token)
  (define s (string-token-content token))
  (cond [(string=? "" s)
         token]
        [else
         (define c (string-ref s 0))
         (define start (span-start token))
         (define new-start (add-to-location start c))
         (define stop (span-stop token))
         (define new-content (substring s 1))
         (string-token new-start
                       stop
                       new-content)]))

(: string-token->character-tokens (-> string-token
                                      (Listof character-token)))
(define (string-token->character-tokens token)
  (define s (string-token-content token))
  (extend-characters-with-location (span-start token)
                                   (string->list s)))

(: character-token->char (-> character-token
                             (Option Char)))
(define (character-token->char ct)
  (define c (character-token-content ct))
  (cond [(char? c) c]
        [(eq? #f (cdr c)) #f]
        [else (cdr c)]))

(: character-tokens->string (-> (Listof character-token)
                                String))
(define (character-tokens->string tokens)
  (list->string (filter char? (map character-token->char tokens))))
