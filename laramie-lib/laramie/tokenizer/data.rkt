#lang typed/racket/base/optional

(provide data)

(require (file "types.rkt")
         (file "tokens.rkt")
         (file "characters.rkt")
         (prefix-in doctype: (file "doctype.rkt"))
         (file "parameters.rkt")
         (file "stream.rkt")
         (prefix-in comment: (file "comment.rkt"))
         (file "predicates.rkt")
         (file "infrastructure.rkt")
         (prefix-in tag: (file "tag.rkt"))
         (file "cdata.rkt"))

(module+ test
  (require typed/rackunit))

; https://html.spec.whatwg.org/multipage/parsing.html#data-state
(: data/loc (-> (Listof (U Token tokenizer))))
(define (data/loc)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list)]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\&)
            (character-reference ct)]
           [(#\<)
            (tag-open (make-character-token/after-read c))]
           [(#\u0000)
            (list (unexpected-null-character loc #f)
                  (drop-character ct))]
           [else
            (list ct)])]))

(define data (tokenizer data/loc))

; https://html.spec.whatwg.org/multipage/parsing.html#tag-open-state
(: tag-open (-> character-token
                (Listof (U Token tokenizer))))
(define (tag-open less-than)
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list less-than
               (eof-before-tag-name loc #f))]
        [(ascii-alpha? c)
         (define scanner (tokenizer-scanner tag:tag-name))
         (parameterize ([current-less-than less-than]
                        [current-tag-name (list)]
                        [current-tag-ending? #f]
                        [current-end-tag-slash #f]
                        [current-tag-self-closing-char #f]
                        [current-greater-than #f]
                        [current-misc (list)])
           (scanner))]
        [else
         (read-char)
         (define ct (make-character-token/after-read c))
         (case c
           [(#\!)
            (markup-declaration-open less-than ct)]
           [(#\/)
            (parameterize ([current-misc (list)])
              (tag:end-tag-open less-than ct))]
           [(#\?)
            (define scanner (tokenizer-scanner comment:bogus-comment))
            (cons (unexpected-question-mark-instead-of-tag-name loc #f)
                  (parameterize ([current-less-than less-than]
                                 [current-bang ct])
                    (scanner)))]
           [else
            (list less-than (invalid-first-character-of-tag-name loc ct))])]))

(: markup-declaration-open (-> character-token
                               character-token
                               (Listof (U Token tokenizer))))
(define (markup-declaration-open less-than exclamation-mark)
  (: fallback (->* ()
                   ((Listof character-token))
                   (Listof (U Token tokenizer))))
  (define (fallback [more-chars (list)])
    (define loc (get-current-location))
    (define scanner (tokenizer-scanner comment:bogus-comment))
    (define (do-it)
      (parameterize ([current-less-than less-than]
                     [current-bang exclamation-mark]
                     [current-comment-buffer more-chars])
        (scanner)))
    (list (incorrectly-opened-comment loc #f)
          (tokenizer do-it)))
  (define loc (get-current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (fallback)]
        [else
         (case c
           [(#\-)
            (read-char)
            (define ct (make-character-token/after-read c))
            (define loc-2 (get-current-location))
            (define c2 (peek-char))
            (cond [(and (char? c2)
                        (char=? #\- c2))
                   (read-char)
                   (define scanner (tokenizer-scanner comment:comment-start))
                   (parameterize ([current-less-than less-than]
                                  [current-bang exclamation-mark]
                                  [current-comment-buffer (list)]
                                  [current-closing-dashes #f]
                                  [current-opening-dashes (list ct (make-character-token/after-read c2))]
                                  [current-greater-than #f])
                     (scanner))]
                  [else
                   (fallback (list ct))])]
           [(#\d #\D #\[)
            (define next-seven (peek-several 7))
            (define next-seven/string (list->string next-seven))
            (cond [(regexp-match-peek #rx"^(?i:doctype)" (current-input-port))
                   (define next-seven (read-several 7))
                   (parameterize ([current-less-than less-than]
                                  [current-bang exclamation-mark]
                                  [current-doctype-opener next-seven])
                     (doctype:DOCTYPE))]
                  [(regexp-match-peek #rx"^\\[CDATA\\[" (current-input-port))
                   (define next-seven (read-several 7))
                   (define chars-so-far (append (list less-than exclamation-mark)
                                                next-seven))
                   (define scanner (tokenizer-scanner CDATA))
                   (append (drop-characters chars-so-far)
                           (scanner))]
                  [else
                   (fallback)])]
           [else
            (fallback)])]))
