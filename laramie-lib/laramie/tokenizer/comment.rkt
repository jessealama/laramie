#lang typed/racket/base/optional

(provide comment-start
         bogus-comment)

(require racket/list
         (file "tokens.rkt")
         (file "parameters.rkt")
         (file "types.rkt")
         (file "stream.rkt"))

(module+ test
  (require typed/rackunit))

(: make-comment (->* ()
                     (#:greater-than (Option character-token)
                      #:content (Option (Listof character-token))
                      #:closing-dashes (Option (U (List character-token)
                                                  (List character-token character-token))))
                     comment-token))
(define (make-comment #:greater-than [greater-than #f]
                      #:content [content #f]
                      #:closing-dashes [closing-dashes #f])
  (define less-than (current-less-than))
  (define bang (current-bang))
  (cond [(and (character-token? less-than)
              (character-token? bang))
         (make-comment-token #:start (span-start less-than)
                             #:stop (get-current-location)
                             #:less-than less-than
                             #:bang bang
                             #:opening-dashes (current-opening-dashes)
                             #:content (or content (current-comment-buffer))
                             #:bogus? #f
                             #:closing-dashes (or closing-dashes (current-closing-dashes))
                             #:greater-than (or greater-than (current-greater-than)))]
        [(character-token? less-than)
         (error "Cannot make comment without a bang")]
        [else
         (error "Cannot make comment without a less than")]))

(: make-bogus-comment (->* ()
                           (#:content (Listof character-token))
                           comment-token))
(define (make-bogus-comment #:content [content #f])
  (define less-than (current-less-than))
  (define bang (current-bang))
  (cond [(and (character-token? less-than)
              (character-token? bang))
         (make-comment-token #:start (span-start less-than)
                             #:stop (get-current-location)
                             #:less-than less-than
                             #:bang bang
                             #:content (or content (current-comment-buffer))
                             #:bogus? #t)]
        [(character-token? less-than)
         (error "Cannot make bogus comment without a bang")]
        [else
         (error "Cannot make bogus comment without a less-than")]))

; https://html.spec.whatwg.org/multipage/parsing.html#bogus-comment-state

(: bogus-comment/scan (-> (Listof Token)))
(define (bogus-comment/scan)
  (: keep-going (-> (Listof character-token)
                    (Listof Token)))
  (define (keep-going chars)
    (define c (peek-char))
    (cond [(eof-object? c)
           (list (make-bogus-comment #:content (reverse chars)))]
          [else
           (case c
             [(#\>)
              (list (make-bogus-comment #:content (reverse chars)))]
             [(#\u0000)
              (read-char)
              (keep-going (cons (replace-character (make-character-token/after-read c))
                                chars))]
             [else
              (read-char)
              (keep-going (cons (make-character-token/after-read c) chars))])]))
  (keep-going (list)))

(define bogus-comment (tokenizer bogus-comment/scan))

; https://html.spec.whatwg.org/multipage/parsing.html#comment-start-state

(: comment-start/scan (-> (Listof Token)))
(define (comment-start/scan)
  (define c (peek-char))
  (cond [(eof-object? c)
         (comment)]
        [else
         (case c
           [(#\-)
            (read-char)
            (comment-start-dash (make-character-token/after-read c))]
           [(#\>)
            (define loc (get-current-location))
            (read-char)
            (list (abrupt-closing-of-empty-comment loc #f)
                  (make-comment #:greater-than (make-character-token/after-read c)))]
           [else
            (comment)])]))

(define comment-start (tokenizer comment-start/scan))

; https://html.spec.whatwg.org/multipage/parsing.html#comment-state
(: comment (->* ()
                ((Listof character-token))
                (Listof Token)))
(define (comment [buffer (list)])
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-comment (get-current-location) #f)
               (make-comment #:content (reverse buffer)))]
        [else
         (define loc (get-current-location))
         (read-char)
         (case c
           [(#\<)
            (comment-less-than-sign (make-character-token/after-read c)
                                    buffer)]
           [(#\-)
            (comment-end-dash (make-character-token/after-read c)
                              buffer)]
           [(#\u0000)
            (cons (unexpected-null-character loc #f)
                  (comment (cons (replace-character (make-character-token/after-read c))
                                 buffer)))]
           [else
            (comment (cons (make-character-token/after-read c)
                           buffer))])]))

; https://html.spec.whatwg.org/multipage/parsing.html#comment-start-dash-state
(: comment-start-dash (->* (character-token)
                           ((Listof character-token))
                           (Listof Token)))
(define (comment-start-dash dash [buffer (list)])
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-comment (get-current-location) #f)
               (make-comment #:content (reverse buffer)
                             #:closing-dashes (list dash)))]
        [else
         (case c
           [(#\-)
            (read-char)
            (comment-end dash
                         (make-character-token/after-read c)
                         buffer)]
           [(#\>)
            (read-char)
            (list (abrupt-closing-of-empty-comment (get-current-location) #f)
                  (make-comment #:content (reverse buffer)
                                #:greater-than (make-character-token/after-read c)
                                #:closing-dashes (list dash)))]
           [else
            (comment (cons dash buffer))])]))

; https://html.spec.whatwg.org/multipage/parsing.html#comment-end-state
(: comment-end (-> character-token
                   character-token
                   (Listof character-token)
                   (Listof Token)))
(define (comment-end dash-1 dash-2 buffer)
  (define loc (current-location))
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-comment (get-current-location) #f)
               (make-comment #:content (reverse buffer)
                             #:closing-dashes (list dash-1 dash-2)))]
        [else
         (case c
           [(#\>)
            (read-char)
            (list (make-comment #:content (reverse buffer)
                                #:closing-dashes (list dash-1 dash-2)
                                #:greater-than (make-character-token/after-read c)))]
           [(#\!)
            (read-char)
            (comment-end-bang dash-1 dash-2 (make-character-token/after-read c) buffer)]
           [(#\-)
            (read-char)
            (comment-end dash-2
                         (make-character-token/after-read c)
                         (cons dash-1 buffer))]
           [else
            (comment (cons dash-2 (cons dash-1 buffer)))])]))

; https://html.spec.whatwg.org/multipage/parsing.html#comment-less-than-sign-state
(: comment-less-than-sign (-> character-token
                              (Listof character-token)
                              (Listof Token)))
(define (comment-less-than-sign less-than buffer)
  (define c (peek-char))
  (cond [(eof-object? c)
         (comment (cons less-than buffer))]
        [else
         (case c
           [(#\!)
            (read-char)
            (comment-less-than-sign-bang less-than
                                         (make-character-token/after-read c)
                                         buffer)]
           [(#\<)
            (read-char)
            (comment-less-than-sign (make-character-token/after-read c)
                                    (cons less-than buffer))]
           [else
            (comment (cons less-than buffer))])]))

; https://html.spec.whatwg.org/multipage/parsing.html#comment-less-than-sign-bang-state
(: comment-less-than-sign-bang (-> character-token
                                   character-token
                                   (Listof character-token)
                                   (Listof Token)))
(define (comment-less-than-sign-bang less-than bang buffer)
  (define c (peek-char))
  (cond [(and (char? c)
              (char=? c #\-))
         (read-char)
         (comment-less-than-sign-bang-dash less-than
                                           bang
                                           (make-character-token/after-read c)
                                           buffer)]
        [else
         (comment (cons bang (cons less-than buffer)))]))

; https://html.spec.whatwg.org/multipage/parsing.html#comment-less-than-sign-bang-dash-state
(: comment-less-than-sign-bang-dash (-> character-token
                                        character-token
                                        character-token
                                        (Listof character-token)
                                        (Listof Token)))
(define (comment-less-than-sign-bang-dash less-than bang dash buffer)
  (define c (peek-char))
  (cond [(and (char? c)
              (char=? c #\-))
         (read-char)
         (comment-less-than-sign-bang-dash-dash less-than
                                                bang
                                                dash
                                                (make-character-token/after-read c)
                                                buffer)]
        [else
         (comment-end-dash dash
                           (cons bang (cons less-than buffer)))]))

; https://html.spec.whatwg.org/multipage/parsing.html#comment-less-than-sign-bang-dash-dash-state
(: comment-less-than-sign-bang-dash-dash (-> character-token
                                             character-token
                                             character-token
                                             character-token
                                             (Listof character-token)
                                             (Listof Token)))
(define (comment-less-than-sign-bang-dash-dash less-than bang dash-1 dash-2 buffer)
  (define new-buffer (cons less-than (cons bang buffer)))
  (define c (peek-char))
  (cond [(or (eof-object? c)
             (char=? c #\>))
         (comment-end dash-1 dash-2 new-buffer)]
        [else
         (cons (nested-comment (get-current-location) #f)
               (comment-end dash-1 dash-2 new-buffer))]))

; https://html.spec.whatwg.org/multipage/parsing.html#comment-end-dash-state
(: comment-end-dash (-> character-token
                        (Listof character-token)
                        (Listof Token)))
(define (comment-end-dash dash buffer)
  (define c (peek-char))
  (cond [(eof-object? c)
         (list (eof-in-comment (get-current-location)
                               #f)
               (make-comment #:content (reverse buffer)
                             #:closing-dashes (list dash)))]
        [(char=? c #\-)
         (read-char)
         (comment-end dash (make-character-token/after-read c) buffer)]
        [else
         (comment (cons dash buffer))]))

; https://html.spec.whatwg.org/multipage/parsing.html#comment-end-bang-state
(: comment-end-bang (-> character-token
                        character-token
                        character-token
                        (Listof character-token)
                        (Listof Token)))
(define (comment-end-bang dash-1 dash-2 bang buffer)
  (define new-buffer (cons bang (cons dash-2 (cons dash-1 buffer))))
  (define c (peek-char))
  (define loc (get-current-location))
  (cond [(eof-object? c)
         (list (eof-in-comment (get-current-location) #f)
               (make-comment #:content (reverse buffer)
                             #:closing-dashes (list dash-1 dash-2))
               (drop-character bang))]
        [(char=? c #\-)
         (read-char)
         (comment-end-dash (make-character-token/after-read c)
                           new-buffer)]
        [(char=? c #\>)
         (read-char)
         (list (incorrectly-closed-comment loc bang)
               (make-comment #:content (reverse buffer)
                             #:closing-dashes (list dash-1 dash-2)
                             #:greater-than (make-character-token/after-read c)))]
        [else
         (comment new-buffer)]))
