#lang typed/racket/base/optional

(provide CDATA)

(require (file "types.rkt")
         (file "parameters.rkt")
         (file "stream.rkt")
         (file "tokens.rkt"))

; https://html.spec.whatwg.org/multipage/parsing.html#cdata-section-state
(: data/loc (-> (Listof Token)))
(define (data/loc)
  (define m (regexp-match-peek #px"^[^]]+"
                               (current-input-port)))
  (cond [(eq? #f m)
         (define c (peek-char))
         (cond [(eof-object? c)
                (list (eof-in-cdata (get-current-location) #f))]
               [(char=? c #\])
                (read-char)
                (define ct (make-character-token/after-read c))
                (section-bracket ct)]
               [else
                (read-char)
                (define ct (make-character-token/after-read c))
                (cons ct (data/loc))])]
        [else
         (define loc/before (get-current-location))
         (define bs (car m))
         (define s (bytes->string/utf-8 bs))
         (for ([c : Char (in-string s)])
           (read-char))
         (define loc/after (get-current-location))
         (cons (string-token loc/before
                             loc/after
                             s)
               (data/loc))]))

(define CDATA (tokenizer data/loc))

; https://html.spec.whatwg.org/multipage/parsing.html#cdata-section-bracket-state
(: section-bracket (-> character-token
                       (Listof Token)))
(define (section-bracket bracket)
  (define c (peek-char))
  (cond [(and (char? c)
              (char=? c #\]))
         (read-char)
         (section-end bracket (make-character-token/after-read c))]
        [else
         (cons bracket (data/loc))]))

; https://html.spec.whatwg.org/multipage/parsing.html#cdata-section-end-state
(: section-end (-> character-token
                   character-token
                   (Listof Token)))
(define (section-end bracket-1 bracket-2)
  (define c (peek-char))
  (cond [(eof-object? c)
         (append (list bracket-1 bracket-2)
                 (data/loc))]
        [else
         (case c
           [(#\])
            (read-char)
            (cons bracket-1
                  (section-end bracket-2 (make-character-token/after-read c)))]
           [(#\>)
            (read-char)
            (drop-characters (list bracket-1 bracket-2 (make-character-token/after-read c)))]
           [else
            (append (list bracket-1 bracket-2)
                    (data/loc))])]))
