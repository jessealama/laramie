#lang typed/racket/base

(provide previous-char-was-whitespace?
         current-tokens
         current-tag-name
         current-dropped-characters
         current-replaced-characters
         current-stream-errors
         raise-stream-error!
         replace-character!
         induce-character!
         current-location
         current-tokenizer-stack
         push-tokenizer!
         currently-in-attribute-value?
         current-ampersand
         current-pound
         current-X
         current-semicolon
         current-less-than
         current-greater-than
         current-attr-starter
         current-attr-name
         current-attr-separator
         current-attr-value-opener
         current-attr-value
         current-attributes
         current-tag-self-closing-char
         current-tag-ending?
         current-end-tag-slash
         current-doctype-opener
         current-bang
         current-comment-buffer
         current-opening-dashes
         current-closing-dashes
         current-token-buffer
         reset-token-buffer!
         include-dropped-chars?
         previous-tokens
         include-tokenizer-errors?
         current-misc
         add-to-misc-pile
         current-cdata-starter)

(require (file "types.rkt"))

#|

Input stream

|#

(: previous-char-was-whitespace? (Parameter Boolean))
(define previous-char-was-whitespace? (make-parameter #f))

(: current-stream-errors (Parameter (Listof stream-error)))
(define current-stream-errors (make-parameter (list)))

(: current-dropped-characters (Parameter (Listof (Pairof Char (Option location)))))
(define current-dropped-characters (make-parameter (list)))

(: current-replaced-characters (Parameter
                                (Listof (List (U Char (Listof Char))
                                              Char
                                              (Option location)))))
(define current-replaced-characters (make-parameter (list)))

(: current-induced-characters (Parameter
                               (Listof (Pairof Char (Option location)))))
(define current-induced-characters (make-parameter (list)))

(: raise-stream-error! (-> stream-error Void))
(define (raise-stream-error! e)
  (current-stream-errors (cons e (current-stream-errors))))

(: drop-character! (-> (U Char
                          (Pairof Char (Option location)))
                       Void))
(define (drop-character! c)
  (define dropped (cond [(char? c)
                         (cons c #f)]
                        [else c]))
  (current-dropped-characters (cons dropped (current-dropped-characters))))

(: replace-character! (-> Char
                          Void))
(define (replace-character! c)
  (current-replaced-characters (cons (list c #\ufffd (current-location))
                                     (current-replaced-characters))))

(: induce-character! (-> Char
                         (Option location)
                         Void))
(define (induce-character! c loc)
  (current-induced-characters (cons (cons c loc)
                                    (current-induced-characters))))

#|

Tokenization

|#

(: current-token-buffer (Parameter (Listof (U Token tokenizer))))
(define current-token-buffer (make-parameter (list)))

(define (reset-token-buffer!)
  (current-token-buffer (list)))

(: current-location (Parameter location))
(define current-location (make-parameter (location 1 0 0)))

(: current-tokens (Parameter (Option (Listof (U EOF Token)))))
(define current-tokens (make-parameter #f))

(: current-tag-name (Parameter (Option (Listof character-token))))
(define current-tag-name (make-parameter #f))

(: current-tokenizer-stack (Parameter (Listof tokenizer)))
(define current-tokenizer-stack (make-parameter (list)))

(: push-tokenizer! (-> tokenizer
                       Void))
(define (push-tokenizer! t)
  (current-tokenizer-stack (cons t (current-tokenizer-stack))))

(: currently-in-attribute-value? (Parameter Boolean))
(define currently-in-attribute-value? (make-parameter #f))

;; Character references

(: current-ampersand (Parameter (Option character-token)))
(define current-ampersand (make-parameter #f))

(: current-pound (Parameter (Option character-token)))
(define current-pound (make-parameter #f))

(: current-X (Parameter (Option character-token)))
(define current-X (make-parameter #f))

(: current-semicolon (Parameter (Option character-token)))
(define current-semicolon (make-parameter #f))

(: current-less-than (Parameter (Option character-token)))
(define current-less-than (make-parameter #f))

;;; Tags

(: current-attr-starter (Parameter (Option character-token)))
(define current-attr-starter (make-parameter #f))

(: current-attr-name (Parameter (Listof character-token)))
(define current-attr-name (make-parameter (list)))

(: current-attr-separator (Parameter (Option character-token)))
(define current-attr-separator (make-parameter #f))

(: current-attr-value-opener (Parameter (Option character-token)))
(define current-attr-value-opener (make-parameter #f))

(: current-attr-value (Parameter (Option
                                  (Listof (U character-token
                                             character-reference-token)))))
(define current-attr-value (make-parameter #f))

(: current-attributes (Parameter (Listof attribute-token)))
(define current-attributes (make-parameter (list)))

(: current-tag-self-closing-char (Parameter (Option character-token)))
(define current-tag-self-closing-char (make-parameter #f))

(: current-tag-ending? (Parameter Boolean))
(define current-tag-ending? (make-parameter #f))

(: current-end-tag-slash (Parameter (Option character-token)))
(define current-end-tag-slash (make-parameter #f))

(: current-doctype-opener (Parameter (Listof character-token)))
(define current-doctype-opener (make-parameter (list)))

(: current-bang (Parameter (Option character-token)))
(define current-bang (make-parameter #f))

(: current-comment-buffer (Parameter (Listof character-token)))
(define current-comment-buffer (make-parameter (list)))

(: current-opening-dashes (Parameter (Option (List character-token character-token))))
(define current-opening-dashes (make-parameter #f))

(: current-closing-dashes (Parameter (Option (List character-token character-token))))
(define current-closing-dashes (make-parameter #f))

(: current-greater-than (Parameter (Option character-token)))
(define current-greater-than (make-parameter #f))

(: include-dropped-chars? (Parameter Boolean))
(define include-dropped-chars? (make-parameter #f))

(: include-tokenizer-errors? (Parameter Boolean))
(define include-tokenizer-errors? (make-parameter #f))

(: previous-tokens (Parameter (Listof Token)))
(define previous-tokens (make-parameter (list)))

(: current-misc (Parameter (Listof character-token)))
(define current-misc (make-parameter (list)))

(: add-to-misc-pile (-> character-token
                        Void))
(define (add-to-misc-pile ct)
  (current-misc (cons ct (current-misc))))

(: current-cdata-starter (Parameter (Option (Listof character-token))))
(define current-cdata-starter (make-parameter #f))
