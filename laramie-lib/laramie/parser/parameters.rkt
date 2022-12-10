#lang typed/racket/base/optional

(provide current-parser-state
         raise-parse-error!
         turn-on-quirks!
         drop-token!
         scripting-enabled?
         current-context-node-for-fragment
         reset-parser-state!)

(require (file "types.rkt")
         (file "../tokenizer/parameters.rkt")
         (file "../tokenizer/types.rkt"))

(define (initial-parser-state)
  (parser-state (list)
                (list)
                (list)
                (document-node (list) #f)
                #f
                #t
                (list)
                #f
                #f
                (list)
                #f
                (list 'initial)))

(: current-parser-state (Parameter parser-state))
(define current-parser-state (make-parameter (initial-parser-state)))

(define (reset-parser-state!)
  (reset-token-buffer!)
  (current-parser-state (initial-parser-state)))

(: current-context-node-for-fragment (Parameter (Option element-node)))
(define current-context-node-for-fragment (make-parameter #f))

(: scripting-enabled? (Parameter Boolean))
(define scripting-enabled? (make-parameter #f))

(: drop-token! (-> Token
                   Void))
(define (drop-token! token)
  (define s (current-parser-state))
  (current-parser-state
   (struct-copy parser-state
                s
                [dropped (cons token (parser-state-dropped s))])))

(: raise-parse-error! (-> (U tokenizer-error
                             parser-error)
                          Void))
(define (raise-parse-error! err)
  (define s (current-parser-state))
  (current-parser-state
   (struct-copy parser-state
                s
                [errors (cons err (parser-state-errors s))])))

(: turn-on-quirks! (-> Void))
(define (turn-on-quirks!)
  (current-parser-state
   (struct-copy parser-state
                (current-parser-state)
                [quirky? #t])))
