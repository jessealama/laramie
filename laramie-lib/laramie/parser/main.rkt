#lang typed/racket/base/optional

(provide parse
         attribute-ref
         first-location-of

         (struct-out element-node)
         (struct-out attribute-node)
         (struct-out document-node)
         (struct-out comment-node)
         (struct-out doctype-node)
         (struct-out parser-state))

(require (file "parser.rkt")
         (file "convert.rkt")
         (file "types.rkt")
         (file "dom.rkt"))
