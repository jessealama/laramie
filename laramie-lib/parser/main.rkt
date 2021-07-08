#lang typed/racket/base

(provide parse

         html->xml

         (struct-out document-type)
         (struct-out prolog)
         (struct-out document)
         (struct-out comment)
         (struct-out element)
         (struct-out attribute))

(require (file "parser.rkt")
         (file "convert.rkt")
         (file "types.rkt"))
