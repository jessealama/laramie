#lang typed/racket/base

(provide html->xml

         (struct-out document-type)
         (struct-out prolog)
         (struct-out document)
         (struct-out comment)
         (struct-out element)
         (struct-out attribute))

(require (file "convert.rkt")
         (file "types.rkt"))
