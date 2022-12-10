#lang typed/racket/base/optional

(provide html->xml

         descendants

         (struct-out location)
         (struct-out document-type)
         (struct-out prolog)
         (struct-out document)
         (struct-out comment)
         (struct-out element)
         (struct-out attribute))

(require (file "convert.rkt")
         (file "types.rkt")
         (file "util.rkt"))
