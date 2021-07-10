#lang typed/racket/base

(provide (struct-out document-type)
         (struct-out prolog)
         (struct-out document)
         (struct-out comment)
         (struct-out element)
         (struct-out attribute))

(require (file "../tokenizer/types.rkt"))

(struct document-type
  ([start : location]
   [name : Symbol]
   [system : (Option String)]
   [public : (Option String)])
  #:transparent)

(struct prolog
  ([misc : (Listof comment)]
   [dtd : (Option document-type)])
  #:transparent)

(struct document
  ([prolog : prolog]
   [element : element]
   [misc : (Listof comment)])
  #:transparent)

(struct comment
  ([start : location]
   [content : String])
  #:transparent)

(struct element
  ([start : location]
   [local-name : Symbol]
   [prefix : (Option Symbol)]
   [attributes : (Listof attribute)]
   [content : (Listof (U element
                         comment
                         String))])
  #:transparent)

(struct attribute
  ([start : location]
   [local-name : Symbol]
   [prefix : (Option Symbol)]
   [value : (Option String)])
  #:transparent)
