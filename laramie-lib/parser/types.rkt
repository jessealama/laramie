#lang typed/racket/base

(provide ElementNodeChild

         ; Parsing
         Node
         ParserResult
         InsertionMode

         (struct-out element-node)
         (struct-out attribute-node)
         (struct-out document-node)
         (struct-out comment-node)
         (struct-out doctype-node)
         (struct-out parser-state)

         (struct-out parser-error)
         (struct-out closing-tag-not-in-scope)
         (struct-out doctype-dropped)
         (struct-out unexpected-token)
         (struct-out element-induced))

(require (file "../tokenizer/types.rkt"))

(struct parser-error ())

(struct closing-tag-not-in-scope parser-error
  ([tag : end-tag-token]
   [scope : (Listof element-node)]))

(struct doctype-dropped parser-error
  ([token : doctype-token]))

(struct unexpected-token parser-error
  ([token : (U Token EOF)]
   [expected : (Option (Listof String))]
   [dropped? : Boolean]))

(struct element-induced parser-error
  ([name : String]))

(define-type Node
  (U element-node
     document-node
     comment-node
     doctype-node))

(define-type ParserResult (Listof (U Token Node)))

(define-type InsertionMode (U 'initial
                              'before-html
                              'before-head
                              'in-head
                              'in-head-noscript
                              'after-head
                              'in-body
                              'text
                              'in-table
                              'in-table-text
                              'in-caption
                              'in-column-group
                              'in-table-body
                              'in-row
                              'in-cell
                              'in-select
                              'in-select-in-table
                              'in-template
                              'after-body
                              'in-frameset
                              'after-frameset
                              'after-after-body
                              'after-after-frameset))

(struct parser-state
  ([open-elements : (Listof element-node)]
   [dropped : (Listof Token)]
   [errors : (Listof (U stream-error
                        tokenizer-error
                        parser-error))]
   [document : document-node]
   [quirky? : Boolean]
   [frameset-ok? : Boolean]
   [template-insertion-modes : (Listof InsertionMode)]
   [form-pointer : (Option element-node)]
   [head-pointer : (Option element-node)]
   [active-formatting-elements : (Listof (U element-node 'mark))]
   [foster-parenting-enabled? : Boolean]
   [insertion-mode : (Listof InsertionMode)])
  #:transparent)

(struct document-node
  ([children : (Listof (U element-node doctype-node comment-node))]
   [namespace : (Option String)])
  #:transparent #:mutable)

(struct comment-node
  ([token : comment-token]
   [parent : (Option (U element-node document-node))])
  #:transparent #:mutable)

(struct doctype-node
  ([token : doctype-token]
   [parent : (Option (U element-node document-node))])
  #:transparent #:mutable)

(struct attribute-node
  ([token : attribute-token]
   [prefix : (Option String)]
   [local-name : String]
   [namespace : (Option String)])
  #:transparent)

(define-type ElementNodeChild (U element-node
                                 comment-node
                                 character-token
                                 character-reference-token
                                 string-token))

(struct element-node
  ([token : start-tag-token]
   [parent : (Option (U element-node document-node))]
   [children : (Listof ElementNodeChild)]
   [attributes : (Listof attribute-node)]
   [namespace : (Option String)]
   [induced? : Boolean])
  #:mutable #:transparent)
