#lang typed/racket/base

(provide (struct-out location)
         (struct-out source)
         (struct-out comment)
         (struct-out p-i)
         (struct-out external-dtd)
         (struct-out external-dtd/public)
         (struct-out external-dtd/system)
         (struct-out pcdata)
         (struct-out cdata)
         (struct-out entity)
         (struct-out document-type)
         (struct-out prolog)
         (struct-out document)
         (struct-out element)
         (struct-out attribute))

(require/typed xml
  [#:struct location
   ([line : (Option Exact-Nonnegative-Integer)]
    [char : (Option Exact-Nonnegative-Integer)]
    [offset : Exact-Nonnegative-Integer])]
  [#:struct source
   ([start : (Option (U Symbol location))]
    [stop : (Option (U Symbol location))])]
  [#:struct comment
   ([text : String])]
  [#:struct (p-i source)
   ([target-name : Symbol]
    [instruction : String])]
  [#:struct external-dtd
   ([system : String])]
  [#:struct (external-dtd/public external-dtd)
   ([public : String])]
  [#:struct (external-dtd/system external-dtd)
   ()]
  [#:struct (pcdata source)
   ([string : String])]
  [#:struct (cdata source)
   ([string : String])]
  [#:struct (entity source)
   ([text : (U Symbol Exact-Nonnegative-Integer)])] ; this is far too many integers
  [#:struct document-type
   ([name : Symbol]
    [external : external-dtd]
    [inlined : False])]
  [#:struct prolog
   ([misc : (Listof (U comment
                       p-i))]
    [dtd : (Option document-type)]
    [misc2 : (Listof (U comment
                        p-i))])]
  [#:struct document
   ([prolog : prolog]
    [element : element]
    [misc : (Listof (U comment p-i))])]
  [#:struct (element source)
   ([name : Symbol]
    [attributes : (Listof attribute)]
    [content : (Listof (U pcdata
                          element
                          entity
                          comment
                          cdata
                          p-i))])]
  [#:struct (attribute source)
   ([name : Symbol]
    [value : String])])
