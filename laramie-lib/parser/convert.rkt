#lang typed/racket/base

(provide ->html)

(require racket/require
         racket/pretty
         racket/list
         racket/match
         racket/function
         typed/racket/unsafe
         (file "types.rkt")
         (file "dom.rkt")
         (prefix-in xml: (file "../private/xml.rkt"))
         (multi-in "../html"
                   ("types.rkt"))
         (multi-in "../tokenizer"
                   ("types.rkt"
                    "tokens.rkt")))

(module+ test
  (require typed/rackunit))

(: partition-prolog-children (->* ((Listof (U comment-node doctype-node)))
                                  ((Listof comment-node)
                                   (Option doctype-node)
                                   (Listof comment-node))
                                  (List (Listof comment-node)
                                        (Option doctype-node)
                                        (Listof comment-node))))
(define (partition-prolog-children children [before (list)] [dtd #f] [after (list)])
  (cond [(null? children)
         (list (reverse before)
               dtd
               (reverse after))]
        [(doctype-node? (car children))
         (partition-prolog-children (cdr children)
                                    before
                                    (cond [(doctype-node? dtd)
                                           dtd]
                                          [else
                                           (car children)])
                                    after)]
        [(doctype-node? dtd)
         (partition-prolog-children (cdr children)
                                    before
                                    dtd
                                    (cons (car children) after))]
        [else
         (partition-prolog-children (cdr children)
                                    (cons (car children) before)
                                    #f
                                    (list))]))

(: partition-document-children (->* ((Listof (U element-node doctype-node comment-node)))
                                    ((Listof (U doctype-node comment-node))
                                     (Option element-node)
                                     (Listof (U doctype-node comment-node)))
                                    (List (Listof (U doctype-node comment-node))
                                          (Option element-node)
                                          (Listof (U doctype-node comment-node)))))
(define (partition-document-children children [before (list)] [element #f] [after (list)])
  (cond [(null? children)
         (list (reverse before)
               element
               (reverse after))]
        [(element-node? (car children))
         (partition-document-children (cdr children)
                                      before
                                      (cond [(element-node? element)
                                             element]
                                            [else
                                             (car children)])
                                      after)]
        [(element-node? element)
         (partition-document-children (cdr children)
                                      before
                                      element
                                      (cons (car children) after))]
        [else
         (partition-document-children (cdr children)
                                      (cons (car children) before)
                                      #f
                                      (list))]))

(: comment-node->comment (-> comment-node
                             comment))
(define (comment-node->comment c)
  (define token (comment-node-token c))
  (define content (comment-token-content token))
  (comment (span-start (comment-token-less-than token))
           (list->string (enumerate-output-characters content))))

(: comment-node->xml-comment (-> comment-node
                                 xml:comment))
(define (comment-node->xml-comment c)
  (define token (comment-node-token c))
  (define content (comment-token-content token))
  (xml:comment (list->string (enumerate-output-characters content))))

(: doctype-node->dtd (-> doctype-node
                         document-type))
(define (doctype-node->dtd d)
  (define token (doctype-node-token d))
  (define start (span-start (doctype-token-less-than token)))
  (define name (character-tokens->string (doctype-token-name token)))
  (define system (doctype-token-system token))
  (define public (doctype-token-system token))
  (document-type start
                 (string->symbol name)
                 (cond [(eq? #f system) #f]
                       [else (character-tokens->string system)])
                 (cond [(eq? #f public) #f]
                       [else (character-tokens->string public)])))

(: doctype-node->xml-document-type (-> doctype-node
                                       xml:document-type))
(define (doctype-node->xml-document-type d)
  (define token (doctype-node-token d))
  (define start (span-start (doctype-token-less-than token)))
  (define name (character-tokens->string (doctype-token-name token)))
  (define system (doctype-token-system token))
  (define public (doctype-token-system token))
  (define system/string (cond [(eq? #f system) ""]
                              [else (character-tokens->string system)]))
  (define public/string (cond [(eq? #f public) ""]
                              [else (character-tokens->string public)]))
  (define dtd (cond [(string=? "" public/string)
                     (xml:external-dtd/system system/string)]
                    [else
                     (xml:external-dtd/public system/string
                                              public/string)]))
  (xml:document-type (string->symbol name)
                     dtd
                     #f))

(: attribute-node->attribute (-> attribute-node
                                 attribute))
(define (attribute-node->attribute attr)
  (define token (attribute-node-token attr))
  (define value (attribute-token-value token))
  (attribute (span-start token)
             (string->symbol
              (character-tokens->string
               (attribute-token-name token)))
             #f
             (cond [(eq? #f value) #f]
                   [else (quoted-attr->string value)])))

(: location->xml-location (-> location
                              xml:location))
(define (location->xml-location loc)
  (xml:location (location-line loc)
                (location-column loc)
                (location-position loc)))

(: attribute-node->xml-attribute (-> attribute-node
                                     xml:attribute))
(define (attribute-node->xml-attribute attr)
  (define token (attribute-node-token attr))
  (define value (attribute-token-value token))
  (define name (character-tokens->string (attribute-token-name token)))
  (xml:attribute (location->xml-location (span-start token))
                 (location->xml-location (span-start token)) ; obviously wrong
                 (string->symbol
                  (character-tokens->string
                   (attribute-token-name token)))
                 (cond [(eq? #f value) name]
                       [else (quoted-attr->string value)])))

(: attribute-node->xexpr-attribute (-> attribute-node
                                       (List Symbol String)))
(define (attribute-node->xexpr-attribute attr)
  (define token (attribute-node-token attr))
  (define value (attribute-token-value token))
  (define name (character-tokens->string (attribute-token-name token)))
  (list (string->symbol
         (character-tokens->string
          (attribute-token-name token)))
        (cond [(eq? #f value) name]
              [else (quoted-attr->string value)])))

(: element-node->element (-> element-node
                             element))
(define (element-node->element elem)
  (define token (element-node-token elem))
  (define start (span-start (tag-token-less-than token)))
  (define name (character-tokens->string (tag-token-name token)))
  (define attrs (map attribute-node->attribute (element-node-attributes elem)))
  (define content (element-children->html (element-node-children elem)))
  (element start
           (string->symbol name)
           'html
           attrs
           content))

(: element-node->xml-element (-> element-node
                                 xml:element))
(define (element-node->xml-element elem)
  (define token (element-node-token elem))
  (define start (span-start (tag-token-less-than token)))
  (define name (character-tokens->string (tag-token-name token)))
  (define attrs (map attribute-node->xml-attribute (element-node-attributes elem)))
  (define content (element-children->xml (element-node-children elem)))
  (xml:element (location->xml-location start)
               (location->xml-location start) ; obviously wrong
               (string->symbol name)
               attrs
               content))

(: element-children->html (-> (Listof (U element-node
                                         comment-node
                                         character-token
                                         character-reference-token
                                         string-token))
                              (Listof (U element
                                         comment
                                         String))))
(define (element-children->html kids)
  (cond [(null? kids)
         (list)]
        [(element-node? (car kids))
         (cons (element-node->element (car kids))
               (element-children->html (cdr kids)))]
        [(comment-node? (car kids))
         (cons (comment-node->comment (car kids))
               (element-children->html (cdr kids)))]
        [else
         (define stretch (takef kids (lambda (x)
                                       (or (character-token? x)
                                           (character-reference-token? x)
                                           (string-token? x)))))
         (cons (text-stretch->string stretch)
               (element-children->html (drop kids (length stretch))))]))

(: element-children->xml (-> (Listof (U element-node
                                        comment-node
                                        character-token
                                        character-reference-token
                                        string-token))
                             (Listof (U xml:element
                                        xml:comment
                                        xml:cdata))))
(define (element-children->xml kids)
  (cond [(null? kids)
         (list)]
        [(element-node? (car kids))
         (cons (element-node->xml-element (car kids))
               (element-children->xml (cdr kids)))]
        [(comment-node? (car kids))
         (cons (comment-node->xml-comment (car kids))
               (element-children->xml (cdr kids)))]
        [else
         (define kid (car kids))
         (define start (span-start kid))
         (define stretch (takef kids (lambda (x)
                                       (or (character-token? x)
                                           (character-reference-token? x)
                                           (string-token? x)))))
         (cons (xml:cdata (location->xml-location start)
                          (location->xml-location start) ; obviously wrong
                          (text-stretch->string stretch))
               (element-children->xml (drop kids (length stretch))))]))

(: html-element-children->xml-element-children
   (-> (Listof (U element
                  comment
                  String))
       (Listof (U xml:element
                  xml:comment
                  xml:cdata))))
(define (html-element-children->xml-element-children kids)
  (cond [(null? kids)
         (list)]
        [(element? (car kids))
         (cons (html-element->xml-element (car kids))
               (html-element-children->xml-element-children (cdr kids)))]
        [(comment? (car kids))
         (cons (html-comment->xml-comment (car kids))
               (html-element-children->xml-element-children (cdr kids)))]
        [else
         (define kid (car kids))
         (cons (xml:cdata (xml:location 1 0 0)
                          (xml:location 1 0 0)
                          kid)
               (html-element-children->xml-element-children (cdr kids)))]))

(: html-attribute->xml-attribute (-> attribute
                                     xml:attribute))
(define (html-attribute->xml-attribute attr)
  (define start (attribute-start attr))
  (define value (attribute-value attr))
  (define name (attribute-local-name attr))
  (xml:attribute (location->xml-location start)
                 (location->xml-location start)
                 name
                 (cond [(eq? #f value) (symbol->string name)]
                       [else value])))

(: ->html (-> document-node
              document))
(define (->html doc)
  (define doc-partitions (partition-document-children (document-node-children doc)))
  (define before-element (first doc-partitions))
  (define element (second doc-partitions))
  (define after-element (third doc-partitions))
  (cond [(eq? #f element)
         (error "Failed to find a root element")]
        [else
         (define prolog-partitions (partition-prolog-children before-element))
         (define before-doctype (first prolog-partitions))
         (define doctype (second prolog-partitions))
         (define after-doctype (third prolog-partitions))
         (define p (prolog (map comment-node->comment
                                (append before-doctype
                                        after-doctype))
                           (cond [(eq? #f doctype) #f]
                                 [else (doctype-node->dtd doctype)])))
         (document p
                   (element-node->element element)
                   (map comment-node->comment (filter comment-node? after-element)))]))

(: text-stretch->string (-> (Listof (U character-token
                                       character-reference-token
                                       string-token))
                            String))
(define (text-stretch->string stretch)
  (cond [(null? stretch)
         ""]
        [(character-token? (car stretch))
         (define c (character-token-content (car stretch)))
         (define s (cond [(char? c)
                          (format "~a" c)]
                         [(char? (cdr c))
                          (format "~a" (cdr c))]
                         [else
                          ""]))
         (string-append s (text-stretch->string (cdr stretch)))]
        [(character-reference-token? (car stretch))
         (define c (character-reference-token-result (car stretch)))
         (define s (cond [(char? c)
                          (format "~a" c)]
                         [else
                          (list->string c)]))
         (string-append s (text-stretch->string (cdr stretch)))]
        [else
         (string-append (string-token-content (car stretch))
                        (text-stretch->string (cdr stretch)))]))

(: html-comment->xml-comment (-> comment
                                 xml:comment))
(define (html-comment->xml-comment c)
  (xml:comment (comment-content c)))

(: html-document-type->xml-document-type (-> document-type
                                             xml:document-type))
(define (html-document-type->xml-document-type d)
  (define name (document-type-name d))
  (define system (document-type-system d))
  (define public (document-type-system d))
  (xml:document-type name
                     (cond [(and (eq? #f system)
                                 (eq? #f public))
                            (xml:external-dtd/system "")]
                           [(eq? #f system)
                            (xml:external-dtd/public public "")]
                           [else
                            (xml:external-dtd/public public system)])
                     #f))

(: html-prolog->xml-prolog (-> prolog
                               xml:prolog))
(define (html-prolog->xml-prolog pro)
  (define dt (prolog-dtd pro))
  (xml:prolog (map html-comment->xml-comment (prolog-misc pro))
              (cond [(eq? #f dt) #f]
                    [else (html-document-type->xml-document-type dt)])
              (list)))

(: html-element->xml-element (-> element
                                 xml:element))
(define (html-element->xml-element elem)
  (define start (element-start elem))
  (xml:element (location->xml-location start)
               (location->xml-location start)
               (element-local-name elem)
               (map html-attribute->xml-attribute (element-attributes elem))
               (html-element-children->xml-element-children (element-content elem))))
