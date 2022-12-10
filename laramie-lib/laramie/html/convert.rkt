#lang typed/racket/base/optional

(provide html->xml)

(require racket/require
         racket/pretty
         racket/list
         racket/match
         racket/function
         typed/net/url
         typed/racket/unsafe
         (file "types.rkt")
         (prefix-in xml: (file "../private/xml.rkt"))
         (multi-in "../tokenizer"
                   ("types.rkt"
                    "tokens.rkt"))
         "../parser/parser.rkt")

(module+ test
  (require typed/rackunit))

(: location->xml-location (-> location
                              xml:location))
(define (location->xml-location loc)
  (xml:location (location-line loc)
                (location-column loc)
                (location-position loc)))

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

(: html->xml (-> (U document String Bytes Input-Port URL)
                 xml:document))
(define (html->xml doc)
  (cond [(or (string? doc)
             (bytes? doc)
             (input-port? doc)
             (url? doc))
         (html->xml (parse doc))]
        [else
         (xml:document (html-prolog->xml-prolog (document-prolog doc))
                       (html-element->xml-element (document-element doc))
                       (map html-comment->xml-comment (document-misc doc)))]))
