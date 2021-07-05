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
         (multi-in "../tokenizer"
                   ("types.rkt"
                    "tokens.rkt")))

(module+ test
  (require typed/rackunit))

(: render-character-stretch (-> (Listof (U Char String))
                                String))
(define (render-character-stretch tokens)
  (cond [(null? tokens)
         ""]
        [(char? (car tokens))
         (define stretch (takef tokens char?))
         (define remainder (dropf tokens char?))
         (string-append (list->string stretch)
                        (render-character-stretch remainder))]
        [(string? (car tokens))
         (string-append (car tokens)
                        (render-character-stretch (cdr tokens)))]))

(: render-comment (-> comment-token comment))
(define (render-comment token)
  (error "render-comment: Not defined"))

(: render-attr-value (-> quoted-attr-value
                         (Option String)))
(define (render-attr-value value)
  (error "render-attr-value: Not defined yet"))

(: render-doctype (-> doctype-token
                      document-type))
(define (render-doctype token)
  (error "render-doctype: Not defined yet"))

(: render-attribute (-> attribute-node
                        (List Symbol String)))
(define (render-attribute a)
  (define local (attribute-node-local-name a))
  (define value (attribute-node-value a))
  (define full-name
    (match (attribute-node-prefix a)
      [(? string? p) (format "~a:~a" p local)]
      [_ local]))
  (cond [(eq? #f value)
         (list (string->symbol full-name)
               full-name)]
        [else
         (list (string->symbol full-name)
               value)]))

(: char-or-string? (-> (U doctype-node ElementNodeChild)
                       Boolean))
(define (char-or-string? x)
  (or (char? x)
      (string? x)))

(: find-character-stretch (-> (Listof (U doctype-node ElementNodeChild))
                              (Listof (U Char String))))
(define (find-character-stretch kids)
  (cond [(null? kids)
         (list)]
        [(or (char? (car kids))
             (string? (car kids)))
         (cons (car kids)
               (find-character-stretch (cdr kids)))]
        [else
         (list)]))

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

(: attribute-node->attribute (-> attribute-node
                                 attribute))
(define (attribute-node->attribute attr)
  (define token (attribute-node-token attr))
  (define value (attribute-token-value token))
  (attribute (span-start token)
             (character-tokens->string (attribute-token-name token))
             #f
             (cond [(eq? #f value) #f]
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

(module+ main
  (require racket/cmdline
           racket/pretty
           racket/port
           typed/racket/unsafe
           (file "parser.rkt"))
  (unsafe-require/typed
   net/http-easy
   [get (->* (Any)
             (#:timeouts Any)
             Any)]
   [response-body (-> Any Bytes)])
  (define url (command-line #:args (url)
                            url))
  (define r (response-body (get (format "~a" url))))
  (unless (eq? #f r)
    (pretty-print (->html (parser-state-document (parse r))))))
