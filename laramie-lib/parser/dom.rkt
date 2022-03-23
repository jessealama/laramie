#lang typed/racket/base

(provide current-document
         append-child!
         insert-character!
         insert-string!
         current-node
         append-child-to-document!
         push-open-element!
         pop-open-element!
         frameset-ok?
         current-open-elements
         set-current-node!
         set-current-open-elements!
         adjust-MathML-attributes
         adjust-SVG-attributes
         adjust-foreign-attributes
         set-element-namespace
         insert-foreign-element!
         reset-current-form-pointer!
         in-html-namespace?
         element-has-name?
         document-of
         descendants
         make-element
         make-comment-node
         make-doctype-node
         strings-in-elements
         document-element-of
         element-has-attribute?
         element-has-value-for-attribute?
         element-has-ci-value-for-attribute?
         find-attribute
         first-location-of
         element-node-name
         attribute-node-value
         doctype-node-name
         doctype-node-public
         doctype-node-system
         attribute-ref
         enumerate-elements
         html-namespace
         mathml-namespace
         svg-namespace
         xlink-namespace
         xml-namespace
         xmlns-namespace)

(require racket/list
         (file "types.rkt")
         (file "../tokenizer/types.rkt")
         (file "../tokenizer/tokens.rkt")
         (file "../private/todo.rkt")
         (file "parameters.rkt")
         (file "../tokenizer/parameters.rkt")
         ;(file "infrastructure.rkt")
         )

(module+ test
  (require typed/rackunit))

(define html-namespace "http://www.w3.org/1999/xhtml")
(define mathml-namespace "http://www.w3.org/1998/Math/MathML")
(define svg-namespace "http://www.w3.org/2000/svg")
(define xlink-namespace "http://www.w3.org/1999/xlink")
(define xml-namespace "http://www.w3.org/XML/1998/namespace")
(define xmlns-namespace "http://www.w3.org/2000/xmlns/")

(: current-open-elements (-> (Listof element-node)))
(define (current-open-elements)
  (parser-state-open-elements (current-parser-state)))

(: set-current-open-elements! (-> (Listof element-node) Void))
(define (set-current-open-elements! nodes)
  (current-parser-state
   (struct-copy parser-state
                (current-parser-state)
                [open-elements nodes])))

(: current-node (-> (Option element-node)))
(define (current-node)
  (findf element-node? (current-open-elements)))

(: set-current-node! (-> element-node Void))
(define (set-current-node! node)
  (define elems (current-open-elements))
  (define new-state (struct-copy parser-state
                                 (current-parser-state)
                                 [open-elements (cond [(null? elems)
                                                       (list node)]
                                                      [else
                                                       (cons node (cdr elems))])]))
  (current-parser-state new-state))

(: frameset-ok? (-> Boolean Void))
(define (frameset-ok? yes-or-no)
  (current-parser-state (struct-copy parser-state
                                     (current-parser-state)
                                     [frameset-ok? yes-or-no])))

(: append-child! (-> (U character-token
                        character-reference-token
                        string-token
                        element-node
                        comment-node
                        doctype-node)
                     Void))
(define (append-child! node)
  (define current (current-node))
  (cond
    [(element-node? current)
     (cond [(or (element-node? node)
                (comment-node? node)
                (character-token? node)
                (character-reference-token? node)
                (string-token? node))
            (set-element-node-children! current
                                        (append (element-node-children current)
                                                (list node)))
            (when (element-node? node)
              (current-tag-name (tag-token-name (element-node-token node)))
              (set-element-node-parent! node current)
              (push-open-element! node))])]
    [else
     (unless (or (character-token? node)
                 (character-reference-token? node)
                 (string-token? node))
       (append-child-to-document! node))
     (when (element-node? node)
       (current-tag-name (tag-token-name (element-node-token node)))
       (push-open-element! node))]))

(: find-document-node (-> (U element-node
                             comment-node)
                          (Option document-node)))
(define (find-document-node node)
  (cond [(element-node? node)
         (define parent (element-node-parent node))
         (cond [(eq? #f parent) #f]
               [(document-node? parent) parent]
               [else (find-document-node parent)])]
        [(comment-node? node)
         (define parent (comment-node-parent node))
         (cond [(eq? #f parent) #f]
               [(document-node? parent) parent]
               [else (find-document-node parent)])]))

(: set-parent! (-> (U element-node comment-node doctype-node)
                   (U element-node document-node)
                   Void))
(define (set-parent! child parent)
  (cond [(element-node? child)
         (set-element-node-parent! child parent)]
        [(comment-node? child)
         (set-comment-node-parent! child parent)]
        [else
         (set-doctype-node-parent! child parent)]))

(: append-child-to-document! (-> (U element-node
                                    comment-node
                                    doctype-node)
                                 Void))
(define (append-child-to-document! node)
  (define doc (current-document))
  (set-parent! node doc)
  (set-document-node-children! doc
                               (append (document-node-children doc)
                                       (list node))))

(: current-document (-> document-node))
(define (current-document)
  (parser-state-document (current-parser-state)))

(: push-open-element! (-> element-node Void))
(define (push-open-element! elem)
  (define s (current-parser-state))
  (current-parser-state
   (struct-copy parser-state
                s
                [open-elements (cons elem (parser-state-open-elements s))])))

(: pop-open-element! (-> Void))
(define (pop-open-element!)
  (define open (current-open-elements))
  (cond [(null? open)
         (log-error "attempt to pop element from empty stack of open elements")]
        [else
         (current-parser-state
          (struct-copy parser-state
                       (current-parser-state)
                       [open-elements (cdr open)]))]))

(: element-has-name? (-> element-node
                         (U String (Listof String))
                         Boolean))
(define (element-has-name? element name)
  (define n (element-node-name element))
  (cond [(string? name)
         (string=? name n)]
        [else
         (and (member n name string=?)
              #t)]))

(: descendants (-> (U parser-state
                      document-node
                      element-node
                      comment-node
                      doctype-node
                      (Listof (U element-node
                                 comment-node
                                 doctype-node)))
                   (Listof (U doctype-node
                              element-node
                              comment-node))))
(define (descendants thing)
  (cond [(parser-state? thing)
         (descendants (parser-state-document thing))]
        [(document-node? thing)
         (descendants (document-node-children thing))]
        [(element-node? thing)
         (cons thing
               (descendants (filter (lambda (x)
                                      (or (element-node? x)
                                          (comment-node? x)
                                          (doctype-node? x)))
                                    (element-node-children thing))))]
        [(or (comment-node? thing)
             (doctype-node? thing))
         (list thing)]
        [(null? thing)
         (list)]
        [else
         (append (descendants (car thing))
                 (descendants (cdr thing)))]))

(: update-attr-with-name (-> (Listof attribute-node)
                             String
                             String
                             (Listof attribute-node)))
(define (update-attr-with-name attrs old-name new-name)
  (cond [(null? attrs)
         (list)]
        [(string=? old-name (attribute-node-local-name (car attrs)))
         (define new-attr (struct-copy attribute-node
                                       (car attrs)
                                       [local-name new-name]))
         (cons new-attr
               (update-attr-with-name (cdr attrs) old-name new-name))]
        [else
         (cons (car attrs)
               (update-attr-with-name (cdr attrs) old-name new-name))]))

(: update-attrs (-> (Listof attribute-node)
                    (Listof (Pairof String String))
                    (Listof attribute-node)))
(define (update-attrs attributes keys)
  (cond [(null? keys)
         attributes]
        [else
         (define old-key (caar keys))
         (define new-key (cdar keys))
         (update-attrs (update-attr-with-name attributes old-key new-key)
                       (cdr keys))]))

(define mathml-attr-rewrites
  (list (cons "definitionurl" "definitionURL")))

(define svg-attr-rewrites
  (list (cons "attributename" "attributeName")
        (cons "attributetype" "attributeType")
        (cons "basefrequency" "baseFrequency")
        (cons "baseprofile" "baseProfile")
        (cons "calcmode" "calcMode")
        (cons "clippathunits" "clipPathUnits")
        (cons "diffuseconstant" "diffuseConstant")
        (cons "edgemode" "edgeMode")
        (cons "filterunits" "filterUnits")
        (cons "glyphref" "glyphRef")
        (cons "gradienttransform" "gradientTransform")
        (cons "gradientunits" "gradientUnits")
        (cons "kernelmatrix" "kernelMatrix")
        (cons "kernelunitlength" "kernelUnitLength")
        (cons "keypoints" "keyPoints")
        (cons "keysplines" "keySplines")
        (cons "keytimes" "keyTimes")
        (cons "lengthadjust" "lengthAdjust")
        (cons "limitingconeangle" "limitingConeAngle")
        (cons "markerheight" "markerHeight")
        (cons "markerunits" "markerUnits")
        (cons "markerwidth" "markerWidth")
        (cons "maskcontentunits" "maskContentUnits")
        (cons "maskunits" "maskUnits")
        (cons "numoctaves" "numOctaves")
        (cons "pathlength" "pathLength")
        (cons "patterncontentunits" "patternContentUnits")
        (cons "patterntransform" "patternTransform")
        (cons "patternunits" "patternUnits")
        (cons "pointsatx" "pointsAtX")
        (cons "pointsaty" "pointsAtY")
        (cons "pointsatz" "pointsAtZ")
        (cons "preservealpha" "preserveAlpha")
        (cons "preserveaspectratio" "preserveAspectRatio")
        (cons "primitiveunits" "primitiveUnits")
        (cons "refx" "refX")
        (cons "refy" "refY")
        (cons "repeatcount" "repeatCount")
        (cons "repeatdur" "repeatDur")
        (cons "requiredextensions" "requiredExtensions")
        (cons "requiredfeatures" "requiredFeatures")
        (cons "specularconstant" "specularConstant")
        (cons "specularexponent" "specularExponent")
        (cons "spreadmethod" "spreadMethod")
        (cons "startoffset" "startOffset")
        (cons "stddeviation" "stdDeviation")
        (cons "stitchtiles" "stitchTiles")
        (cons "surfacescale" "surfaceScale")
        (cons "systemlanguage" "systemLanguage")
        (cons "tablevalues" "tableValues")
        (cons "targetx" "targetX")
        (cons "targety" "targetY")
        (cons "textlength" "textLength")
        (cons "viewbox" "viewBox")
        (cons "viewtarget" "viewTarget")
        (cons "xchannelselector" "xChannelSelector")
        (cons "ychannelselector" "yChannelSelector")
        (cons "zoomandpan" "zoomAndPan")))

(: adjust-MathML-attributes (-> element-node element-node))
(define (adjust-MathML-attributes node)
  (define updated-attrs (update-attrs (element-node-attributes node)
                                      mathml-attr-rewrites))
  (struct-copy element-node
               node
               [attributes updated-attrs]))

(: adjust-SVG-attributes (-> element-node element-node))
(define (adjust-SVG-attributes node)
  (define updated-attrs (update-attrs (element-node-attributes node)
                                      svg-attr-rewrites))
  (struct-copy element-node
               node
               [attributes updated-attrs]))

(: foreign-rewrites (Immutable-HashTable String
                                         (List (Option String)
                                               String
                                               String)))
(define foreign-rewrites
  (hash "xlink:actuate" (list "xlink"   "actuate" xlink-namespace)
        "xlink:arcrole" (list "xlink"   "arcrole" xlink-namespace)
        "xlink:href"    (list "xlink"   "href"    xlink-namespace)
        "xlink:role"    (list "xlink"   "role"    xlink-namespace)
        "xlink:show"    (list "xlink"   "show"    xlink-namespace)
        "xlink:title"   (list "xlink"   "title"   xlink-namespace)
        "xlink:type"    (list "xlink"   "type"    xlink-namespace)
        "xml:lang"      (list "xml"     "lang"    xml-namespace)
        "xml:space"     (list "xml"     "space"   xml-namespace)
        "xmlns"         (list #f        "xmlns"   xmlns-namespace)
        "xmlns:xlink"   (list "xmlns"   "xlink"   xmlns-namespace)))

(: adjust-foreign-attributes (-> element-node element-node))
(define (adjust-foreign-attributes node)
  (: do-it (-> (Listof attribute-node)
               (Listof attribute-node)))
  (define (do-it attrs)
    (cond [(null? attrs)
           (list)]
          [(hash-has-key? foreign-rewrites (attribute-node-local-name (car attrs)))
           (define updated-attr-data (hash-ref foreign-rewrites
                                               (attribute-node-local-name (car attrs))))
           (define new-attribute (struct-copy attribute-node
                                              (car attrs)
                                              [prefix (car updated-attr-data)]
                                              [local-name (cadr updated-attr-data)]
                                              [namespace (caddr updated-attr-data)]))
           (cons new-attribute (do-it (cdr attrs)))]
          [else
           (cons (car attrs) (do-it (cdr attrs)))]))
  (struct-copy element-node
               node
               [attributes (do-it (element-node-attributes node))]))

(: set-element-namespace (-> element-node
                             String
                             element-node))
(define (set-element-namespace node new-namespace)
  (struct-copy element-node
               node
               [namespace new-namespace]))

(: insert-foreign-element! (-> element-node Void))
(define (insert-foreign-element! element)
  (append-child! element))

(: reset-current-form-pointer! (-> Void))
(define (reset-current-form-pointer!)
  (current-parser-state
   (struct-copy parser-state
                (current-parser-state)
                [form-pointer #f])))

(: in-html-namespace? (-> element-node Boolean))
(define (in-html-namespace? node)
  (string=? html-namespace (or (element-node-namespace node)
                               "")))

(: document-of (-> (U element-node
                      doctype-node
                      comment-node)
                   (Option document-node)))
(define (document-of node)
  (define parent (cond [(element-node? node)
                        (element-node-parent node)]
                       [(doctype-node? node)
                        (doctype-node-parent node)]
                       [else
                        (comment-node-parent node)]))
  (cond [(eq? #f parent) #f]
        [(document-node? parent) parent]
        [else (document-of parent)]))

(: current-node-or-document (-> (Option (U element-node document-node))))
(define (current-node-or-document)
  (define node (current-node))
  (define document (current-document))
  (cond [node node]
        [else document]))

(: make-comment-node (->* (#:token comment-token)
                          (#:parent (Option (U element-node document-node)))
                          comment-node))
(define (make-comment-node #:token token
                           #:parent [parent #f])
  (comment-node token
                (or parent (current-node-or-document))))

(: make-doctype-node (->* (#:token doctype-token)
                          (#:parent (Option (U element-node document-node)))
                          doctype-node))
(define (make-doctype-node #:token token
                           #:parent [parent #f])
  (doctype-node token
                (or parent (current-node-or-document))))

(: make-element (->* (#:token start-tag-token)
                     (#:parent (Option (U element-node document-node))
                      #:namespace String
                      #:induced? Boolean)
                     element-node))
(define (make-element #:token token
                      #:parent [parent #f]
                      #:namespace [namespace html-namespace]
                      #:induced? [induced? #f])
  (element-node token
                (or parent (current-node-or-document))
                (list)
                (list)
                namespace
                induced?))

(: insert-character! (-> (U character-token
                            character-reference-token)
                         Void))
(define (insert-character! char)
  (append-child! char))

(: insert-string! (-> string-token
                      Void))
(define (insert-string! str)
  (append-child! str))

(: fuse-character-stretch (-> (Listof (U character-token
                                         string-token))
                              String))
(define (fuse-character-stretch chars-or-strings)
  (cond [(null? chars-or-strings)
         ""]
        [(character-token? (car chars-or-strings))
         (define stretch (takef chars-or-strings character-token?))
         (define chars (enumerate-output-characters stretch))
         (string-append (list->string chars)
                        (fuse-character-stretch (drop chars-or-strings
                                                      (length stretch))))]
        [else
         (string-append (string-token-content (car chars-or-strings))
                        (fuse-character-stretch (cdr chars-or-strings)))]))

(: strings-in-element-node-children (-> (Listof ElementNodeChild)
                                        (Listof String)))
(define (strings-in-element-node-children children)
  (cond [(null? children)
         (list)]
        [(or (comment-node? (car children))
             (element-node? (car children)))
         (strings-in-element-node-children (cdr children))]
        [else
         (define stretch (takef children (lambda (x)
                                           (or (character-token? x)
                                               (string-token? x)))))
         (cond [(null? stretch)
                (list)]
               [else
                (cons (fuse-character-stretch stretch)
                      (strings-in-element-node-children (drop children
                                                              (length stretch))))])]))

(: strings-in-elements (-> (U parser-state
                              document-node
                              element-node
                              (Listof element-node))
                           (Listof String)))
(define (strings-in-elements thing)
  (cond [(parser-state? thing)
         (strings-in-elements (parser-state-document thing))]
        [(document-node? thing)
         (strings-in-elements (filter element-node? (document-node-children thing)))]
        [(element-node? thing)
         (define kids (element-node-children thing))
         (append (strings-in-element-node-children kids)
                 (strings-in-elements (filter element-node? kids)))]
        [(null? thing)
         (list)]
        [else
         (append (strings-in-elements (car thing))
                 (strings-in-elements (cdr thing)))]))

(: element-has-attribute? (-> element-node
                              (U String (Listof String))
                              Boolean))
(define (element-has-attribute? element attr-name)
  (: check? (-> attribute-node Boolean))
  (define (check? a)
    (define name (attribute-node-local-name a))
    (cond [(string? attr-name)
           (string-ci=? attr-name name)]
          [else
           (and (member name attr-name string-ci=?)
                #t)]))
  (and (findf check? (element-node-attributes element))
       #t))

(: element-has-value-for-attribute? (-> element-node
                                        String
                                        String
                                        Boolean))
(define (element-has-value-for-attribute? elem attr-name value)
  (define v (attribute-ref elem attr-name))
  (and (string? v)
       (string=? v value)))

(: element-has-ci-value-for-attribute? (-> element-node
                                           String
                                           String
                                           Boolean))
(define (element-has-ci-value-for-attribute? elem attr-name value)
  (define v (attribute-ref elem attr-name))
  (and (string? v)
       (string-ci=? v value)))

(: document-element-of (-> (U parser-state
                              document-node)
                           (Option element-node)))
(define (document-element-of thing)
  (cond [(parser-state? thing)
         (document-element-of (parser-state-document thing))]
        [else
         (define kids (filter element-node? (document-node-children thing)))
         (cond [(null? kids)
                #f]
               [(null? (cdr kids))
                (car kids)]
               [else
                (log-error "document has multiple root elements: after a ~a(n) element, there's a(n) ~a element"
                           (element-node-name (car kids))
                           (element-node-name (cadr kids)))
                (car kids)])]))

(: find-attribute (-> String
                      (U parser-state
                         document-node
                         element-node
                         (Listof element-node))
                      (Option element-node)))
(define (find-attribute attr-name thing)
  (cond [(parser-state? thing)
         (find-attribute attr-name (parser-state-document thing))]
        [(document-node? thing)
         (find-attribute attr-name (filter element-node? (document-node-children thing)))]
        [(element-node? thing)
         (cond [(element-has-attribute? thing attr-name)
                thing]
               [else #f])]
        [(null? thing) #f]
        [else
         (or (find-attribute attr-name (car thing))
             (find-attribute attr-name (cdr thing)))]))

(: first-location-of (-> (U element-node
                            comment-node
                            doctype-node
                            attribute-node
                            start-tag-token
                            comment-token
                            doctype-token
                            attribute-token)
                         (Option location)))
(define (first-location-of thing)
  (cond [(element-node? thing)
         (first-location-of (element-node-token thing))]
        [(comment-node? thing)
         (first-location-of (comment-node-token thing))]
        [(doctype-node? thing)
         (first-location-of (doctype-node-token thing))]
        [(attribute-node? thing)
         (first-location-of (attribute-node-token thing))]
        [(start-tag-token? thing)
         (span-start thing)]
        [(comment-token? thing)
         (span-start thing)]
        [(doctype-token? thing)
         (span-start thing)]
        [(attribute-token? thing)
         (span-start thing)]))

(: element-node-name (-> element-node String))
(define (element-node-name node)
  (list->string
   (enumerate-output-characters
    (tag-token-name (element-node-token node)))))

(: attribute-node-value (-> attribute-node
                            (Option String)))
(define (attribute-node-value node)
  (define v (attribute-token-value (attribute-node-token node)))
  (cond [(quoted-attr-value? v)
         (quoted-attr->string v)]
        [else #f]))

(: doctype-node-name (-> doctype-node String))
(define (doctype-node-name node)
  (list->string
   (enumerate-output-characters
    (doctype-token-name (doctype-node-token node)))))

(: doctype-node-public (-> doctype-node
                           (Option String)))
(define (doctype-node-public node)
  (list->string
   (enumerate-output-characters
    (doctype-token-public (doctype-node-token node)))))

(: doctype-node-system (-> doctype-node
                           (Option String)))
(define (doctype-node-system node)
  (list->string
   (enumerate-output-characters
    (doctype-token-system (doctype-node-token node)))))

(: attribute-ref (-> element-node
                     String
                     (Option String)))
(define (attribute-ref node attribute-name)
  (: find-it (-> (Listof attribute-node)
                 (Option String)))
  (define (find-it attrs)
    (cond [(null? attrs) #f]
          [(and (string=? attribute-name (attribute-node-local-name (car attrs)))
                (quoted-attr-value? (attribute-token-value (attribute-node-token (car attrs)))))
           (list->string
            (enumerate-output-characters
             (quoted-attr-value-content (attribute-token-value (attribute-node-token (car attrs))))))]
          [else (find-it (cdr attrs))]))
  (find-it (element-node-attributes node)))

(: enumerate-elements (-> (U parser-state
                          document-node
                          element-node
                          (Listof element-node))
                       (Listof element-node)))
(define (enumerate-elements thing)
  (cond [(parser-state? thing)
         (enumerate-elements (parser-state-document thing))]
        [(document-node? thing)
         (define e (document-element-of thing))
         (cond [(eq? #f e)
                (list)]
               [else
                (enumerate-elements e)])]
        [(element-node? thing)
         (cons thing
               (enumerate-elements (filter element-node? (element-node-children thing))))]
        [(null? thing)
         (list)]
        [else
         (append (enumerate-elements (car thing))
                 (enumerate-elements (cdr thing)))]))
