#lang typed/racket/base

(provide parse)

(require racket/list
         racket/port
         racket/require
         (file "types.rkt")
         (file "dom.rkt")
         (file "parameters.rkt")
         (file "convert.rkt")
         (multi-in "../tokenizer"
                   ("parameters.rkt"
                    "stream.rkt"
                    "types.rkt"
                    "tokens.rkt"
                    "tokenize.rkt"
                    "main.rkt"))
         (multi-in "../private"
                   ("todo.rkt"))
         (multi-in "../html"
                   ("types.rkt")))

(: keep-popping (-> (U String (Listof String))
                    Void))
(define (keep-popping target-element-name)
  (define top (current-top-open-element))
  (when (element-node? top)
    (pop-open-element!)
    (unless (element-has-name? top target-element-name)
      (keep-popping target-element-name))))

(: template-node? (-> element-node Boolean))
(define (template-node? node)
  (element-has-name? node "template"))

(: template-in-open-elements? (-> (Option element-node)))
(define (template-in-open-elements?)
  (findf template-node? (current-open-elements)))

(: current-top-open-element (-> (Option element-node)))
(define (current-top-open-element)
  (define open (current-open-elements))
  (cond [(null? open) #f]
        [else (car open)]))

(: previous-open-element (-> (Option element-node)))
(define (previous-open-element)
  (define open (current-open-elements))
  (cond [(null? open) #f]
        [(null? (cdr open)) #f]
        [else (caddr open)]))

(: current-node-has-name? (-> (U String (Listof String)) Boolean))
(define (current-node-has-name? name)
  (define top (current-top-open-element))
  (cond [(eq? #f top) #f]
        [else
         (element-has-name? top name)]))

(: previous-node-has-name? (-> String Boolean))
(define (previous-node-has-name? name)
  (define prev (previous-open-element))
  (cond [(eq? #f prev) #f]
        [else
         (element-has-name? prev name)]))

(: attr-token->attribute (-> attribute-token attribute-node))
(define (attr-token->attribute a)
  (attribute-node a
                  #f
                  (list->string (enumerate-output-characters (attribute-token-name a)))
                  #f))

(: tag->element (-> start-tag-token
                    element-node))
(define (tag->element token)
  (element-node token
                (or (current-node)
                    (current-document))
                (list)
                (map attr-token->attribute
                     (tag-token-attrs token))
                html-namespace
                #f))

(: first-token-with-location (-> (Listof Token)
                                 (Option span)))
(define (first-token-with-location tokens)
  (define token (findf span? tokens))
  (cond [(span? token)
         (span (span-start token)
                    (span-stop token))]
        [else #f]))

(: induce-element (-> String element-node))
(define (induce-element name)
  (define previous (first-token-with-location (previous-tokens)))
  (define loc (cond [(location? previous) previous]
                    [else (location 1 0 0)]))
  (define token (start-tag-token loc
                                 loc
                                 (character-token loc (add-to-location loc #\<) #\<)
                                 (extend-characters-with-location loc (string->list name))
                                 (list)
                                 #f
                                 #f
                                 (list)))
  (make-element #:token token
                #:parent (or (current-node)
                             (current-document))
                #:namespace html-namespace
                #:induced? #t))

(: switch-mode (-> InsertionMode Void))
(define (switch-mode new-mode)
  (current-parser-state
   (struct-copy parser-state
                (current-parser-state)
                [insertion-mode (list new-mode)])))

(: push-insertion-mode (-> InsertionMode Void))
(define (push-insertion-mode mode)
  (switch-mode mode))

(: consume-first-character (-> string-token
                               Boolean
                               Void))
(define (consume-first-character token insert?)
  (define c (extract-first-character token))
  (when (character-token? c)
    (consume-token c)
    (when insert?
      (insert-character! c)))
  (set-next-token! (drop-first-character token)))

; https://html.spec.whatwg.org/multipage/parsing.html#the-initial-insertion-mode
(: initial (-> Void))
(define (initial [token #f])
  (define t (peek-token))
  (cond [(eq? #f t)
         (stop-parsing)]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(string-token? t)
         (define s (string-token-content t))
         (cond [(empty-string? t)
                (void (next-token))]
               [(string-starts-with-whitespace? t)
                (consume-first-character t #f)]
               [else
                (switch-mode 'before-html)])]
        [(or (character-token? t)
             (character-reference-token? t))
         (cond [(whitespace-token? t)
                (void (next-token))]
               [else
                (switch-mode 'before-html)])]
        [(comment-token? t)
         (next-token)
         (define node (make-comment-node #:token t))
         (append-child! node)]
        [(doctype-token? t)
         (next-token)
         (define node (make-doctype-node #:token t))
         (append-child! node)
         (when (doctype-token-is-quirky? t)
           (turn-on-quirks!))
         (switch-mode 'before-html)]
        [else
         (switch-mode 'before-html)]))

; https://html.spec.whatwg.org/multipage/parsing.html#the-before-html-insertion-mode
(: before-html (-> Void))
(define (before-html)
  (: fallback (-> Void))
  (define (fallback)
    (define node (induce-element "html"))
    (append-child! node)
    (switch-mode 'before-head))
  (define t (peek-token))
  (cond [(eq? #f t)
         (stop-parsing)]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(comment-token? t)
         (next-token)
         (define node (make-comment-node #:token t))
         (append-child! node)]
        [(string-token? t)
         (cond [(empty-string? t)
                (void (next-token))]
               [(string-starts-with-whitespace? t)
                (consume-first-character t #f)]
               [else
                (fallback)])]
        [(and (or (character-token? t)
                  (character-reference-token? t))
              (whitespace-token? t))
         (next-token)
         (drop-token! t)]
        [(and (start-tag-token? t)
              (tag-has-name? t "html"))
         (next-token)
         (define node (tag->element t))
         (append-child! node)
         (switch-mode 'before-head)]
        [(and (end-tag-token? t)
              (tag-has-name? t (list "head"
                                     "body"
                                     "html"
                                     "br")))
         (next-token)
         (drop-token! t)
         (fallback)]
        [(or (tag-token? t)
             (end-tag-token? t))
         (next-token)
         (drop-token! t)]
        [else
         (fallback)]))

(: merge-attributes (-> (Listof attribute-node)
                        (Listof attribute-token)
                        (Listof attribute-node)))
(define (merge-attributes dom-attrs token-attrs)
  (cond [(null? token-attrs)
         dom-attrs]
        [else
         (define a (car token-attrs))
         (define n (list->string (enumerate-output-characters (attribute-token-name a))))
         (cond [(findf (lambda ([x : attribute-node])
                         (define an (attribute-node-local-name x))
                         (and (string? an)
                              (string=? n an)))
                       dom-attrs)
                (merge-attributes dom-attrs (cdr token-attrs))]
               [else
                (define new-attribute (attribute-node a
                                                      #f
                                                      n
                                                      #f))
                (merge-attributes (cons new-attribute dom-attrs)
                                  (cdr token-attrs))])]))

(: merge-attributes-from-tag-token (-> element-node tag-token element-node))
(define (merge-attributes-from-tag-token node token)
  (struct-copy element-node
               node
               [attributes (merge-attributes (element-node-attributes node)
                                             (tag-token-attrs token))]))

(: current-head (-> (Option element-node)))
(define (current-head)
  (parser-state-head-pointer (current-parser-state)))

(: set-current-head! (-> element-node Void))
(define (set-current-head! elem)
  (define new-state (struct-copy parser-state
                                 (current-parser-state)
                                 [head-pointer elem]))
  (current-parser-state new-state))

(: currently-quirky? (-> Boolean))
(define (currently-quirky?)
  (parser-state-quirky? (current-parser-state)))

; https://html.spec.whatwg.org/multipage/parsing.html#the-before-head-insertion-mode
(: before-head (-> Void))
(define (before-head)
  (: fallback (-> Void))
  (define (fallback)
    (raise-parse-error! (element-induced "head"))
    (define node (induce-element "head"))
    (append-child! node)
    (set-current-head! node)
    (switch-mode 'in-head))
  (define t (peek-token))
  (cond [(eq? #f t)
         (stop-parsing)]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (string-token? t)
              (empty-string? t))
         (void (next-token))]
        [(and (string-token? t)
              (string-starts-with-whitespace? t))
         (consume-first-character t #f)]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(or (and (or (character-token? t)
                      (character-reference-token? t))
                  (whitespace-token? t))
             (comment-token? t))
         (next-token)
         (drop-token! t)]
        [(doctype-token? t)
         (next-token)
         (raise-parse-error! (doctype-dropped t))]
        [(and (start-tag-token? t)
              (tag-has-name? t "html"))
         (next-token)
         (in-body t)]
        [(and (start-tag-token? t)
              (tag-has-name? t "head"))
         (next-token)
         (define node (tag->element t))
         (append-child! node)
         (check-top-element "head")
         (set-current-head! node)
         (switch-mode 'in-head)]
        [(and (end-tag-token? t)
              (tag-has-name? t (list "head"
                                     "body"
                                     "html"
                                     "br")))
         (fallback)]
        [(tag-token? t)
         (next-token)
         (drop-token! t)]
        [else
         (fallback)]))

(: check-top-element (-> String Void))
(define (check-top-element name)
  (define top (current-top-open-element))
  (cond [(eq? #f top)
         (log-error "no top element at all!")]
        [(not (element-has-name? top name))
         (log-error "a non-~a elemement (~a) is the top of the stack of open elements"
                    name
                    (element-node-name top))]))

(: in-head:script (-> start-tag-token Void))
(define (in-head:script token)
  (define element (tag->element token))
  (next-token)
  ; quite a lot is being ignored here, since we're
  ; ignoring running scripts and all the
  ; compleixty that goes along with that
  (append-child! element)
  (push-tokenizer! SCRIPT)
  (switch-mode 'text))

(: in-head:template (-> start-tag-token Void))
(define (in-head:template token)
  (define element (tag->element token))
  (next-token)
  ; ignoring a lot here, since we don't handle templates as intended
  (append-child! element))


; https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inhead
(: in-head (->* ()
                ((Option Token))
                Void))
(define (in-head [token #f])
  (define t (or token (peek-token)))
  (cond [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (string-token? t)
              (empty-string? t))
         (void (next-token))]
        [(and (string-token? t)
              (string-starts-with-whitespace? t))
         (consume-first-character t #t)]
        [(and (or (character-token? t)
                  (character-reference-token? t))
              (whitespace-token? t))
         (next-token)
         (insert-character! t)]
        [(comment-token? t)
         (next-token)
         (define node (make-comment-node #:token t))
         (append-child! node)]
        [(doctype-token? t)
         (next-token)
         (raise-parse-error! (doctype-dropped t))]
        [(start-tag-token? t)
         (define element (tag->element t))
         (cond
           [(tag-name-equals? t "html")
            (in-body t)]
           [(tag-name-equals? t (list "base"
                                      "basefont"
                                      "bgsound"
                                      "link"))
            (next-token)
            (append-child! element)
            (pop-open-element!)]
           [(tag-name-equals? t "meta")
            (next-token)
            (append-child! element)
            (pop-open-element!)
            ; TODO:
            ;
            ; We're supposed to look at the charset attribute,
            ; and http-equiv but don't, since everything is
            ; treated as UTF-8 for us
            ]
           [(tag-name-equals? t "title")
            (define n (next-token))
            (append-child! element)
            (push-tokenizer! RCDATA)
            (switch-mode 'text)]
           [(tag-name-equals? t "noscript")
            (next-token)
            (append-child! element)
            [cond [(scripting-enabled?)
                   (push-tokenizer! RAWTEXT)]
                  [else
                   (append-child! element)
                   (switch-mode 'in-head-noscript)]]]
           [(tag-name-equals? t (list "noframes" "style"))
            (next-token)
            (append-child! element)
            (push-tokenizer! RAWTEXT)
            (switch-mode 'text)]
           [(tag-name-equals? t "script")
            (in-head:script t)]
           [(tag-name-equals? t "template")
            (in-head:template t)]
           [(tag-name-equals? t "head")
            (next-token)
            (drop-token! t)]
           [else
            (raise-parse-error! (unexpected-token t
                                                  (list "html"
                                                        "base"
                                                        "basefont"
                                                        "bgsound"
                                                        "link"
                                                        "meta"
                                                        "title"
                                                        "noscript"
                                                        "noframes" "style"
                                                        "script"
                                                        "template"
                                                        "head")
                                                  #f))
            (switch-mode 'after-head)])]
        [(end-tag-token? t)
         (check-top-element "head")
         (define open (current-open-elements))
         (pop-open-element!)
         (cond [(tag-name-equals? t "head")
                (next-token)
                (switch-mode 'after-head)]
               [(tag-name-equals? t (list "html" "body" "br"))
                (switch-mode 'after-head)]
               [else
                (next-token)
                (raise-parse-error! (closing-tag-not-in-scope t open))])]
        [else
         (check-top-element "head")
         (switch-mode 'after-head)]))

; https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inheadnoscript
(: in-head-noscript (-> Void))
(define (in-head-noscript)
  (: fallback (-> (Option
                   (U doctype-token
                      start-tag-token
                      end-tag-token
                      string-token
                      character-token
                      character-reference-token))
                  Void))
  (define (fallback t)
    (raise-parse-error! (unexpected-token (or t eof) #f #f))
    (check-top-element "noscript")
    (pop-open-element!)
    (check-top-element "head")
    (switch-mode 'in-head))
  (define t (peek-token))
  (cond [(eq? #f t)
         (fallback t)]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(doctype-token? t)
         (next-token)
         (raise-parse-error! (doctype-dropped t))]
        [(tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (cond [(start-tag-token? t)
                (cond [(tag-has-name? t "html")
                       (in-body t)]
                      [(tag-has-name? t (list "basefont" "bgsound" "link" "meta" "noframes" "style"))
                       (in-head t)]
                      [(member name (list "head" "noscript") string=?)
                       (next-token)
                       (raise-parse-error! (unexpected-token t
                                                             #f
                                                             #t))]
                      [else
                       (fallback t)])]
               [(end-tag-token? t)
                (cond [(string=? name "noscript")
                       (check-top-element "noscript")
                       (pop-open-element!)
                       (check-top-element "head")
                       (next-token)
                       (switch-mode 'in-head)]
                      [(string=? name "br")
                       (fallback t)]
                      [else
                       (next-token)
                       (raise-parse-error! (unexpected-token t
                                                             (list "noscript" "br")
                                                             #t))])])]
        [(string-token? t)
         (cond [(empty-string? t)
                (void (next-token))]
               [(string-starts-with-whitespace? t)
                (consume-first-character t #t)]
               [else
                (fallback t)])]
        [(or (character-token? t)
             (character-reference-token? t))
         (cond [(whitespace-token? t)
                (next-token)
                (insert-character! t)]
               [else
                (fallback t)])]
        [(comment-token? t)
         (next-token)
         (define node (make-comment-node #:token t))
         (append-child! node)]))

; https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-incdata
(: text (-> Void))
(define (text)
  (define t (peek-token))
  (cond [(eq? #f t)
         (raise-parse-error! (unexpected-token eof #f #f))
         (pop-open-element!)
         (stop-parsing)]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(string-token? t)
         (next-token)
         (insert-string! t)]
        [(or (character-token? t)
             (character-reference-token? t))
         (next-token)
         (insert-character! (cond [(null-character? t)
                                   (replace-character t)]
                                  [else t]))]
        [(and (end-tag-token? t)
              (tag-name-equals? t "script"))
         (next-token)
         ; LOTS being ignored here, since we don't support scripting
         (pop-open-element!)
         (stop-parsing)]
        [(end-tag-token? t)
         (next-token)
         (pop-open-element!)
         (stop-parsing)]
        [else
         (stop-parsing)]))

; https://html.spec.whatwg.org/multipage/parsing.html#the-after-head-insertion-mode
(: after-head (-> Void))
(define (after-head)
  (define (fallback)
    (raise-parse-error! (element-induced "body"))
    (define element (induce-element "body"))
    (append-child! element)
    (switch-mode 'in-body))
  (define t (peek-token))
  (cond [(eq? #f t)
         (fallback)]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(string-token? t)
         (cond [(empty-string? t)
                (void (next-token))]
               [(string-starts-with-whitespace? t)
                (consume-first-character t #t)]
               [else
                (fallback)])]
        [(or (character-token? t)
             (character-reference-token? t))
         (cond [(whitespace-token? t)
                (next-token)
                (insert-character! t)]
               [else
                (fallback)])]
        [(comment-token? t)
         (next-token)
         (define node (make-comment-node #:token t))
         (append-child! node)]
        [(doctype-token? t)
         (next-token)
         (raise-parse-error! (doctype-dropped t))]
        [(tag-token? t)
         (next-token)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (cond [(start-tag-token? t)
                (define element (tag->element t))
                (cond [(string=? name "html")
                       (in-body t)]
                      [(string=? name "body")
                       (append-child! element)
                       (frameset-ok? #f)
                       (switch-mode 'in-body)]
                      [(string=? name "frameset")
                       (append-child! element)
                       (switch-mode 'in-frameset)]
                      [(member name (list
                                     "base" "basefont" "bgsound"
                                     "link" "meta" "noframes"
                                     "script" "style" "template"
                                     "title")
                               string=?)
                       (raise-parse-error! (unexpected-token t #f #f))
                       (when (equal? (current-head) (current-top-open-element))
                         (pop-open-element!))
                       (in-head t)]
                      [(string=? name "head")
                       (raise-parse-error! (unexpected-token t #f #f))]
                      [else
                       (fallback)])]
               ; end tag token now:
               [(string=? name "template")
                (in-head t)]
               [(member name (list "body" "html" "br") string=?)
                (fallback)]
               [else
                (raise-parse-error! (unexpected-token t #f #f))])]
        [else
         (fallback)]))

; https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inframeset
(: in-frameset (-> Void))
(define (in-frameset)
  (define t (peek-token))
  (cond [(eq? #f t)
         (define n (current-node))
         (when (and (element-node? n)
                    (not (element-has-name? n "html")))
           (raise-parse-error! (unexpected-token eof (list "html") #f)))
         (stop-parsing)]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(string-token? t)
         (cond [(empty-string? t)
                (void (next-token))]
               [(string-starts-with-whitespace? t)
                (consume-first-character t #t)]
               [else
                (raise-parse-error! (unexpected-token (string-token (span-start t)
                                                                    (span-stop t)
                                                                    (substring (string-token-content t) 0 1))
                                                      #f
                                                      #f))
                (consume-first-character t #f)])]
        [(and (or (character-token? t)
                  (character-reference-token? t))
              (whitespace-token? t))
         (next-token)
         (insert-character! t)]
        [(comment-token? t)
         (next-token)
         (define node (make-comment-node #:token t))
         (append-child! node)]
        [(doctype-token? t)
         (next-token)
         (raise-parse-error! (doctype-dropped t))]
        [(tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (cond [(start-tag-token? t)
                (define element (tag->element t))
                (cond [(string=? name "html")
                       (next-token)
                       (raise-parse-error! (unexpected-token t #f #f))
                       (unless (template-in-open-elements?)
                         (define top (current-top-open-element))
                         (when (element-node? top)
                           (define enriched (merge-attributes-from-tag-token top t))
                           (set-current-node! enriched)))]
                      [(string=? name "frameset")
                       (next-token)
                       (append-child! element)]
                      [(string=? name "frame")
                       (next-token)
                       (append-child! element)]
                      [(string=? name "noframes")
                       (raise-parse-error! (unexpected-token t #f #f))
                       (when (equal? (current-head) (current-top-open-element))
                         (pop-open-element!))]
                      [else
                       (next-token)
                       (raise-parse-error! (unexpected-token t #f #f))])]
               ; end tag
               [(string=? name "frameset")
                (next-token)
                (define open (current-open-elements))
                (cond [(or (null? open)
                           (null? (cdr open)))
                       (raise-parse-error! (unexpected-token t #f #f))]
                      [else
                       (pop-open-element!)
                       ; we do not support fragment parsing,
                       ; so this is not exactly what the
                       ; spec says:
                       (switch-mode 'after-frameset)])]
               [else
                (next-token)
                (raise-parse-error! (unexpected-token t #f #f))])]
        [else
         (error (format "[in-frameset] Cannot handle token: ~a" t))]))

; https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-afterframeset
(: after-frameset (-> Void))
(define (after-frameset)
  (define t (peek-token))
  (cond [(eq? #f t)
         (stop-parsing)]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(and (string-token? t)
              (not (empty-string? t))
              (string-starts-with-whitespace? t))
         (consume-first-character t #t)]
        [(and (or (character-token? t)
                  (character-reference-token? t))
              (whitespace-token? t))
         (next-token)
         (insert-character! t)]
        [(comment-token? t)
         (next-token)
         (define node (make-comment-node #:token t))
         (append-child! node)]
        [(doctype-token? t)
         (next-token)
         (raise-parse-error! (doctype-dropped t))]
        [(tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (cond [(start-tag-token? t)
                (cond [(string=? name "html")
                       (next-token)
                       (raise-parse-error! (unexpected-token t #f #f))
                       (unless (template-in-open-elements?)
                         (define top (current-top-open-element))
                         (when (element-node? top)
                           (define enriched (merge-attributes-from-tag-token top t))
                           (set-current-node! enriched)))]
                      [(string=? name "noframes")
                       (next-token)
                       (push-tokenizer! RAWTEXT)]
                      [else
                       (next-token)
                       (raise-parse-error! (unexpected-token t #f #f))])]
               [(string=? name "html")
                (switch-mode 'after-after-frameset)]
               [else
                (next-token)
                (raise-parse-error! (unexpected-token t #f #f))])]
        [else
         (error (format "[after-frameset] Cannot handle token ~a" t))]))

(: reconstruct-active-formatting-elements (-> Void))
(define (reconstruct-active-formatting-elements)
  ; we don't support formatting elements (yet)
  (void))

; https://html.spec.whatwg.org/multipage/parsing.html#the-after-after-frameset-insertion-mode
(: after-after-frameset (-> Void))
(define (after-after-frameset)
  (define t (peek-token))
  (cond [(eq? #f t)
         (stop-parsing)]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(comment-token? t)
         (next-token)
         (define node (make-comment-node #:token t))
         (append-child! node)]
        [(doctype-token? t)
         (next-token)
         (raise-parse-error! (doctype-dropped t))]
        [(and (string-token? t)
              (not (empty-string? t))
              (string-starts-with-whitespace? t))
         (consume-first-character t #t)
         (reconstruct-active-formatting-elements)]
        [(and (or (character-token? t)
                  (character-reference-token? t))
              (whitespace-token? t))
         (next-token)
         (reconstruct-active-formatting-elements)
         (insert-character! t)]
        [(start-tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (cond [(string=? name "html")
                (next-token)
                (raise-parse-error! (unexpected-token t #f #f))
                (unless (template-in-open-elements?)
                  (define top (current-top-open-element))
                  (when (element-node? top)
                    (define enriched (merge-attributes-from-tag-token top t))
                    (set-current-node! enriched)))]
               [(string=? name "noframes")
                (next-token)
                (push-tokenizer! RAWTEXT)]
               [else
                (next-token)
                (raise-parse-error! (unexpected-token t #f #f))])]
        [else
         (error (format "[after-after-frameset] Cannot handle token ~a" t))]))

(: in-body:template-end-tag (-> end-tag-token
                                (-> Void)
                                Void))
(define (in-body:template-end-tag t fallback)
  (next-token)
  (cond [(template-in-open-elements?)
         (generate-all-implied-tags-thoroughly)
         (define top (current-top-open-element))
         (when (and (element-node? top)
                    (not (string=? "template" (element-node-name top))))
           (raise-parse-error! (unexpected-token t #f #f)))
         (keep-popping "template")
         (clear-formatting-elements!)
         (pop-template-insertion-modes!)
         (switch-mode (find-appropriate-insertion-mode))]
        [else
         (raise-parse-error! (unexpected-token t #f #f))
         (fallback)]))

; https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inbody
(: in-body (->* ()
                ((Option (U EOF Token))) Void))
(define (in-body [token #f])
  (define t (or token (peek-token)))
  (cond [(eq? #f t)
         (define tis (current-template-insertion-modes))
         (when (and (null? tis)
                    (error-node-at-eof?))
           (raise-parse-error! (unexpected-token eof #f #f)))]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(string-token? t)
         (cond [(empty-string? t)
                (void (next-token))]
               [else
                (consume-first-character t #t)
                (reconstruct-active-formatting-elements)
                (unless (string-starts-with-whitespace? t)
                  (frameset-ok? #f))])]
        [(or (character-token? t)
             (character-reference-token? t))
         (next-token)
         (cond [(null-character? t)
                (drop-token! t)]
               [else
                (reconstruct-active-formatting-elements)
                (insert-character! t)
                (unless (whitespace-token? t)
                  (frameset-ok? #f))])]
        [(comment-token? t)
         (next-token)
         (define node (make-comment-node #:token t))
         (append-child! node)]
        [(doctype-token? t)
         (next-token)
         (raise-parse-error! (doctype-dropped t))]
        [(start-tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (define element (tag->element t))
         (cond [(string=? name "html")
                (next-token)
                (raise-parse-error! (unexpected-token t #f #f))
                (unless (template-in-open-elements?)
                  (define top (current-top-open-element))
                  (when (element-node? top)
                    (define enriched (merge-attributes-from-tag-token top t))
                    (set-current-node! enriched)))]
               [(member name (list
                              "base" "basefont" "bgsound"
                              "link" "meta" "noframes"
                              "script" "style" "template"
                              "title")
                        string=?)
                (in-head t)]
               [(string=? name "body")
                (next-token)
                (define open (current-open-elements))
                (cond [(or (null? open)
                           (null? (cdr open))
                           (not (element-has-name? (caddr open) "body"))
                           (template-in-open-elements?))
                       (drop-token! t)]
                      [else
                       (frameset-ok? #f)
                       (define head-element (cadr open))
                       (when (element-node? head-element)
                         (define enriched (merge-attributes-from-tag-token head-element t))
                         (set-current-open-elements! (append (list (car open)
                                                                   enriched)
                                                             (cddr open))))])]
               [(string=? name "frameset")
                (next-token)
                (raise-parse-error! (unexpected-token t #f #f))
                (define open (current-open-elements))
                (cond [(or (null? open)
                           (null? (cdr open))
                           (not (element-has-name? (caddr open) "body")))
                       (drop-token! t)]
                      [(current-frameset-ok?)
                       (drop-token! t)]
                      [else
                       (set-current-open-elements! (list (last open)))
                       (append-child! element)
                       (switch-mode 'in-frameset)])]
               [(member name (list "address" "article" "aside" "blockquote" "center" "details" "dialog" "dir" "div" "dl" "fieldset" "figcaption" "figure" "footer" "header" "hgroup" "main" "menu" "nav" "ol" "p" "section" "summary" "ul") string=?)
                (next-token)
                (when (p-element-in-button-scope?)
                  (close-a-p-element t))
                (append-child! element)]
               [(member name (list "h1" "h2" "h3" "h4" "h5" "h6") string=?)
                (next-token)
                (when (p-element-in-button-scope?)
                  (close-a-p-element t))
                (define top (current-top-open-element))
                (when (and (element-node? top)
                           (element-has-name? top
                                              (list "h1" "h2" "h3" "h4" "h5" "h6")))
                  (raise-parse-error! (unexpected-token t #f #f))
                  (pop-open-element!))
                (append-child! element)]
               [(member name (list "pre" "listing") string=?)
                (next-token)
                (when (p-element-in-button-scope?)
                  (close-a-p-element t))
                (append-child! element)
                (define next (peek-token))
                (cond [(and (or (character-token? next)
                                (character-reference-token? next))
                            (newline-character? next))
                       (drop-token! next)
                       (next-token)]
                      [(and (string-token? next)
                            (not (empty-string? next))
                            (string-starts-with-null-character? next))
                       (consume-first-character next #f)])
                (frameset-ok? #f)]
               [(string=? name "form")
                (next-token)
                (cond [(and (current-form-pointer)
                            (not (template-in-open-elements?)))
                       (raise-parse-error! (unexpected-token t #f #f))]
                      [else
                       (when (p-element-in-button-scope?)
                         (close-a-p-element t))
                       (append-child! element)
                       (unless (template-in-open-elements?)
                         (set-form-pointer! element))])]
               [(string=? name "li")
                (next-token)
                (frameset-ok? #f)
                (: done (-> Void))
                (define (done)
                  (when (p-element-in-button-scope?)
                    (close-a-p-element t))
                  (append-child! element))
                (: loop (-> (Listof element-node)
                     Void))
                (define (loop nodes)
                  (cond [(null? nodes)
                         (done)]
                        [(element-has-name? (car nodes) "li")
                         (generate-implied-end-tags (list "li"))
                         (define top (current-top-open-element))
                         (when (and (element-node? top)
                                    (not (string=? "li" (element-node-name top))))
                           (raise-parse-error! (unexpected-token t #f #f)))
                         (keep-popping "li")
                         (done)]
                        [(and (special-node? (car nodes))
                              (not (element-has-name? (car nodes)
                                                      (list "address" "div" "p"))))
                         (done)]
                        [else
                         (loop (cdr nodes))]))
                (loop (current-open-elements))]
               [(member name (list "dd" "dt") string=?)
                (next-token)
                (frameset-ok? #f)
                (: done (-> Void))
                (define (done)
                  (when (p-element-in-button-scope?)
                    (close-a-p-element t))
                  (append-child! element))
                (: loop (-> (Listof element-node)
                            Void))
                (define (loop nodes)
                  (cond [(null? nodes)
                         (done)]
                        [(element-has-name? (car nodes) "dd")
                         (generate-implied-end-tags (list "dd"))
                         (define top (current-top-open-element))
                         (when (and (element-node? top)
                                    (not (string=? "dd" (element-node-name top))))
                           (raise-parse-error! (unexpected-token t #f #f)))
                         (keep-popping "dd")
                         (done)]
                        [(element-has-name? (car nodes) "dt")
                         (generate-implied-end-tags (list "dt"))
                         (define top (current-top-open-element))
                         (when (and (element-node? top)
                                    (not (string=? "dt" (element-node-name top))))
                           (raise-parse-error! (unexpected-token t #f #f)))
                         (keep-popping "dt")
                         (done)]
                        [(and (special-node? (car nodes))
                              (not (element-has-name? (car nodes)
                                                      (list "address" "div" "p"))))
                         (done)]
                        [else
                         (loop (cdr nodes))]))
                (loop (current-open-elements))]
               [(string=? name "plaintext")
                (next-token)
                (push-tokenizer! PLAINTEXT)]
               [(string=? name "button")
                (next-token)
                (when (element-in-scope? "button")
                  (raise-parse-error! (unexpected-token t #f #f))
                  (generate-implied-end-tags)
                  (keep-popping "button"))
                (reconstruct-active-formatting-elements)
                (append-child! element)
                (frameset-ok? #f)]
               [(string=? name "a")
                (next-token)
                ; adoption agency algorithm not used!
                (reconstruct-active-formatting-elements)
                (append-child! element)
                (push-active-formatting-element! element)]
               [(member name (list "b" "big" "code" "em" "font" "i" "s" "small" "strike" "strong" "tt" "u") string=?)
                (next-token)
                (reconstruct-active-formatting-elements)
                (append-child! element)
                (push-active-formatting-element! element)]
               [(string=? name "nobr")
                (next-token)
                (reconstruct-active-formatting-elements)
                (when (element-in-scope? "nobr")
                  (raise-parse-error! (unexpected-token t #f #f))
                  (adopt! t)
                  (reconstruct-active-formatting-elements))
                (append-child! element)
                (push-active-formatting-element! element)]
               [(member name (list "applet" "marquee" "object") string=?)
                (next-token)
                (reconstruct-active-formatting-elements)
                (append-child! element)
                (insert-marker!)
                (frameset-ok? #f)]
               [(string=? name "table")
                (next-token)
                (when (and (not (currently-quirky?))
                           (p-element-in-button-scope?))
                  (close-a-p-element t))
                (append-child! element)
                (frameset-ok? #f)
                (in-table)]
               [(member name (list "area" "br" "embed" "img" "keygen" "wbr") string=?)
                (next-token)
                (reconstruct-active-formatting-elements)
                (append-child! element)
                (pop-open-element!)
                (frameset-ok? #f)]
               [(string=? name "input")
                (next-token)
                (reconstruct-active-formatting-elements)
                (append-child! element)
                (pop-open-element!)
                (define type-attr (tag-token-attr-value t "type"))
                (cond [(string? type-attr)
                       (unless (regexp-match-exact? #px"(?i:hidden)" type-attr)
                         (frameset-ok? #f))]
                      [else
                       (frameset-ok? #f)])]
               [(member name (list "param" "source" "track") string=?)
                (next-token)
                (append-child! element)
                (pop-open-element!)]
               [(string=? name "hr")
                (next-token)
                (when (p-element-in-button-scope?)
                  (close-a-p-element t))
                (append-child! element)
                (pop-open-element!)
                (frameset-ok? #f)]
               [(string=? name "image")
                (next-token)
                (raise-parse-error! (element-induced "img"))
                (in-body (start-tag-token (span-start t)
                                          (span-stop t)
                                          (tag-token-less-than t)
                                          (extend-characters-with-location (add-to-location (span-start
                                                                                             (tag-token-less-than t)) #\<)
                                                                           (string->list "img"))
                                          (tag-token-attrs t)
                                          (tag-token-self-closing-char t)
                                          (tag-token-greater-than t)
                                          (tag-token-misc t)))]
               [(string=? name "textarea")
                (next-token)
                (append-child! element)
                (define next (peek-token))
                (cond [(and (or (character-token? next)
                                (character-reference-token? next))
                            (null-character? next))
                       (next-token)
                       (drop-token! next)]
                      [(and (string-token? next)
                            (string-starts-with-null-character? next))
                       (consume-first-character next #f)])
                (push-tokenizer! RCDATA)
                (push-insertion-mode 'in-body)
                (switch-mode 'text)]
               [(string=? name "xmp")
                (next-token)
                (when (p-element-in-button-scope?)
                  (close-a-p-element t))
                (reconstruct-active-formatting-elements)
                (frameset-ok? #f)
                (push-tokenizer! RAWTEXT)]
               [(string=? name "iframe")
                (next-token)
                (frameset-ok? #f)
                (push-tokenizer! RAWTEXT)]
               [(string=? name "noembed")
                (next-token)
                (push-tokenizer! RAWTEXT)]
               [(and (string=? name "noscript")
                     (scripting-enabled?))
                (next-token)
                (push-tokenizer! RAWTEXT)]
               [(string=? name "select")
                (next-token)
                (reconstruct-active-formatting-elements)
                (append-child! element)
                (frameset-ok? #f)
                ; doesn't seem to be quite right
                ; how do we get back to the in-body insertion mode?
                (switch-mode 'in-select)]
               [(member name (list "optgroup" "option") string=?)
                (next-token)
                (define top (current-top-open-element))
                (when (and (element-node? top)
                           (element-has-name? top "option"))
                  (pop-open-element!))
                (reconstruct-active-formatting-elements)
                (append-child! element)]
               [(member name (list "rb" "rtc") string=?)
                (next-token)
                (when (element-in-scope? "ruby")
                  (generate-implied-end-tags))
                (define top (current-top-open-element))
                (when (and (element-node? top)
                           (not (element-has-name? top "ruby")))
                  (raise-parse-error! (unexpected-token t #f #f)))
                (append-child! element)]
               [(member name (list "rp" "rt") string=?)
                (next-token)
                (when (element-in-scope? "ruby")
                  (generate-implied-end-tags (list "rtc")))
                (define top (current-top-open-element))
                (when (and (element-node? top)
                           (not (element-has-name? top "ruby"))
                           (not (element-has-name? top "rtc")))
                  (raise-parse-error! (unexpected-token t #f #f)))
                (append-child! element)]
               [(string=? name "math")
                (next-token)
                (reconstruct-active-formatting-elements)
                (define adjusted (adjust-MathML-attributes element))
                (define fixed (adjust-foreign-attributes adjusted))
                (define mathml-element (set-element-namespace fixed mathml-namespace))
                (insert-foreign-element! mathml-element)
                (when (tag-token-self-closing? t)
                  (pop-open-element!))]
               [(string=? name "svg")
                (next-token)
                (reconstruct-active-formatting-elements)
                (define adjusted (adjust-SVG-attributes element))
                (define fixed (adjust-foreign-attributes adjusted))
                (define svg-element (set-element-namespace fixed svg-namespace))
                (insert-foreign-element! svg-element)
                (when (tag-token-self-closing? t)
                  (pop-open-element!))]
               [(member name (list "caption" "col" "colgroup" "frame" "head" "tbody" "td" "tfoot" "th" "thead" "tr") string=?)
                (next-token)
                (raise-parse-error! (unexpected-token t #f #f))]
               [else
                (next-token)
                (reconstruct-active-formatting-elements)
                (append-child! element)])]
        [(end-tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (cond [(string=? name "template")
                (in-body:template-end-tag t in-body)]
               [(string=? name "body")
                (cond [(not (element-in-scope? "body"))
                       (next-token)
                       (raise-parse-error! (unexpected-token t #f #f))]
                      [else
                       (when (error-node-at-eof?)
                         (raise-parse-error! (unexpected-token t #f #f)))
                       (next-token)
                       (pop-open-element!)
                       (switch-mode 'after-body)])]
               [(string=? name "html")
                (cond [(not (element-in-scope? "body"))
                       (next-token)
                       (raise-parse-error! (unexpected-token t #f #f))]
                      [else
                       (when (error-node-at-eof?)
                         (raise-parse-error! (unexpected-token t #f #f)))
                       (switch-mode 'after-body)])]
               [(member name (list "address" "article" "aside" "blockquote" "center" "details" "dialog" "dir" "div" "dl" "fieldset" "figcaption" "figure" "footer" "header" "hgroup" "main" "menu" "nav" "ol" "pre" "section" "summary" "ul") string=?)
                (cond [(not (member name
                                    (map (lambda ([n : element-node])
                                           (element-node-name n))
                                         (filter in-html-namespace? (current-open-elements)))
                                    string=?))
                       (next-token)
                       (raise-parse-error! (unexpected-token t #f #f))]
                      [else
                       (generate-implied-end-tags)
                       (define top (current-top-open-element))
                       (when (and (element-node? top)
                                  (in-html-namespace? top)
                                  (not (string=? name (element-node-name top))))
                         (raise-parse-error! (unexpected-token t #f #f)))
                       (keep-popping name)])]
               [(string=? name "form")
                (next-token)
                (cond [(template-in-open-elements?)
                       (cond [(element-in-scope? "form")
                              (generate-implied-end-tags)
                              (unless (current-node-has-name? "form")
                                (raise-parse-error! (unexpected-token t #f #f)))
                              (keep-popping "form")]
                             [else
                              (raise-parse-error! (unexpected-token t #f #f))])]
                      [else
                       (define form (current-form-pointer))
                       (reset-current-form-pointer!)
                       (cond [(or (eq? #f form)
                                  (not (element-in-scope? "form")))
                              (raise-parse-error! (unexpected-token t #f #f))]
                             [else
                              (generate-implied-end-tags)
                              (unless (equal? form (current-top-open-element))
                                (raise-parse-error! (unexpected-token t #f #f)))
                              (keep-popping "form")])])]
               [(string=? name "p")
                (next-token)
                (unless (p-element-in-button-scope?)
                  (raise-parse-error! (element-induced "p"))
                  (define fake-p (induce-element "p"))
                  (append-child! fake-p))
                (close-a-p-element t)]
               [(string=? name "li")
                (next-token)
                (cond [(element-in-scope? "li")
                       (generate-implied-end-tags (list "li"))
                       (unless (current-node-has-name? "li")
                         (raise-parse-error! (unexpected-token t #f #f)))
                       (keep-popping "li")]
                      [else
                       (raise-parse-error! (unexpected-token t #f #f))])]
               [(member name (list "dd" "dt"))
                (next-token)
                (cond [(element-in-scope? name)
                       (generate-implied-end-tags (list "dd" "dt"))
                       (unless (current-node-has-name? name)
                         (raise-parse-error! (unexpected-token t #f #f)))
                       (keep-popping name)]
                      [else
                       (raise-parse-error! (unexpected-token t #f #f))])]
               [(member name (list "h1" "h2" "h3" "h4" "h5" "h6"))
                (next-token)
                (cond [(element-in-scope? name)
                       (generate-implied-end-tags)
                       (unless (current-node-has-name? name)
                         (raise-parse-error! (unexpected-token t #f #f)))
                       (keep-popping (list "h1" "h2" "h3" "h4" "h5" "h6"))]
                      [else
                       (raise-parse-error! (unexpected-token t #f #f))])]
               [(member name (list "a" "b" "big" "code" "em" "font" "i" "nobr" "s" "small" "strike" "strong" "tt" "u"))
                (next-token)
                (adopt! t)]
               [(member name (list "applet" "marquee" "object"))
                (cond [(element-in-scope? name)
                       (generate-implied-end-tags)
                       (unless (current-node-has-name? name)
                         (raise-parse-error! (unexpected-token t #f #f)))
                       (keep-popping name)
                       (clear-formatting-elements!)]
                      [else
                       (raise-parse-error! (unexpected-token t #f #f))])]
               [(string=? name "br")
                (next-token)
                (raise-parse-error! (unexpected-token t #f #t))
                ; we should probably note that we might be dropping end tag attributes
                (define new-tag/no-attrs (strip-start-tag-attributes
                                          (end-tag->start-tag t)))
                (in-body new-tag/no-attrs)]
               [else
                (: fallback (-> (Option element-node) Void))
                (define (fallback node)
                  (define top (current-top-open-element))
                  (when (and (element-node? node)
                             (element-node? top))
                    (cond [(and (in-html-namespace? node)
                                (element-has-name? top name))
                           (generate-implied-end-tags (list name))
                           (unless (equal? top node)
                             (raise-parse-error! (unexpected-token t #f #f)))
                           (keep-popping name)
                           (fallback #f)]
                          [(special-node? node)
                           (raise-parse-error! (unexpected-token t #f #f))
                           (fallback #f)]
                          [else
                           (pop-open-element!)
                           (fallback (current-top-open-element))])))
                (next-token)
                (fallback (current-top-open-element))])]))

(: error-node-at-eof? (-> Boolean))
(define (error-node-at-eof?)
  (: error-node? (-> element-node Boolean))
  (define (error-node? n)
    (not (member (element-node-name n)
                (list "dd"
                      "dt"
                      "li"
                      "optgroup"
                      "option"
                      "p"
                      "rb"
                      "rp"
                      "rt"
                      "rtc"
                      "tbody"
                      "tbody"
                      "td"
                      "tfoot"
                      "th"
                      "thead"
                      "tr"
                      "body"
                      "html")
                string=?)))
  (element-node? (findf error-node? (current-open-elements))))

(: in-select (->* ()
                  ((Option (U EOF Token)))
                  Void))
(define (in-select [token #f])
  (define t (or token (peek-token)))
  (cond [(eq? #f t)
         (define tis (current-template-insertion-modes))
         (when (and (null? tis)
                    (error-node-at-eof?))
           (raise-parse-error! (unexpected-token eof #f #f)))
         (stop-parsing)]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(string-token? t)
         (cond [(empty-string? t)
                (void (next-token))]
               [else
                (next-token)
                (todo/log/compile-time (void)) ; record null characters as errors!
                (insert-string! t)])]
        [(or (character-token? t)
             (character-reference-token? t))
         (next-token)
         (cond [(null-character? t)
                (raise-parse-error! (unexpected-token t #f #t))]
               [else
                (insert-character! t)])]
        [(comment-token? t)
         (next-token)
         (define node (make-comment-node #:token t))
         (append-child! node)]
        [(doctype-token? t)
         (next-token)
         (raise-parse-error! (doctype-dropped t))]
        [(start-tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (define element (tag->element t))
         (cond [(string=? name "html")
                (next-token)
                (raise-parse-error! (unexpected-token t #f #t))
                (unless (template-in-open-elements?)
                  (define top (current-top-open-element))
                  (when (element-node? top)
                    (define enriched (merge-attributes-from-tag-token top t))
                    (set-current-node! enriched)))]
               [(string=? name "option")
                (next-token)
                (when (current-node-has-name? "option")
                  (pop-open-element!))
                (append-child! element)]
               [(string=? name "option")
                (next-token)
                (when (current-node-has-name? "optgroup")
                  (pop-open-element!))
                (when (current-node-has-name? "option")
                  (pop-open-element!))
                (append-child! element)]
               [(string=? name "select")
                (next-token)
                (raise-parse-error! (unexpected-token t #f #t))
                (when (or (not (currently-parsing-fragment?))
                          (select-element-in-select-scope?))
                  (keep-popping "select")
                  (switch-mode (find-appropriate-insertion-mode)))]
               [(member name (list "input" "keygen" "textarea") string=?)
                ; do not go to the next token
                (raise-parse-error! (unexpected-token t #f #f))
                (when (or (not (currently-parsing-fragment?))
                          (select-element-in-select-scope?))
                  (keep-popping "select")
                  (switch-mode (find-appropriate-insertion-mode)))]
               [(string=? name "script")
                (in-head t)
                (switch-mode 'in-select)]
               [(string=? name "template")
                (in-head t)
                (switch-mode 'in-select)]
               [else
                (next-token)
                (raise-parse-error! (unexpected-token t #f #t))])]
        [(end-tag-token? t)
         (next-token)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (cond [(string=? name "optgroup")
                (next-token)
                (when (and (current-node-has-name? "option")
                           (previous-node-has-name? "optgroup"))
                  (pop-open-element!))
                (cond [(current-node-has-name? "optgroup")
                       (pop-open-element!)]
                      [else
                       (raise-parse-error! (unexpected-token t #f #t))])]
               [(string=? name "option")
                (next-token)
                (cond [(current-node-has-name? "option")
                       (pop-open-element!)]
                      [else
                       (raise-parse-error! (unexpected-token t #f #t))])]
               [(string=? name "select")
                (next-token)
                (cond [(and (currently-parsing-fragment?)
                            (not (select-element-in-select-scope?)))
                       (raise-parse-error! (unexpected-token t #f #t))]
                      [else
                       (keep-popping "select")
                       (switch-mode (find-appropriate-insertion-mode))])]
               [(string=? name "template")
                (in-body:template-end-tag t in-select)]
               [else
                (next-token)
                (raise-parse-error! (unexpected-token t #f #t))])]
        [else
         (error (format "[in-select] Cannot handle token: ~a" t))]))

(: after-body (-> Void))
(define (after-body)
  (define t (peek-token))
  (cond [(eq? #f t)
         (stop-parsing)]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(string-token? t)
         (cond [(empty-string? t)
                (void (next-token))]
               [(string-starts-with-whitespace? t)
                (consume-first-character t #t)
                (reconstruct-active-formatting-elements)]
               [else
                (raise-parse-error! (unexpected-token (string-token (span-start t)
                                                                    (span-stop t)
                                                                    (substring (string-token-content t) 0 1))
                                                      #f
                                                      #f))
                (switch-mode 'in-body)])]
        [(or (character-token? t)
             (character-reference-token? t))
         (cond [(whitespace-token? t)
                (next-token)
                (reconstruct-active-formatting-elements)
                (insert-character! t)]
               [else
                (raise-parse-error! (unexpected-token t #f #f))
                (switch-mode 'in-body)])]
        [(comment-token? t)
         (next-token)
         (define node (make-comment-node #:token t))
         (append-child! node)]
        [(doctype-token? t)
         (next-token)
         (raise-parse-error! (doctype-dropped t))]
        [(start-tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (cond [(string=? "html" name)
                (next-token)
                (raise-parse-error! (unexpected-token t #f #t))
                (unless (template-in-open-elements?)
                  (define top (current-top-open-element))
                  (when (element-node? top)
                    (define enriched (merge-attributes-from-tag-token top t))
                    (set-current-node! enriched)))]
               [else
                (raise-parse-error! (unexpected-token t #f #f))
                (switch-mode 'in-body)])]
        [(end-tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (cond [(string=? name "html")
                (next-token)
                (cond [(currently-parsing-fragment?)
                       (raise-parse-error! (unexpected-token t #f #t))]
                      [else
                       (pop-open-element!)
                       (switch-mode 'after-after-body)])]
               [else
                (raise-parse-error! (unexpected-token t #f #f))
                (switch-mode 'in-body)])]
        [else
         (error (format "[after-body] Cannot handle token: ~a" t))]))

(: after-after-body (-> Void))
(define (after-after-body)
  (define t (peek-token))
  (cond [(eq? #f t)
         (stop-parsing)]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(comment-token? t)
         (next-token)
         (define node (make-comment-node #:token t))
         (append-child! node)]
        [(doctype-token? t)
         (next-token)
         (raise-parse-error! (doctype-dropped t))]
        [(string-token? t)
         (cond [(empty-string? t)
                (void (next-token))]
               [(string-starts-with-whitespace? t)
                (consume-first-character t #t)
                (reconstruct-active-formatting-elements)]
               [else
                (raise-parse-error! (unexpected-token (string-token (span-start t)
                                                                    (span-stop t)
                                                                    (substring (string-token-content t) 0 1))
                                                      #f
                                                      #f))
                (switch-mode 'in-body)])]
        [(or (character-token? t)
             (character-reference-token? t))
         (cond [(whitespace-token? t)
                (next-token)
                (reconstruct-active-formatting-elements)
                (insert-character! t)]
               [else
                (raise-parse-error! (unexpected-token t #f #f))
                (switch-mode 'in-body)])]
        [(start-tag-token? t)
         (cond [(string=? "html" (list->string (enumerate-output-characters (tag-token-name t))))
                (next-token)
                (raise-parse-error! (unexpected-token t #f #t))
                (unless (template-in-open-elements?)
                  (define top (current-top-open-element))
                  (when (element-node? top)
                    (define enriched (merge-attributes-from-tag-token top t))
                    (set-current-node! enriched)))]
               [else
                (raise-parse-error! (unexpected-token t #f #f))
                (switch-mode 'in-body)])]
        [else
         (error (format "[after-after-body] Cannot handle token ~a" t))]))

(: current-template-insertion-modes (-> (Listof InsertionMode)))
(define (current-template-insertion-modes)
  (parser-state-template-insertion-modes (current-parser-state)))

(: current-template-insertion-mode (-> (Option InsertionMode)))
(define (current-template-insertion-mode)
  (define modes (current-template-insertion-modes))
  (cond [(null? modes) #f]
        [else (car modes)]))

(: current-frameset-ok? (-> Boolean))
(define (current-frameset-ok?)
  (parser-state-frameset-ok? (current-parser-state)))

(: current-form-pointer (-> (Option element-node)))
(define (current-form-pointer)
  (parser-state-form-pointer (current-parser-state)))

(: p-element-in-button-scope? (-> Boolean))
(define (p-element-in-button-scope?)
  (element-in-scope? "p" (hash html-namespace (list "button"))))

(: select-element-in-select-scope? (-> Boolean))
(define (select-element-in-select-scope?)
  (element-in-scope? "select" (hash html-namespace (list "select"))))

(: tr-element-in-table-scope? (-> Boolean))
(define (tr-element-in-table-scope?)
  (element-in-scope? "tr" (hash html-namespace (list "table"))))

(define (caption-element-in-table-scope?)
  (element-in-scope? "caption" (hash html-namespace (list "table"))))

(: table-element-in-table-scope? (-> Boolean))
(define (table-element-in-table-scope?)
  (element-in-scope? "table" (hash html-namespace (list "table"))))

(: element-in-scope? (->* ((U String (Listof String)))
                          ((Immutable-HashTable String (Listof String)))
                          Boolean))
(define (element-in-scope? node-name [additional-scopes (hash)])
  (: default-scopes-by-namespace (Immutable-HashTable String (Listof String)))
  (define default-scopes-by-namespace
    (hash html-namespace (list "applet" "caption" "html" "table" "td" "th" "marquee" "object" "template")
          mathml-namespace (list "mi" "mo" "mn" "ms" "mtext" "annotation-xml")
          svg-namespace (list "foreignObject" "desc" "title")))
  (: do-it (-> (Listof element-node) Boolean))
  (define (do-it nodes)
    (cond [(null? nodes) #f]
          [else
           (define node (car nodes))
           (define name (element-node-name node))
           (define namespace (element-node-namespace node))
           (cond [(and (string? node-name)
                       (string=? name node-name))
                  #t]
                 [(and (list? node-name)
                       (member name node-name string=?))
                  #t]
                 [(and (string? namespace)
                       (hash-has-key? additional-scopes namespace))
                  (define names (hash-ref additional-scopes namespace (lambda () (list))))
                  (cond [(and (list? names)
                              (member name names (lambda (a b) (and (string? a)
                                                                    (string? b)
                                                                    (string=? a b)))))
                         #f]
                        [(hash-has-key? default-scopes-by-namespace namespace)
                         (: names (Listof String))
                         (define names (hash-ref default-scopes-by-namespace namespace))
                         (cond [(member name names string=?) #f]
                               [else (do-it (cdr nodes))])]
                        [else (do-it (cdr nodes))])]
                 [(and (string? namespace)
                       (hash-has-key? default-scopes-by-namespace namespace))
                  (: names (Listof String))
                  (define names (hash-ref default-scopes-by-namespace namespace))
                  (cond [(member name names string=?) #f]
                        [else (do-it (cdr nodes))])]
                 [else (do-it (cdr nodes))])]))
  (do-it (current-open-elements)))

(: tag-has-name? (-> tag-token
                     (U String
                        (Listof String))
                     Boolean))
(define (tag-has-name? t name)
  (define n (list->string (enumerate-output-characters (tag-token-name t))))
  (cond [(string? name)
         (string=? name n)]
        [else
         (and (member n name string=?)
              #t)]))

(: close-a-p-element (-> (U start-tag-token end-tag-token)
                         Void))
(define (close-a-p-element token)
  (generate-implied-end-tags (list "p"))
  (raise-parse-error! (unexpected-token token (list "p") #f))
  (keep-popping "p"))

(: generate-implied-end-tags (->* ()
                                  ((Listof String))
                                  Void))
(define (generate-implied-end-tags [except (list)])
  (define top (current-top-open-element))
  (when (element-node? top)
    (define n (element-node-name top))
    (when (and (member n (list "dd" "dt" "li" "optgroup" "option" "p" "rb" "rp" "rt" "rtc") string=?)
               (not (member n except string=?)))
      (pop-open-element!)
      (generate-implied-end-tags except))))

(: special-node? (-> element-node Boolean))
(define (special-node? node)
  (define namespace (element-node-namespace node))
  (and (string? namespace)
       (cond [(string=? namespace html-namespace)
              (element-has-name? node
                                 (list "address" "applet" "area" "article" "aside" "base" "basefont" "bgsound"
                                       "blockquote" "body" "br" "button" "caption" "center" "col" "colgroup"
                                       "dd" "details" "dir" "div" "dl" "dt" "embed" "fieldset" "figcaption"
                                       "figure" "footer" "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5"
                                       "h6" "head" "header" "hgroup" "hr" "html" "iframe" "img" "input" "keygen"
                                       "li" "link" "listing" "main" "marquee" "menu" "meta" "nav" "noembed"
                                       "noframes" "noscript" "object" "ol" "p" "param" "plaintext" "pre" "script"
                                       "section" "select" "source" "style" "summary" "table" "tbody" "td" "template"
                                       "textarea" "tfoot" "th" "thead" "title" "tr" "track" "ul" "wbr" "xmp"))]
             [(string=? namespace mathml-namespace)
              (element-has-name? node (list "mi" "mo" "mn" "ms" "mtext" "annotation-xml"))]
             [(string=? namespace svg-namespace)
              (element-has-name? node (list "foreignObject" "desc" "title"))]
             [else #f])
       #t))

(: push-active-formatting-element! (-> element-node Void))
(define (push-active-formatting-element! element)
  (define s (current-parser-state))
  (define new-elements (cons element (parser-state-active-formatting-elements s)))
  (current-parser-state
   (struct-copy parser-state
                s
                [active-formatting-elements new-elements])))

(: pop-active-formatting-element! (-> (Option (U 'mark element-node))))
(define (pop-active-formatting-element!)
  (define state (current-parser-state))
  (define active (parser-state-active-formatting-elements state))
  (cond [(null? active)
         #f]
        [else
         (define next-state
           (struct-copy parser-state
                        state
                        [active-formatting-elements (cdr active)]))
         (begin0
             (car active)
           (current-parser-state next-state))]))

(define (insert-marker!)
  (define s (current-parser-state))
  (define new-elements (cons 'mark (parser-state-active-formatting-elements s)))
  (current-parser-state
   (struct-copy parser-state
                s
                [active-formatting-elements new-elements])))

(: adopt! (-> (U start-tag-token end-tag-token)
              Void))
(define (adopt! token)
  ; not implemented yet
  (void token))

(: set-form-pointer! (-> element-node Void))
(define (set-form-pointer! element)
  (define name (element-node-name element))
  (unless (string=? "form" name)
    (log-warning "Setting form pointer to a non-form (~a) element." name))
  (define new-state
    (struct-copy parser-state
                 (current-parser-state)
                 [form-pointer element]))
  (current-parser-state new-state))

(: clear-form-pointer! (-> Void))
(define (clear-form-pointer!)
  (current-parser-state (struct-copy parser-state
                                     (current-parser-state)
                                     [form-pointer #f])))

(: generate-all-implied-tags-thoroughly (-> Void))
(define (generate-all-implied-tags-thoroughly)
  (define top (current-top-open-element))
  (when (and (element-node? top)
             (element-has-name? top
                                (list "caption" "colgroup" "dd" "dt" "li" "optgroup" "option" "p" "rb" "rp" "rt" "rtc" "tbody" "tfoot" "th" "thead" "tr")))
    (pop-open-element!)
    (generate-all-implied-tags-thoroughly)))

(: current-top-formatting-element (-> (Option element-node)))
(define (current-top-formatting-element)
  (define s (current-parser-state))
  (findf element-node? (filter element-node?
                               (parser-state-active-formatting-elements s))))

(: clear-formatting-elements! (-> Void))
(define (clear-formatting-elements!)
  (when (element-node? (current-top-formatting-element))
    (pop-active-formatting-element!)
    (clear-formatting-elements!)))

(: pop-template-insertion-modes! (-> (Option InsertionMode)))
(define (pop-template-insertion-modes!)
  (define s (current-parser-state))
  (define modes (parser-state-template-insertion-modes s))
  (cond [(null? modes) #f]
        [else
         (begin0
             (car modes)
           (current-parser-state (struct-copy parser-state
                                              s
                                              [template-insertion-modes (cdr modes)])))]))

(: push-template-insertion-mode (-> InsertionMode Void))
(define (push-template-insertion-mode mode)
  (define s (current-parser-state))
  (define modes (parser-state-template-insertion-modes s))
  (current-parser-state (struct-copy parser-state
                                     s
                                     [template-insertion-modes (cons mode modes)])))

(: find-appropriate-insertion-mode (-> InsertionMode))
(define (find-appropriate-insertion-mode)
  ; massively incomplete; see https://html.spec.whatwg.org/multipage/parsing.html#the-insertion-mode
  'in-body)

(: reset-insertion-mode-appropriately (-> InsertionMode))
(define (reset-insertion-mode-appropriately)
  (: find-it (-> (Listof element-node)
                 InsertionMode))
  (define (find-it nodes)
    (cond [(null? nodes)
           (define context (current-context-node-for-fragment))
           (cond [(element-node? context)
                  (find-it (list context))]
                 [else 'in-body])]
          [else
           (define e (car nodes))
           (define n (element-node-name e))
           (cond [(string=? n "select")
                  (cond [(null? (cdr nodes)) 'in-select]
                        ; todo there's more here
                        [else (todo/log/compile-time 'in-select)])]
                 [(member n (list "td" "th") string=?)
                  'in-cell]
                 [(string=? n "tr")
                  'in-row]
                 [(member n (list "tbody" "thead" "tfoot") string=?)
                  'in-table-body]
                 [(string=? n "caption")
                  'in-caption]
                 [(string=? n "colgroup")
                  'in-column-group]
                 [(string=? n "table")
                  'in-table]
                 [(string=? n "template")
                  (define template-mode (current-template-insertion-mode))
                  (cond [(eq? #f template-mode)
                         'in-body]
                        [else template-mode])]
                 [(and (string=? n "head")
                       (not (null? (cdr nodes))))
                  'in-head]
                 [(string=? n "body")
                  'in-body]
                 [(string=? n "frameset")
                  'in-frameset]
                 [(string=? n "html")
                  (cond [(element-node? (current-head))
                         'after-head]
                        [else
                         'before-head])]
                 [(null? (cdr nodes))
                  'in-body]
                 [else
                  (find-it (cdr nodes))])]))
  (find-it (current-open-elements)))

(: currently-parsing-fragment? (-> Boolean))
(define (currently-parsing-fragment?)
  (element-node? (current-context-node-for-fragment)))

(: enable-foster-parenting! (-> Void))
(define (enable-foster-parenting!)
  (current-parser-state
   (struct-copy parser-state
                (current-parser-state)
                [foster-parenting-enabled? #t])))

(: disable-foster-parenting! (-> Void))
(define (disable-foster-parenting!)
  (current-parser-state
   (struct-copy parser-state
                (current-parser-state)
                [foster-parenting-enabled? #f])))

(: in-table (->* ()
                 ((Option (U EOF Token)))
                 Void))
(define (in-table [token #f])
  (: fallback (-> Void))
  (define (fallback)
    (enable-foster-parenting!)
    (in-body)
    (disable-foster-parenting!))
  (define t (or token (peek-token)))
  (cond [(eq? #f t)
         (in-body t)]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (or (character-token? t)
                  (character-reference-token? t)
                  (string-token? t))
              (current-node-has-name? (list "table" "tbody" "tfoot" "thead" "tr")))
         (push-insertion-mode 'in-table)
         (switch-mode 'in-table-text)]
        [(comment-token? t)
         (next-token)
         (define node (make-comment-node #:token t))
         (append-child! node)]
        [(doctype-token? t)
         (next-token)
         (raise-parse-error! (doctype-dropped t))]
        [(start-tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (define element (tag->element t))
         (cond [(string=? name "caption")
                (next-token)
                (clear-back-to-table)
                (insert-marker!)
                (append-child! element)
                (switch-mode 'in-caption)]
               [(string=? name "colgroup")
                (next-token)
                (clear-back-to-table)
                (append-child! element)
                (switch-mode 'in-column-group)]
               [(string=? name "col")
                (raise-parse-error! (element-induced "colgroup"))
                (define col (induce-element "colgroup"))
                (append-child! col)
                (switch-mode 'in-column-group)]
               [(member name (list "tbody" "tfoot" "thead") string=?)
                (next-token)
                (clear-back-to-table)
                (append-child! element)
                (switch-mode 'in-table-body)]
               [(member name (list "td" "th" "tr") string=?)
                (clear-back-to-table)
                (raise-parse-error! (element-induced "tbody"))
                (define tbody (induce-element "tbody"))
                (append-child! tbody)
                (switch-mode 'in-table-body)]
               [(string=? name "style")
                (in-head t)]
               [(string=? name "script")
                (in-head:script t)]
               [(string=? name "template")
                (in-head:template t)]
               [(string=? name "input")
                (define type (tag-token-attr-ref t "type"))
                (cond [(eq? #f type)
                       (fallback)]
                      [(regexp-match-exact? #px"(?i:hidden)" type)
                       (next-token)
                       (raise-parse-error! (unexpected-token t #f #f))
                       (append-child! element)
                       (pop-open-element!)]
                      [else
                       (fallback)])]
               [(string=? name "form")
                (next-token)
                (cond [(template-in-open-elements?)
                       (drop-token! t)]
                      [(element-node? (current-form-pointer))
                       (drop-token! t)]
                      [else
                       (append-child! element)
                       (set-form-pointer! element)
                       (pop-open-element!)])]
               [else
                (fallback)])]
        [(end-tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (cond [(string=? name "table")
                (cond [(table-element-in-table-scope?)
                       (keep-popping "table")
                       (switch-mode (reset-insertion-mode-appropriately))]
                      [else
                       (next-token)
                       (raise-parse-error! (unexpected-token t #f #t))])]
               [(member name (list "body" "caption" "col" "colgroup" "html" "tbody" "td" "tfoot" "th" "thead" "tr") string=?)
                (next-token)
                (raise-parse-error! (unexpected-token t #f #t))]
               [(string=? name "template")
                (in-head t)]
               [else
                (fallback)])]
        [else
         (fallback)]))

(: clear-back-to-table (-> Void))
(define (clear-back-to-table)
  (unless (current-node-has-name? (list "table" "template" "html"))
    (pop-open-element!)
    (clear-back-to-table)))

; https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-intabletext
(: in-table-text (->* ()
                      ((Listof (U character-token
                                  character-reference-token)))
                      Void))
(define (in-table-text [buffer (list)])
  (define (fallback)
    (define non-whitespace-char (findf (lambda ([x : (U character-token
                                                        character-reference-token)])
                                         (not (whitespace-token? x)))
                                       buffer))
    (cond [(or (character-token? non-whitespace-char)
               (character-reference-token? non-whitespace-char))
           (raise-parse-error! (unexpected-token non-whitespace-char #f #f))
           (for ([c : (U character-token
                         character-reference-token)
                    (reverse buffer)])
             (reconstruct-active-formatting-elements)
             (insert-character! c)
             (when (whitespace-token? c)
               (frameset-ok? #f)))]
          [else
           (for ([c : (U character-token
                         character-reference-token)
                    (reverse buffer)])
             (insert-character! c))])
    (stop-parsing))
  (define t (peek-token))
  (cond [(eq? #f t)
         (fallback)]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(string-token? t)
         (next-token)
         (define chars (string-token->character-tokens t))
         (for ([ct : character-token (in-list chars)])
           (when (null-character? ct)
             (raise-parse-error! (unexpected-token ct #f #f))))
         (define no-nulls (filter (lambda ([ct : character-token])
                                    (not (null-character? ct)))
                                  chars))
         (in-table-text (append (reverse chars)
                                buffer))]
        [(or (character-token? t)
             (character-reference-token? t))
         (next-token)
         (cond [(null-character? t)
                (raise-parse-error! (unexpected-token t #f #f))
                (in-table-text buffer)]
               [else
                (in-table-text (cons t buffer))])]
        [else
         (fallback)]))

; https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-intbody
(: in-table-body (-> Void))
(define (in-table-body)
  (: fallback (-> (U comment-token doctype-token character-token start-tag-token end-tag-token)
                  Void))
  (define (fallback t)
    (in-table t))
  (define t (peek-token))
  (cond [(eq? #f t)
         (in-table t)]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(start-tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (define element (tag->element t))
         (cond [(string=? name "tr")
                (next-token)
                (clear-back-to-table)
                (append-child! element)
                (switch-mode 'in-row)]
               [(member name (list "th" "td") string=?)
                (raise-parse-error! (element-induced "tr"))
                (define tr (induce-element "tr"))
                (append-child! tr)
                (switch-mode 'in-row)]
               [(member name (list "caption" "col" "colgroup" "tbody" "tfoot" "thead") string=?)
                (cond [(element-in-scope? (list "tbody" "thead" "tfoot"))
                       (clear-back-to-table)
                       (pop-open-element!)
                       (switch-mode 'in-table)]
                      [else
                       (next-token)
                       (raise-parse-error! (unexpected-token t #f #t))])]
               [else
                (fallback t)])]
        [(end-tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (cond [(member name (list "tbody" "tfoot" "thead") string=?)
                (next-token)
                (cond [(element-in-scope? name)
                       (clear-back-to-table)
                       (pop-open-element!)
                       (switch-mode 'in-table)]
                      [else
                       (raise-parse-error! (closing-tag-not-in-scope t (current-open-elements)))])]
               [(string=? name "table")
                (cond [(element-in-scope? (list "tbody" "thead" "tfoot"))
                       (clear-back-to-table)
                       (pop-open-element!)
                       (switch-mode 'in-table)]
                      [else
                       (next-token)
                       (raise-parse-error! (unexpected-token t #f #t))])]
               [(member name (list "body" "caption" "col" "colgroup" "html" "td" "th" "tr") string=?)
                (next-token)
                (raise-parse-error! (unexpected-token t #f #t))]
               [else
                (fallback t)])]
        [else
         (error (format "[in-table-body] Cannot handle token: ~a" t))]))

(: in-row (-> Void))
(define (in-row)
  (define t (peek-token))
  (cond [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(tokenizer-error? t)
         (raise-parse-error! (unexpected-token t #f #f))
         (void (next-token))]
        [(tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (define start? (start-tag-token? t))
         (define end? (end-tag-token? t))
         (cond [(and start? (member name (list "th" "td") string=?))
                (next-token)
                (clear-back-to-table)
                (append-child! (tag->element t))
                (switch-mode 'in-cell)
                (insert-marker!)]
               [(and end? (string=? name "tr"))
                (next-token)
                (cond [(tr-element-in-table-scope?)
                       (clear-back-to-table)
                       (check-top-element "tr")
                       (pop-open-element!)
                       (switch-mode 'in-table-body)]
                      [else
                       (raise-parse-error! (unexpected-token t #f #f))])]
               [(or (and start? (member name (list "caption" "col" "colgroup" "tbody" "tfoot" "thead" "tr") string=?))
                    (and end? (string=? name "table")))

                (cond [(tr-element-in-table-scope?)
                       (clear-back-to-table)
                       (check-top-element "tr")
                       (pop-open-element!)
                       (switch-mode 'in-table-body)]
                      [else
                       (next-token)
                       (raise-parse-error! (unexpected-token t #f #t))])]
               [(and end? (member name (list "tbody" "tfoot" "thead") string=?))
                (cond [(element-in-scope? name)
                       (clear-back-to-table)
                       (check-top-element "tr")
                       (switch-mode 'in-table-body)]
                      [else
                       (next-token)
                       (raise-parse-error! (unexpected-token t #f #t))])]
               [(and end? (member name (list "body" "caption" "col" "colgroup" "html" "td" "th") string=?))
                (next-token)
                (raise-parse-error! (unexpected-token t #f #t))]
               [else
                (in-table t)])]
        [(or (character-token? t)
             (character-reference-token? t)
             (string-token? t)
             (comment-token? t)
             (doctype-token? t))
         (in-table t)]
        [else
         (error (format "[in-row] Cannot handle token: ~a" t))]))

(: in-cell (-> Void))
(define (in-cell)
  (define t (peek-token))
  (cond [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(tokenizer-error? t)
         (raise-parse-error! (unexpected-token t #f #f))
         (void (next-token))]
        [(tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (define start? (start-tag-token? t))
         (define end? (end-tag-token? t))
         (cond [(and end? (member name (list "td" "th") string=?))
                (next-token)
                (cond [(element-in-scope? name)
                       (generate-implied-end-tags)
                       (unless (current-node-has-name? name)
                         (raise-parse-error! (unexpected-token t (list name) #f)))
                       (keep-popping name)
                       (clear-formatting-elements!)
                       (switch-mode 'in-row)]
                      [else
                       (raise-parse-error! (unexpected-token t #f #f))])]
               [(and start? (member name (list "caption" "col" "colgroup" "tbody" "td" "tfoot" "th" "thead" "tr") string=?))
                (cond [(element-in-scope? (list "td" "th"))
                       (close-the-cell t)]
                      [else
                       (next-token)
                       (raise-parse-error! (unexpected-token t #f #t))])]
               [(and end? (member name (list "body" "caption" "col" "colgroup" "html") string=?))
                (next-token)
                (raise-parse-error! (unexpected-token t #f #t))]
               [(and end? (member name (list "table" "tbody" "tfoot" "thead" "tr") string=?))
                (cond [(element-in-scope? name)
                       (close-the-cell t)]
                      [else
                       (next-token)
                       (raise-parse-error! (unexpected-token t #f #t))])]
               [else
                (in-body t)])]
        [(or (character-token? t)
             (character-reference-token? t)
             (string-token? t)
             (comment-token? t)
             (doctype-token? t))
         (in-body t)]
        [else
         (error (format "[in-cell] Cannot handle token: ~a" t))]))

(: close-the-cell (-> Token Void))
(define (close-the-cell token)
  (generate-implied-end-tags)
  (unless (current-node-has-name? (list "td" "th"))
    (raise-parse-error! (unexpected-token token (list "td" "th") #f)))
  (keep-popping (list "td" "th"))
  (clear-formatting-elements!)
  (switch-mode 'in-row))

(: in-select-in-table (-> Void))
(define (in-select-in-table)
  (define t (peek-token))
  (cond [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (define start? (start-tag-token? t))
         (define end? (end-tag-token? t))
         (cond [(and start? (member name (list "caption" "table" "tbody" "tfoot" "thead" "tr" "td" "th") string=?))
                (raise-parse-error! (unexpected-token t #f #f))
                (keep-popping "select")
                (switch-mode (reset-insertion-mode-appropriately))]
               [(and end? (member name (list "caption" "table" "tbody" "tfoot" "thead" "tr" "td" "th") string=?))
                (cond [(element-in-scope? name)
                       (raise-parse-error! (unexpected-token t #f #f))
                       (keep-popping "select")
                       (switch-mode (reset-insertion-mode-appropriately))]
                      [else
                       (next-token)
                       (raise-parse-error! (unexpected-token t #f #t))])])]
        [(or (character-token? t)
             (character-reference-token? t)
             (string-token? t)
             (doctype-token? t)
             (comment-token? t))
         (in-select t)]
        [else
         (error (format "[in-select-in-table] Cannot handle token ~a" t))]))

(: in-caption (-> Void))
(define (in-caption)
  (: handle-caption-and-switch (-> Boolean Token Void))
  (define (handle-caption-and-switch consume? t)
    (cond [(caption-element-in-table-scope?)
           (generate-implied-end-tags)
           (unless (current-node-has-name? "caption")
             (raise-parse-error! (unexpected-token t #f #f)))
           (keep-popping "caption")
           (clear-formatting-elements!)
           (when consume?
             (next-token))
           (switch-mode 'in-table)]
          [else
           (next-token)
           (raise-parse-error! (unexpected-token t #f #t))]))
  (define t (peek-token))
  (cond [(eq? #f t)
         (in-body t)]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(start-tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (cond [(member name (list "caption" "col" "colgroup" "tbody" "td" "tfoot" "th" "thead" "tr") string=?)
                (handle-caption-and-switch #f t)]
               [else
                (in-body t)])]
        [(end-tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (cond [(string=? name "caption")
                (next-token)
                (handle-caption-and-switch #t t)]
               [(string=? name "table")
                (handle-caption-and-switch #f t)])]
        [(or (character-token? t)
             (character-reference-token? t)
             (comment-token? t)
             (doctype-token? t))
         (in-body t)]
        [else
         (error (format "[in-caption] Cannot handle token: ~a" t))]))

(: in-column-group (-> Void))
(define (in-column-group)
  (: fallback (-> (U character-token
                     character-reference-token
                     string-token
                     start-tag-token
                     end-tag-token)
                  Void))
  (define (fallback t)
    ; not sure what to do with strings
    (cond [(current-node-has-name? "colgroup")
           (pop-open-element!)
           (switch-mode 'in-table)]
          [else
           (next-token)
           (raise-parse-error! (unexpected-token t #f #t))]))
  (define t (peek-token))
  (cond [(eq? #f t)
         (in-body t)]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(string-token? t)
         (cond [(empty-string? t)
                (void (next-token))]
               [(string-starts-with-whitespace? t)
                (consume-first-character t #t)]
               [else
                (fallback t)])]
        [(or (character-token? t)
             (character-reference-token? t))
         (cond [(whitespace-token? t)
                (next-token)
                (insert-character! t)]
               [else
                (fallback t)])]
        [(comment-token? t)
         (next-token)
         (append-child! (make-comment-node #:token t))]
        [(doctype-token? t)
         (next-token)
         (raise-parse-error! (doctype-dropped t))]
        [(start-tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (define element (tag->element t))
         (cond [(string=? name "html")
                (in-body t)]
               [(string=? name "col")
                (next-token)
                (append-child! element)
                (pop-open-element!)]
               [(string=? name "template")
                (in-head t)]
               [else
                (fallback t)])]
        [(end-tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (cond [(string=? name "colgroup")
                (next-token)
                (cond [(current-node-has-name? "colgroup")
                       (raise-parse-error! (unexpected-token t #f #t))]
                      [else
                       (pop-open-element!)
                       (switch-mode 'in-table)])]
               [(string=? name "col")
                (next-token)
                (raise-parse-error! (unexpected-token t #f #t))]
               [(string=? name "template")
                (in-head t)]
               [else
                (fallback t)])]
        [else
         (error (format "[in-column-group] Cannot handle token: ~a" t))]))

; https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-intemplate
(: in-template (-> Void))
(define (in-template)
  (define t (peek-token))
  (cond [(eq? #f t)
         (cond [(template-in-open-elements?)
                (raise-parse-error! (unexpected-token eof #f #f))
                (keep-popping "template")
                (clear-formatting-elements!)
                (pop-template-insertion-modes!)
                (switch-mode (reset-insertion-mode-appropriately))]
               [else
                (stop-parsing)])]
        [(tokenizer-error? t)
         (raise-parse-error! t)
         (void (next-token))]
        [(and (character-token? t)
              (deleted-character? t))
         (void (next-token))]
        [(or (character-token? t)
             (character-reference-token? t)
             (string-token? t)
             (comment-token? t)
             (doctype-token? t))
         (in-body t)]
        [(tag-token? t)
         (define name (list->string (enumerate-output-characters (tag-token-name t))))
         (define start? (start-tag-token? t))
         (define end? (end-tag-token? t))
         (cond [(or (and start? (member name (list "base" "basefont" "bgsound" "link" "meta" "noframes" "script" "style" "template" "title") string=?))
                    (and end? (string=? name "template")))
                (in-head t)]
               [(and start? (member name (list "caption" "colgroup" "tbody" "tfoot" "thead") string=?))
                (pop-template-insertion-modes!)
                (push-template-insertion-mode 'in-table)
                (switch-mode 'in-table)]
               [(and start? (string=? name "col"))
                (pop-template-insertion-modes!)
                (push-template-insertion-mode 'in-column-group)
                (switch-mode 'in-column-group)]
               [(and start? (string=? name "tr"))
                (pop-template-insertion-modes!)
                (push-template-insertion-mode 'in-body)
                (switch-mode 'in-table-body)]
               [(and start? (member name (list "td" "th") string=?))
                (pop-template-insertion-modes!)
                (push-template-insertion-mode 'in-row)
                (switch-mode 'in-row)]
               [start?
                (pop-template-insertion-modes!)
                (push-template-insertion-mode 'in-body)
                (switch-mode 'in-body)]
               [end?
                (next-token)
                (raise-parse-error! (unexpected-token t #f #t))])]))

(: parsers (Immutable-HashTable InsertionMode (-> Void)))
(define parsers
  (hash 'initial initial
        'before-html before-html
        'before-head before-head
        'in-head in-head
        'in-head-noscript in-head-noscript
        'after-head after-head
        'in-body in-body
        'text text
        'in-table in-table
        'in-table-text in-table-text
        'in-caption in-caption
        'in-column-group in-column-group
        'in-table-body in-table-body
        'in-row in-row
        'in-cell in-cell
        'in-select in-select
        'in-select-in-table in-select-in-table
        'in-template in-template
        'after-body after-body
        'in-frameset in-frameset
        'after-frameset after-frameset
        'after-after-body after-after-body
        'after-after-frameset after-after-frameset))

(: parse/1 (-> Void))
(define (parse/1)
  (define s (current-parser-state))
  (define mode (parser-state-insertion-mode s))
  (unless (null? mode)
    (define p (hash-ref parsers (car mode)))
    (p)))

(: keep-parsing (-> Void))
(define (keep-parsing)
  (parse/1)
  (define s (current-parser-state))
  (define mode (parser-state-insertion-mode s))
  (unless (null? mode)
    (keep-parsing)))

(: parse (-> (U String Bytes)
             document))
(define (parse str)
  (define in (cond [(string? str) (open-input-string str)]
                   [else (open-input-bytes str)]))
  (port-count-lines! in)
  (define validated-in (make-validating-input-port in))
  (parameterize ([current-input-port validated-in]
                 [include-dropped-chars? #t]
                 [include-tokenizer-errors? #t])
    (keep-parsing)
    (->html (parser-state-document (current-parser-state)))))

(: stop-parsing (-> Void))
(define (stop-parsing)
  (current-parser-state
   (struct-copy parser-state
                (current-parser-state)
                [insertion-mode (list)])))

(module+ main
  (require racket/cmdline
           racket/pretty
           racket/port
           typed/racket/unsafe)
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
    (pretty-print (parse r))))
