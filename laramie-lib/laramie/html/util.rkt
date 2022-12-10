#lang typed/racket/base/optional

(provide descendants)

(require (file "types.rkt"))

(: descendants (-> (U String
                      document
                      prolog
                      comment
                      element
                      document-type
                      (Listof (U String
                                 comment
                                 element)))
                   (Listof (U element
                              comment
                              document-type
                              String))))
(define (descendants html-thing)
  (cond [(document? html-thing)
         (define prolog (document-prolog html-thing))
         (define element (document-element html-thing))
         (define misc (document-misc html-thing))
         (append (descendants prolog)
                 (list element)
                 (descendants (document-misc html-thing))
                 (descendants element))]
        [(prolog? html-thing)
         (define misc (prolog-misc html-thing))
         (define dtd (prolog-dtd html-thing))
         (append (descendants misc)
                 (cond [(eq? #f dtd) (list)]
                       [else (cons dtd (descendants dtd))]))]
        [(or (document-type? html-thing)
             (comment? html-thing))
         (list)]
        [(element? html-thing)
         (descendants (element-content html-thing))]
        [(null? html-thing)
         (list)]
        [(list? html-thing)
         (define x (car html-thing))
         (append (list x)
                 (descendants x)
                 (descendants (cdr html-thing)))]
        [else
         (list)]))
