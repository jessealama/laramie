#lang typed/racket/base

(require laramie)

(module+ test
  (require typed/rackunit
           racket/format))

(module+ test
  (let* ([test-name "Comment and doctype"]
         [result (parse "<!--xyz--><!doctype html>")]
         [document (parser-state-document result)]
         [dropped (parser-state-dropped result)]
         [doc-children (document-node-children document)])
    (test-begin
      (test-case
          (string-append test-name " [dropped 0 tokens]")
        (check-length dropped 0))
      (test-case
          (string-append " [two children of the document]")
        (check-length doc-children 2))
      (test-case
          (string-append " [first child is comment node]")
        (check-true (comment-node? (car doc-children))
                    (~a (car doc-children))))
      (test-case
          (string-append test-name " [second child is doctype node]")
        (check-true (doctype-node? (cadr doc-children))
                    (~a (cadr doc-children)))))))

; TODO (not close to working yet)
#;
(module+ test
  ; https://html.spec.whatwg.org/multipage/parsing.html#parse-error-non-void-html-element-start-tag-with-trailing-solidus
  (let ([test-name "non-void HTML start tag with trailing solidus"]
        [result (parse "<div/><span></span><span></span>")])
    (test-begin
      (check-length (enumerate-elements result) 5))))
