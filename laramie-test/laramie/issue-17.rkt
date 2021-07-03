#lang typed/racket/base

(require laramie
         (file "util.rkt"))

(module+ test
  (require typed/rackunit
           racket/format))

(define html #<<HTML
<!doctype html>
<html>
  <head></head>
  <body><!-- hey--></body>
</html>
HTML
)

(define state (parse html))

(module+ test
  (test-begin
    (define doc-descendents (descendants state))
    (test-case
        "Enough descendants found"
      (check-length doc-descendents 5))
    (define doctype-descendants (filter doctype-node? doc-descendents))
    (define comment-descendants (filter comment-node? doc-descendents))
    (test-case
        "Document has one doctype"
      (check-length doctype-descendants 1))
    (define doctype (car doctype-descendants))
    (test-case
        "Document has one comment descendant"
      (check-length comment-descendants 1))
    (define comment (car comment-descendants))
    (test-case
        "Parent of comment is set"
      (check-not-false (element-node? (comment-node-parent comment))))
    (test-case
        "Parent of doctype is set"
      (check-not-false (document-node? (doctype-node-parent doctype))))
    (test-case
        "Document of comment equals document of doctype"
      (check-equal? (document-of comment)
                    (document-of doctype)))))
