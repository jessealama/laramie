#lang racket/base

(require laramie
         (file "util.rkt"))

(module+ test
  (require rackunit))

(define html #<<HTML
<!doctype html>
<html>
  <head></head>
  <body><!-- hey--></body>
</html>
HTML
)

(define doc (parse html))

(module+ test
  (test-begin
    (define doc-descendents (descendants doc))
    (test-case
        "Enough descendants found"
      (check-length doc-descendents 5))
    (define doctype-descendants (filter document-type? doc-descendents))
    (define comment-descendants (filter comment? doc-descendents))
    (test-case
        "Document has one doctype"
      (check-length doctype-descendants 1))
    (define doctype (car doctype-descendants))
    (test-case
        "Document has one comment descendant"
      (check-length comment-descendants 1))))
