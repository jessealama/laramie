#lang racket/base

(require laramie)

(module+ test
  (require rackunit))

(define document #<<DOC
<!doctype html>
<html>
<head>
  <script>hey!</script>
<head>
<body></body>
DOC
)

(define (script-element? x)
  (and (element-node? x)
       (string=? "script"
                 (element-node-name x))))

(module+ test
  (define elements (enumerate-elements (parse document)))
  (define script-element (findf script-element? elements))
  (check-not-false script-element)
  (define script-kiddies (element-node-children script-element))
  (check-equal? script-kiddies
                (list
                 (string-token (location 4 8 39)
                               (location 4 12 43)
                               "hey!"))))
