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
  (and (element? x)
       (equal? 'script (element-local-name x))))

(module+ test
  (define elements (filter element? (descendants (parse document))))
  (define script-element (findf script-element? elements))
  (check-not-false script-element)
  (define script-kiddies (element-content script-element))
  (check-equal? script-kiddies
                (list "hey!")))
