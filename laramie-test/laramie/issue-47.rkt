#lang racket/base

(require laramie)

(module+ test
  (require rackunit))

(define document #<<DOC
<!doctype html>
<html>
  <head>
    <title>Hi!</title>
  </head>
  <body>
    <p>Hey there!</p>
  </body>
</html>
DOC
)
