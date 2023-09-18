#lang racket/base

(require laramie)

(module+ test
  (require rackunit))

(define document #<<DOC
<!doctype html>
<html>
  <head>
    <title>foo</title>
  </head>
  <body>
    <main><table><thead><tr><th>a</th><th>b</th><th>c</th></tr></thead></table></main>
  </body>
</html>
DOC
)

(module+ test
  (check-not-exn (lambda () (parse document))))
