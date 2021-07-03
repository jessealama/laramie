#lang typed/racket/base

(require laramie)

(module+ test
  (require typed/rackunit))

(define document #<<DOC
<html>
  <body>
    <p>foobar</p>
  </body>
</html>
DOC
)

(module+ test
  (check-not-false (member "foobar" (strings-in-elements (parse document)))))
