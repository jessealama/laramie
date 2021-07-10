#lang racket/base

(require laramie)

(module+ test
  (require rackunit))

(define document #<<DOC
<!doctype html>
<html><head></head><body><p>Hey there!</li></body></html>
DOC
)

(define state (parse document))

; not sure how to represent this:
#;
(module+ test
  (check-match (current-parser-errors)
               (list (unexpected-token _ _ _))))
