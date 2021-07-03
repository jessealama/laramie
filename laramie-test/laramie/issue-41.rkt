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

(module+ test
  (check-match (parser-state-errors state)
               (list (unexpected-token _ _ _))))
