#lang racket/base

(require laramie)

(module+ test
  (require rackunit))

(define document #<<DOC
<!doctype html>
<html><head></head><body></body></html>
DOC
)

(define rendered (render (parse document)))

(module+ test
  (check-equal?
   (render (parse document))
   '((div ((class "doctype") (doctype "doctype")) (span ((class "name")) "html"))
     (html (head) (body)))))
