#lang racket/base

(require laramie)

(module+ test
  (require rackunit
           (file "util.rkt")))

(module+ test
  (let* ([test-name "Comment and doctype"]
         [result (parse "<!--xyz--><!doctype html>")]
         [p (document-prolog result)])
    (test-begin
      (test-case
          (string-append test-name " [one comment")
        (check-= (length (prolog-misc p))
                 1
                 0))
      (test-case
          (string-append test-name " [one DTD")
        (check-not-false (prolog-dtd p) 1)))))

; TODO (not close to working yet)
#;
(module+ test
  ; https://html.spec.whatwg.org/multipage/parsing.html#parse-error-non-void-html-element-start-tag-with-trailing-solidus
  (let ([test-name "non-void HTML start tag with trailing solidus"]
        [result (parse "<div/><span></span><span></span>")])
    (test-begin
      (check-length (enumerate-elements result) 5))))
