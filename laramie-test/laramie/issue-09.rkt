#lang racket/base

(require laramie)

(module+ test
  (require rackunit))

(module+ test
  (let ([test-name "issues/9"])
    (test-begin
      (test-case
          (string-append test-name " [does not crash]")
        (check-not-exn (lambda () (parse "<html>")))))))
