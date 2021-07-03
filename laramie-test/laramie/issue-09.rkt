#lang typed/racket/base

(require laramie
         (file "util.rkt"))

(module+ test
  (require typed/rackunit
           racket/format))

(module+ test
  (let ([test-name "issues/9"])
    (test-begin
      (test-case
          (string-append test-name " [does not crash]")
        (check-not-exn (lambda () (parse "<html>")))))))
