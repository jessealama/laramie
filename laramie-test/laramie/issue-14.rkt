#lang racket/base

(require laramie
         (file "util.rkt"))

(module+ test
  (require rackunit))

(define simple-html #<<BOOTSTRAP
<!doctype html>
<html lang="en">
  <head>
    <title>Hello, world!</title>
  </head>
  <body>
    <h1>Hello, world!</h1>
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta2/dist/js/bootstrap.bundle.min.js" integrity="sha384-b5kHyXgcpbZJO/tY9Ul7kGkf1S0CWuKcCD38l8YkeH8z8QjE0GmW1gYU5S9FOnJ0" crossorigin="anonymous"></script>
  </body>
</html>
BOOTSTRAP
)

(module+ test
  (let* ([test-name "Issue 14: Simple warmup to Bootstrap starter template"]
         [result (parse simple-html)]
         [root (document-element result)]
         [doc-children (filter element? (element-content root))])
    (test-begin
      (check-length doc-children 1)
      (define html-node (car doc-children))
      (test-case
          "html node present"
        (check-equal? (element-local-name html-node)
                      'html))
      (define html-children (filter element? (element-content html-node)))
      (define first-element (car html-children))
      (test-case
          "2 children of the HTML node"
        (check-length html-children 2))
      (define head-element (car html-children))
      (define body-element (cadr html-children))
      (test-case
          "head element is the first child"
        (check-equal? (element-local-name head-element)
                      'head))
      (test-case
          "body element is the second child"
        (check-equal? (element-local-name body-element)
                      'body))
      (define head-children (filter element? (element-content head-element)))
      (define body-children (filter element? (element-content body-element)))
      (test-case
          "head has 1 element child"
        (check-length head-children 1))
      (test-case
          "body has 2 element children"
        (check-length body-children 2)))))
