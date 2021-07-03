#lang typed/racket/base

(require (file "util.rkt")
         (file "../src/types.rkt")
         (file "../src/tokenize.rkt")
         (file "../src/parser.rkt")
         (file "../src/parameters.rkt")
         (file "../src/stream.rkt")
         (file "../src/parser.rkt")
         (file "../src/dom.rkt")
         (file "../src/data.rkt"))

(module+ test
  (require typed/rackunit
           racket/format))

(module+ test
  (let* ([test-name "Bootstrap starter template is parsable"]
         [html #<<BOOTSTRAP
<!doctype html>
<html lang="en">
  <head>
    <!-- Required meta tags -->
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">

    <!-- Bootstrap CSS -->
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta2/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-BmbxuPwQa2lc/FVzBcNJ7UAyJxM6wuqIj61tLrc4wSX0szH/Ev+nYRRuWlolflfl" crossorigin="anonymous">

    <title>Hello, world!</title>
  </head>
  <body>
    <h1>Hello, world!</h1>
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta2/dist/js/bootstrap.bundle.min.js" integrity="sha384-b5kHyXgcpbZJO/tY9Ul7kGkf1S0CWuKcCD38l8YkeH8z8QjE0GmW1gYU5S9FOnJ0" crossorigin="anonymous"></script>
  </body>
</html>
BOOTSTRAP
])
    (check-not-false (parse html))))
