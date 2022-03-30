#lang racket/base

(require laramie)

(module+ test
  (require rackunit))

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
    <link href="f" rel="stylesheet" integrity="sha384" crossorigin="anonymous">
    <title>Hello, world!</title>
  </head>
  <body>
    <h1>Hello, world!</h1>
    <script src="bar" integrity="sha385" crossorigin="anonymous"></script>
  </body>
</html>
BOOTSTRAP
])
    (check-not-false (parse html))))
