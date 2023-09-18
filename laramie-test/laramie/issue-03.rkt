#lang racket/base

(require laramie)

(module+ test
  (require rackunit))

(define document #<<DOC
<!doctype html>
<html>
  <head>
    <title>foo</title>
  </head>
  <body>
    <main><table><thead><tr><th>a</th><th>b</th><th>c</th></tr></thead></table></main>
  </body>
</html>
DOC
)

(define (style-element? x)
  (and (element? x)
       (equal? 'style (element-local-name x))))

(define elements (filter element? (descendants (parse document))))

(define style-element (findf style-element? elements))

(module+ test
  (check-not-false style-element)
  (check-equal?
   (element-content style-element)
   (list
    "\n#main_welcome\n{\n    width:380px;\n    height:480px;\n    float:left;\n}\n")))
