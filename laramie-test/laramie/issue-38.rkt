#lang racket/base

(require laramie)

(module+ test
  (require rackunit))

(define document #<<DOC
<!doctype html>
<html>
<head><head>
<body>
  <style>
#main_welcome
{
    width:380px;
    height:480px;
    float:left;
}
</style>
</body>
</html>
DOC
)

(define (style-element? x)
  (and (element? x)
       (string=? "style" (element-local-name x))))

(define elements (filter element? (descendants (parse document))))

(define style-element (findf style-element? elements))

(module+ test
  (check-not-false style-element)
  (check-equal?
   (element-content style-element)
   (list
    "\n#main_welcome\n{\n    width:380px;\n    height:480px;\n    float:left;\n}\n")))
