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
  (and (element-node? x)
       (string=? "style"
                 (element-node-name x))))

(define elements (enumerate-elements (parse document)))

(define style-element (findf style-element? elements))

(module+ test
  (check-not-false style-element)
  (check-equal?
   (element-node-children style-element)
   (list
    (string-token
     (location 5 7 51)
     (location 12 0 121)
     "\n#main_welcome\n{\n    width:380px;\n    height:480px;\n    float:left;\n}\n"))))
