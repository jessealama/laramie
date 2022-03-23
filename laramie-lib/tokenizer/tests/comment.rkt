#lang racket/base

(require racket/require
         (multi-in ".."
                   ("types.rkt"
                    "tokenize.rkt"
                    "tokens.rkt"
                    "comment.rkt")))

(module+ test
  (require rackunit
           syntax/parse/define))

(module+ test
  (define-simple-macro (check-it test-name subject bogus? value)
    (let ([tokens (tokenize subject
                            #:include-dropped? #t
                            #:include-errors? #t)])
      (test-case
          (format "~a [value]" test-name)
        (check-equal? tokens
                      value))
      (test-case
          (format "~a [length]" test-name)
        (check-= (length (enumerate-input-characters tokens))
                 (string-length subject)
                 0))
      (let ([comment (findf comment-token? tokens)])
        (cond [(comment-token? comment)
               (test-case
                   (format "~a [bogus]" test-name)
                 (check-equal? (comment-token-bogus? comment)
                               bogus?))]
              [else
               (error "No comment token found!")])))))

(module+ test
  (check-it
   "Comment contains \"<!\", then EOF"
   "<!-- k<!"
   #f
   (list
    (eof-in-comment (location 1 8 9) #f)
    (comment-token
     (location 1 0 1)
     (location 1 8 9)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\k)
      (character-token (location 1 6 7) (location 1 7 8) #\<)
      (character-token (location 1 7 8) (location 1 8 9) #\!))
     #f
     #f
     #f)))
  (check-it
   "Comment contains \"<\", not followed by \"!\""
   "<!-- x<y-->"
   #f
   (list
    (comment-token
     (location 1 0 1)
     (location 1 11 12)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\x)
      (character-token (location 1 6 7) (location 1 7 8) #\<)
      (character-token (location 1 7 8) (location 1 8 9) #\y))
     #f
     (list
      (character-token (location 1 8 9) (location 1 9 10) #\-)
      (character-token (location 1 9 10) (location 1 10 11) #\-))
     (character-token (location 1 10 11) (location 1 11 12) #\>))))
  (check-it
   "Comment contains two \"<\" signs"
   "<!-- <<-->"
   #f
   (list
    (comment-token
     (location 1 0 1)
     (location 1 10 11)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\<)
      (character-token (location 1 6 7) (location 1 7 8) #\<))
     #f
     (list
      (character-token (location 1 7 8) (location 1 8 9) #\-)
      (character-token (location 1 8 9) (location 1 9 10) #\-))
     (character-token (location 1 9 10) (location 1 10 11) #\>))))
  (check-it
   "Comment EOFs after \"<\""
   "<!-- p<"
   #f
   (list
    (eof-in-comment (location 1 7 8) #f)
    (comment-token
     (location 1 0 1)
     (location 1 7 8)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\p)
      (character-token (location 1 6 7) (location 1 7 8) #\<))
     #f
     #f
     #f)))
  (check-it
   "Comment ends with three dashes"
   "<!-- d--->"
   #f
   (list
    (comment-token
     (location 1 0 1)
     (location 1 10 11)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\d)
      (character-token (location 1 6 7) (location 1 7 8) #\-))
     #f
     (list
      (character-token (location 1 7 8) (location 1 8 9) #\-)
      (character-token (location 1 8 9) (location 1 9 10) #\-))
     (character-token (location 1 9 10) (location 1 10 11) #\>))))
  (check-it
   "Comment contains \"<!-\""
   "<!-- x <!- y-->"
   #f
   (list
    (comment-token
     (location 1 0 1)
     (location 1 15 16)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\x)
      (character-token (location 1 6 7) (location 1 7 8) #\space)
      (character-token (location 1 7 8) (location 1 8 9) #\<)
      (character-token (location 1 8 9) (location 1 9 10) #\!)
      (character-token (location 1 9 10) (location 1 10 11) #\-)
      (character-token (location 1 10 11) (location 1 11 12) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\y))
     #f
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\-)
      (character-token (location 1 13 14) (location 1 14 15) #\-))
     (character-token (location 1 14 15) (location 1 15 16) #\>))))
  (check-it
   "Comment EOFs after \"<!-"
   "<!-- l<!-"
   #f
   (list
    (eof-in-comment (location 1 9 10) #f)
    (comment-token
     (location 1 0 1)
     (location 1 9 10)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\l)
      (character-token (location 1 6 7) (location 1 7 8) #\<)
      (character-token (location 1 7 8) (location 1 8 9) #\!))
     #f
     (list (character-token (location 1 8 9) (location 1 9 10) #\-))
     #f)))
  (check-it
   "Comment contains nested comment"
   "<!-- x <!-- y -->"
   #f
   (list
    (nested-comment (location 1 11 12) #f)
    (comment-token
     (location 1 0 1)
     (location 1 17 18)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\x)
      (character-token (location 1 6 7) (location 1 7 8) #\space)
      (character-token (location 1 8 9) (location 1 9 10) #\!)
      (character-token (location 1 7 8) (location 1 8 9) #\<)
      (character-token (location 1 9 10) (location 1 10 11) #\-)
      (character-token (location 1 10 11) (location 1 11 12) #\-)
      (character-token (location 1 11 12) (location 1 12 13) #\space)
      (character-token (location 1 12 13) (location 1 13 14) #\y)
      (character-token (location 1 13 14) (location 1 14 15) #\space))
     #f
     (list
      (character-token (location 1 14 15) (location 1 15 16) #\-)
      (character-token (location 1 15 16) (location 1 16 17) #\-))
     (character-token (location 1 16 17) (location 1 17 18) #\>))))
  (check-it
   "Comment contains \"<!--, followed by EOF"
   "<!-- x <!--"
   #f
   (list
    (eof-in-comment (location 1 11 12) #f)
    (comment-token
     (location 1 0 1)
     (location 1 11 12)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\x)
      (character-token (location 1 6 7) (location 1 7 8) #\space)
      (character-token (location 1 8 9) (location 1 9 10) #\!)
      (character-token (location 1 7 8) (location 1 8 9) #\<))
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\-)
      (character-token (location 1 10 11) (location 1 11 12) #\-))
     #f)))
  (check-it
   "Looks like comment is about to end with \"--!\", but it doesn't"
   "<!-- x --! y-->"
   #f
   (list
    (comment-token
     (location 1 0 1)
     (location 1 15 16)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\x)
      (character-token (location 1 6 7) (location 1 7 8) #\space)
      (character-token (location 1 7 8) (location 1 8 9) #\-)
      (character-token (location 1 8 9) (location 1 9 10) #\-)
      (character-token (location 1 9 10) (location 1 10 11) #\!)
      (character-token (location 1 10 11) (location 1 11 12) #\space)
      (character-token (location 1 11 12) (location 1 12 13) #\y))
     #f
     (list
      (character-token (location 1 12 13) (location 1 13 14) #\-)
      (character-token (location 1 13 14) (location 1 14 15) #\-))
     (character-token (location 1 14 15) (location 1 15 16) #\>))))
  (check-it
   "Comment ends with \"--!-\""
   "<!-- x --!-->"
   #f
   (list
    (comment-token
     (location 1 0 1)
     (location 1 13 14)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\x)
      (character-token (location 1 6 7) (location 1 7 8) #\space)
      (character-token (location 1 7 8) (location 1 8 9) #\-)
      (character-token (location 1 8 9) (location 1 9 10) #\-)
      (character-token (location 1 9 10) (location 1 10 11) #\!))
     #f
     (list
      (character-token (location 1 10 11) (location 1 11 12) #\-)
      (character-token (location 1 11 12) (location 1 12 13) #\-))
     (character-token (location 1 12 13) (location 1 13 14) #\>))))
  (check-it
   "EOF in comment that ends with \"--!\""
   "<!-- wat --!"
   #f
   (list
    (eof-in-comment (location 1 12 13) #f)
    (comment-token
     (location 1 0 1)
     (location 1 12 13)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\w)
      (character-token (location 1 6 7) (location 1 7 8) #\a)
      (character-token (location 1 7 8) (location 1 8 9) #\t)
      (character-token (location 1 8 9) (location 1 9 10) #\space))
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\-)
      (character-token (location 1 10 11) (location 1 11 12) #\-))
     #f)
    (character-token (location 1 11 12) (location 1 12 13) '(#\! . #f))))
  (check-it
   "Comment ends with \"--!\""
   "<!-- wat --!>"
   #f
   (list
    (incorrectly-closed-comment
     (location 1 12 13)
     (character-token (location 1 11 12) (location 1 12 13) #\!))
    (comment-token
     (location 1 0 1)
     (location 1 13 14)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\w)
      (character-token (location 1 6 7) (location 1 7 8) #\a)
      (character-token (location 1 7 8) (location 1 8 9) #\t)
      (character-token (location 1 8 9) (location 1 9 10) #\space))
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\-)
      (character-token (location 1 10 11) (location 1 11 12) #\-))
     (character-token (location 1 12 13) (location 1 13 14) #\>))))
  (check-it
   "Commends ends with dash followed by non-dash, then ends properly"
   "<!-- h -ey-->"
   #f
   (list
    (comment-token
     (location 1 0 1)
     (location 1 13 14)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\h)
      (character-token (location 1 6 7) (location 1 7 8) #\space)
      (character-token (location 1 7 8) (location 1 8 9) #\-)
      (character-token (location 1 8 9) (location 1 9 10) #\e)
      (character-token (location 1 9 10) (location 1 10 11) #\y))
     #f
     (list
      (character-token (location 1 10 11) (location 1 11 12) #\-)
      (character-token (location 1 11 12) (location 1 12 13) #\-))
     (character-token (location 1 12 13) (location 1 13 14) #\>))))
  (check-it
   "EOF in comment after single dash"
   "<!-- hey -"
   #f
   (list
    (eof-in-comment (location 1 10 11) #f)
    (comment-token
     (location 1 0 1)
     (location 1 10 11)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\h)
      (character-token (location 1 6 7) (location 1 7 8) #\e)
      (character-token (location 1 7 8) (location 1 8 9) #\y)
      (character-token (location 1 8 9) (location 1 9 10) #\space))
     #f
     (list (character-token (location 1 9 10) (location 1 10 11) #\-))
     #f)))
  (check-it
   "Comment ends after double dashes"
   "<!-- hey --"
   #f
   (list
    (eof-in-comment (location 1 11 12) #f)
    (comment-token
     (location 1 0 1)
     (location 1 11 12)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\h)
      (character-token (location 1 6 7) (location 1 7 8) #\e)
      (character-token (location 1 7 8) (location 1 8 9) #\y)
      (character-token (location 1 8 9) (location 1 9 10) #\space))
     #f
     (list
      (character-token (location 1 9 10) (location 1 10 11) #\-)
      (character-token (location 1 10 11) (location 1 11 12) #\-))
     #f)))
  (check-it
   "Closing \">\" after three initial dashes"
   "<!--->"
   #f
   (list
    (abrupt-closing-of-empty-comment (location 1 6 7) #f)
    (comment-token
     (location 1 0 1)
     (location 1 6 7)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     '()
     #f
     (list (character-token (location 1 4 5) (location 1 5 6) #\-))
     (character-token (location 1 5 6) (location 1 6 7) #\>))))
  (check-it
   "Four dashes"
   "<!---->"
   #f
   (list
    (comment-token
     (location 1 0 1)
     (location 1 7 8)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     '()
     #f
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\-)
      (character-token (location 1 5 6) (location 1 6 7) #\-))
     (character-token (location 1 6 7) (location 1 7 8) #\>))))
  (check-it
   "Three initial dashes, then EOF"
   "<!---"
   #f
   (list
    (eof-in-comment (location 1 5 6) #f)
    (comment-token
     (location 1 0 1)
     (location 1 5 6)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     '()
     #f
     (list (character-token (location 1 4 5) (location 1 5 6) #\-))
     #f)))
  (check-it
   "Null character in comment"
   (format "<!-- h~a-->" #\nul)
   #f
   (list
    (unexpected-null-character (location 1 6 7) #f)
    (comment-token
     (location 1 0 1)
     (location 1 10 11)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\space)
      (character-token (location 1 5 6) (location 1 6 7) #\h)
      (character-token (location 1 6 7) (location 1 7 8) '(#\nul . #\�)))
     #f
     (list
      (character-token (location 1 7 8) (location 1 8 9) #\-)
      (character-token (location 1 8 9) (location 1 9 10) #\-))
     (character-token (location 1 9 10) (location 1 10 11) #\>))))
  (check-it
   "Abrupt closing of comment"
   "<!-->x"
   #f
   (list
    (abrupt-closing-of-empty-comment (location 1 4 5) #f)
    (comment-token
     (location 1 0 1)
     (location 1 5 6)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     '()
     #f
     #f
     (character-token (location 1 4 5) (location 1 5 6) #\>))
    (character-token (location 1 5 6) (location 1 6 7) #\x)))
  (check-it
   "Three initial dashes in comment"
   "<!---x-->"
   #f
   (list
    (comment-token
     (location 1 0 1)
     (location 1 9 10)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\-)
      (character-token (location 1 5 6) (location 1 6 7) #\x))
     #f
     (list
      (character-token (location 1 6 7) (location 1 7 8) #\-)
      (character-token (location 1 7 8) (location 1 8 9) #\-))
     (character-token (location 1 8 9) (location 1 9 10) #\>))))
  (check-it
   "EOF in comment"
   "<!--"
   #f
   (list
    (eof-in-comment (location 1 4 5) #f)
    (comment-token
     (location 1 0 1)
     (location 1 4 5)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     '()
     #f
     #f
     #f)))
  (check-it
   "EOF in non-empty comment"
   "<!--end"
   #f
   (list
    (eof-in-comment (location 1 7 8) #f)
    (comment-token
     (location 1 0 1)
     (location 1 7 8)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\e)
      (character-token (location 1 5 6) (location 1 6 7) #\n)
      (character-token (location 1 6 7) (location 1 7 8) #\d))
     #f
     #f
     #f)))
  (check-it
   "Straightforward comment"
   "<!--comment-->"
   #f
   (list
    (comment-token
     (location 1 0 1)
     (location 1 14 15)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\c)
      (character-token (location 1 5 6) (location 1 6 7) #\o)
      (character-token (location 1 6 7) (location 1 7 8) #\m)
      (character-token (location 1 7 8) (location 1 8 9) #\m)
      (character-token (location 1 8 9) (location 1 9 10) #\e)
      (character-token (location 1 9 10) (location 1 10 11) #\n)
      (character-token (location 1 10 11) (location 1 11 12) #\t))
     #f
     (list
      (character-token (location 1 11 12) (location 1 12 13) #\-)
      (character-token (location 1 12 13) (location 1 13 14) #\-))
     (character-token (location 1 13 14) (location 1 14 15) #\>))))
  (check-it
   "A nearly-nested comment.

See https://html.spec.whatwg.org/multipage/syntax.html#comments"
   "<!--My favorite operators are > and <!-->."
   #f
   (list
    (comment-token
     (location 1 0 1)
     (location 1 41 42)
     (character-token (location 1 0 1) (location 1 1 2) #\<)
     (character-token (location 1 1 2) (location 1 2 3) #\!)
     (list
      (character-token (location 1 2 3) (location 1 3 4) #\-)
      (character-token (location 1 3 4) (location 1 4 5) #\-))
     (list
      (character-token (location 1 4 5) (location 1 5 6) #\M)
      (character-token (location 1 5 6) (location 1 6 7) #\y)
      (character-token (location 1 6 7) (location 1 7 8) #\space)
      (character-token (location 1 7 8) (location 1 8 9) #\f)
      (character-token (location 1 8 9) (location 1 9 10) #\a)
      (character-token (location 1 9 10) (location 1 10 11) #\v)
      (character-token (location 1 10 11) (location 1 11 12) #\o)
      (character-token (location 1 11 12) (location 1 12 13) #\r)
      (character-token (location 1 12 13) (location 1 13 14) #\i)
      (character-token (location 1 13 14) (location 1 14 15) #\t)
      (character-token (location 1 14 15) (location 1 15 16) #\e)
      (character-token (location 1 15 16) (location 1 16 17) #\space)
      (character-token (location 1 16 17) (location 1 17 18) #\o)
      (character-token (location 1 17 18) (location 1 18 19) #\p)
      (character-token (location 1 18 19) (location 1 19 20) #\e)
      (character-token (location 1 19 20) (location 1 20 21) #\r)
      (character-token (location 1 20 21) (location 1 21 22) #\a)
      (character-token (location 1 21 22) (location 1 22 23) #\t)
      (character-token (location 1 22 23) (location 1 23 24) #\o)
      (character-token (location 1 23 24) (location 1 24 25) #\r)
      (character-token (location 1 24 25) (location 1 25 26) #\s)
      (character-token (location 1 25 26) (location 1 26 27) #\space)
      (character-token (location 1 26 27) (location 1 27 28) #\a)
      (character-token (location 1 27 28) (location 1 28 29) #\r)
      (character-token (location 1 28 29) (location 1 29 30) #\e)
      (character-token (location 1 29 30) (location 1 30 31) #\space)
      (character-token (location 1 30 31) (location 1 31 32) #\>)
      (character-token (location 1 31 32) (location 1 32 33) #\space)
      (character-token (location 1 32 33) (location 1 33 34) #\a)
      (character-token (location 1 33 34) (location 1 34 35) #\n)
      (character-token (location 1 34 35) (location 1 35 36) #\d)
      (character-token (location 1 35 36) (location 1 36 37) #\space)
      (character-token (location 1 37 38) (location 1 38 39) #\!)
      (character-token (location 1 36 37) (location 1 37 38) #\<))
     #f
     (list
      (character-token (location 1 38 39) (location 1 39 40) #\-)
      (character-token (location 1 39 40) (location 1 40 41) #\-))
     (character-token (location 1 40 41) (location 1 41 42) #\>))
    (character-token (location 1 41 42) (location 1 42 43) #\.))))
