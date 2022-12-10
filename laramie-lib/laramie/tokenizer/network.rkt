#lang typed/racket/base/optional

(provide make-validating-input-port)

(require racket/port
         (file "infrastructure.rkt")
         (file "parameters.rkt"))

(module+ test
  (require typed/rackunit))

(: initial-byte? (-> Byte Boolean))
(define (initial-byte? b)
  (and (not (= b #xc0))
       (not (= b #xc1))
       (not (<= #xf5 b #xff))
       (<= #x00 b #x7f)))

(: continuation-byte? (-> Byte Boolean))
(define (continuation-byte? b)
  (<= #x80 b #xbf))

#|

Taken from https://stackoverflow.com/questions/6555015/check-for-invalid-utf8

Code Points        First Byte Second Byte Third Byte Fourth Byte
U+0000..U+007F     00..7F
U+0080..U+07FF     C2..DF     80..BF
U+0800..U+0FFF     E0         A0..BF      80..BF
U+1000..U+CFFF     E1..EC     80..BF      80..BF
U+D000..U+D7FF     ED         80..9F      80..BF
U+E000..U+FFFF     EE..EF     80..BF      80..BF
U+10000..U+3FFFF   F0         90..BF      80..BF     80..BF
U+40000..U+FFFFF   F1..F3     80..BF      80..BF     80..BF
U+100000..U+10FFFF F4         80..8F      80..BF     80..BF

|#

(: valid-utf-8-bytes/1? (-> Byte Boolean))
(define (valid-utf-8-bytes/1? b)
  (initial-byte? b))

(: valid-utf-8-bytes/2? (-> Byte Byte Boolean))
(define (valid-utf-8-bytes/2? b1 b2)
  (and (<= #xc2 b1 #xdf)
       (continuation-byte? b2)))

(: valid-utf-8-bytes/3? (-> Byte Byte Byte Boolean))
(define (valid-utf-8-bytes/3? b1 b2 b3)
  (or (and (= b1 #xe0)
           (<= #xa0 b2 #xbf)
           (continuation-byte? b3))
      (and (<= #xe1 b1 #xec)
           (continuation-byte? b2)
           (continuation-byte? b3))
      (and (= b1 #xed)
           (<= #x80 b2 #x9f)
           (continuation-byte? b3))
      (and (<= #xee b1 #xef)
           (continuation-byte? b2)
           (continuation-byte? b3))))

(: valid-utf-8-bytes/4? (-> Byte Byte Byte Byte Boolean))
(define (valid-utf-8-bytes/4? b1 b2 b3 b4)
  (or (and (= b1 #xf0)
           (<= #x90 b2 #xbf)
           (continuation-byte? b3)
           (continuation-byte? b4))
      (and (<= #xf1 b1 #xf3)
           (continuation-byte? b2)
           (continuation-byte? b3)
           (continuation-byte? b4))
      (and (= b1 #xf4)
           (continuation-byte? b2)
           (continuation-byte? b3)
           (continuation-byte? b4))))

(module+ test
  (test-case "Initial byte Cf invalid0"
    (check-false (valid-utf-8-bytes/1? #xc0)))
  (test-case "Initial byte C1 invalid"
    (check-false (valid-utf-8-bytes/1? #xc1)))
  (test-case "Initial byte E0 followed by 80 invalid"
    (check-false (valid-utf-8-bytes/2? #xe0 #x80)))
  (test-case "Initial byte E0 followed by 9F invalid"
    (check-false (valid-utf-8-bytes/2? #xe0 #x9f)))
  (test-case "Initial byte F0 followed by 80 invalid"
    (check-false (valid-utf-8-bytes/2? #xf0 #x80)))
  (test-case "Initial byte F0 followed by 8F invalid"
    (check-false (valid-utf-8-bytes/2? #xf0 #x8f))))

#|


In addition to validating UTF characters, we also need to
check for "non-character" characters.

In addition, if we encounter two consecutive whitespace
characters -- possibly with malformed bytes or
non-characters in between -- we need to drop them.

Thus, the peek function needs to know whether the previously
read character was whitespace. If so, then we need to keep
peeking ahead.

|#

(: do-it (-> (Listof Byte)
             Exact-Nonnegative-Integer
             Exact-Nonnegative-Integer))
(define (do-it bs exponent)
  (cond [(null? bs)
         0]
        [else
         (+ (* (car bs) (expt 8 exponent))
            (do-it (cdr bs) (add1 exponent)))]))

(require/typed
    racket/port
  [filter-read-input-port (-> Input-Port
                              (-> Bytes
                                  (U (-> (U Exact-Positive-Integer False)
                                         (U Exact-Nonnegative-Integer False)
                                         (U Exact-Positive-Integer False)
                                         (U Exact-Nonnegative-Integer False)
                                         Any)
                                     (Evtof Zero)
                                     EOF
                                     Exact-Nonnegative-Integer)
                                  (U (-> (U Exact-Positive-Integer False)
                                         (U Exact-Nonnegative-Integer False)
                                         (U Exact-Positive-Integer False)
                                         (U Exact-Nonnegative-Integer False)
                                         Any)
                                     (Evtof Zero)
                                     EOF
                                     Exact-Nonnegative-Integer))
                              (-> Bytes
                                  Exact-Nonnegative-Integer
                                  (Option (Evtof Zero))
                                  (Option
                                   (U (-> (U Exact-Positive-Integer False)
                                          (U Exact-Nonnegative-Integer False)
                                          (U Exact-Positive-Integer False)
                                          (U Exact-Nonnegative-Integer False)
                                          Any)
                                      (Evtof Zero)
                                      EOF
                                      Exact-Nonnegative-Integer))
                                  (Option
                                   (U (-> (U Exact-Positive-Integer False)
                                          (U Exact-Nonnegative-Integer False)
                                          (U Exact-Positive-Integer False)
                                          (U Exact-Nonnegative-Integer False)
                                          Any)
                                      (Evtof Zero)
                                      EOF
                                      Exact-Nonnegative-Integer)))
                              Input-Port)])

(: make-validating-input-port (-> Input-Port Input-Port))
(define (make-validating-input-port in)
  (: previous-char-was-whitespace? Boolean)
  (define previous-char-was-whitespace? #f)
  (: read-wrap (-> Bytes
                   (U (-> (U Exact-Positive-Integer False)
                          (U Exact-Nonnegative-Integer False)
                          (U Exact-Positive-Integer False)
                          (U Exact-Nonnegative-Integer False)
                          Any)
                      (Evtof Zero)
                      EOF
                      Exact-Nonnegative-Integer)
                   (U (-> (U Exact-Positive-Integer False)
                          (U Exact-Nonnegative-Integer False)
                          (U Exact-Positive-Integer False)
                          (U Exact-Nonnegative-Integer False)
                          Any)
                      (Evtof Zero)
                      EOF
                      Exact-Nonnegative-Integer)))
  (define (read-wrap bstr whatever)
    (define bl (bytes->list bstr))
    (: white? Boolean)
    (define white? (and (not (null? bl))
                        (whitespace-codepoint? (car bl))))
    (cond [(and previous-char-was-whitespace? white?)
           0]
          [else
           (set! previous-char-was-whitespace? white?)
           whatever]))
  (: peek-wrap (-> Bytes
                   Exact-Nonnegative-Integer
                   (Option (Evtof Zero))
                   (Option
                    (U (-> (U Exact-Positive-Integer False)
                           (U Exact-Nonnegative-Integer False)
                           (U Exact-Positive-Integer False)
                           (U Exact-Nonnegative-Integer False)
                           Any)
                       (Evtof Zero)
                       EOF
                       Exact-Nonnegative-Integer))
                   (Option
                    (U (-> (U Exact-Positive-Integer False)
                           (U Exact-Nonnegative-Integer False)
                           (U Exact-Positive-Integer False)
                           (U Exact-Nonnegative-Integer False)
                           Any)
                       (Evtof Zero)
                       EOF
                       Exact-Nonnegative-Integer))))
  (define (peek-wrap bstr skip evt whatever)
    whatever)
  (define filtered-in (filter-read-input-port
                       in
                       read-wrap
                       peek-wrap))
  (define reencoded (reencode-input-port filtered-in
                                         "UTF-8-permissive"
                                         #f
                                         #f
                                         (object-name filtered-in)
                                         #t ; <-- convert-newlines?
                                         ))
  (port-count-lines! reencoded)
  reencoded)

(module+ test
  (: one-by-one (-> (Listof Char)))
  (define (one-by-one)
    (define c (peek-char))
    (cond [(eof-object? c) (list)]
          [else (read-char)
                (cons c (one-by-one))]))
  (: check-string (-> String (Listof Char)))
  (define (check-string s)
    (parameterize ([current-input-port (make-validating-input-port (open-input-string s))])
      (one-by-one))))

(module+ test
  (check-equal? (check-string (format "x~a~ay  z" #\return #\newline))
                '(#\x #\newline #\y #\space #\z)))
