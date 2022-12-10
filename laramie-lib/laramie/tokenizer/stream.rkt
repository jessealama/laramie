#lang typed/racket/base/optional

(provide peek-several
         read-several
         add-to-location
         subtract-from-location
         sum-of-character-lengths
         peek
         get-current-location
         with-location-increased-by)

(require racket/port
         racket/list
         syntax/parse/define
         (file "types.rkt")
         (file "predicates.rkt")
         (prefix-in params: (file "parameters.rkt"))
         (file "infrastructure.rkt"))

(module+ test
  (require typed/rackunit))

(: make-character-token (->* (Char)
                             (#:start location)
                             character-token))
(define (make-character-token c
                              #:start [start (params:current-location)])
  (character-token start
                   (add-to-location start c)
                   c))

(: byte-valid/initial? (-> Byte Boolean))
(define (byte-valid/initial? b)
  (and (not (= b #xc0))
       (not (= b #xc1))
       (not (<= #xf5 b #xff))
       (<= #x00 b #x7f)))

(: byte-valid/continuation? (-> Byte Boolean))
(define (byte-valid/continuation? b)
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

; Looks like 3 bytes, but it didn't work:
; first byte E0, second byte in A0..BF, third byte

(: invalid-bytes/1? (-> Byte Boolean))
(define (invalid-bytes/1? b)
  (or (<= #x80 b #xc1)
      (<= #xf5 b #xff)))

(: invalid-bytes/2? (-> Byte Byte
                        Boolean))
(define (invalid-bytes/2? b1 b2)
  (and (<= #xc2 b1 #xdf)
       (not (<= #xb0 b2 #xbf))))

#|
U+0800..U+0FFF     E0         A0..BF      80..BF
U+1000..U+CFFF     E1..EC     80..BF      80..BF
U+D000..U+D7FF     ED         80..9F      80..BF
U+E000..U+FFFF     EE..EF     80..BF      80..BF
|#
(: invalid-bytes/3? (-> Byte Byte Byte
                        Boolean))
(define (invalid-bytes/3? b1 b2 b3)
  (or (and (= b1 #xe0)
           (<= #xa0 b2 #xbf)
           (not (<= #x80 b3 #xbf)))
      (and (<= #xe1 b1 #xec)
           (<= #x80 b2 #xbf)
           (not (<= #x80 b3 #xbf)))
      (and (= b1 #xed)
           (<= #x80 b2 #x9f)
           (not (<= #x80 b3 #xbf)))
      (and (<= #xee b1 #xef)
           (<= #x80 b2 #xbf)
           (not (<= #x80 b3 #xbf)))))

#|
U+10000..U+3FFFF   F0         90..BF      80..BF     80..BF
U+40000..U+FFFFF   F1..F3     80..BF      80..BF     80..BF
U+100000..U+10FFFF F4         80..8F      80..BF     80..BF
|#
(: invalid-bytes/4? (-> Byte Byte Byte Byte
                        Boolean))
(define (invalid-bytes/4? b1 b2 b3 b4)
  (or (and (= b1 #xf0)
           (<= #x90 b2 #xbf)
           (<= #x80 b3 #xbf)
           (not (<= #x80 b4 #xbf)))
      (and (<= #xf1 b1 #xf3)
           (<= #x80 b2 #xbf)
           (<= #x80 b3 #xbf)
           (not (<= #x80 b4 #xbf)))
      (and (= b1 #xf4)
           (<= #x80 b2 #x8f)
           (<= #x80 b3 #xbf)
           (not (<= #x80 b4 #xbf)))))

(: valid-utf-8-bytes/1? (-> Byte Boolean))
(define (valid-utf-8-bytes/1? b)
  (byte-valid/initial? b))

(: valid-utf-8-bytes/2? (-> Byte Byte Boolean))
(define (valid-utf-8-bytes/2? b1 b2)
  (and (<= #xc2 b1 #xdf)
       (byte-valid/continuation? b2)))

(: valid-utf-8-bytes/3? (-> Byte Byte Byte Boolean))
(define (valid-utf-8-bytes/3? b1 b2 b3)
  (or (and (= b1 #xe0) (<= #xa0 b2 #xbf) (byte-valid/continuation? b3))
      (and (<= #xe1 b1 #xec) (byte-valid/continuation? b2) (byte-valid/continuation? b3))
      (and (= b1 #xed) (<= #x80 b2 #x9f) (byte-valid/continuation? b3))
      (and (<= #xee b1 #xef) (byte-valid/continuation? b2) (byte-valid/continuation? b3))))

(: valid-utf-8-bytes/4? (-> Byte Byte Byte Byte Boolean))
(define (valid-utf-8-bytes/4? b1 b2 b3 b4)
  (or (and (= b1 #xf0) (<= #x90 b2 #xbf) (byte-valid/continuation? b3) (byte-valid/continuation? b4))
      (and (<= #xf1 b1 #xf3) (byte-valid/continuation? b2) (byte-valid/continuation? b3) (byte-valid/continuation? b4))
      (and (= b1 #xf4) (byte-valid/continuation? b2) (byte-valid/continuation? b3) (byte-valid/continuation? b4))))

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

(: peek-bytes (-> Input-Port
                  Exact-Nonnegative-Integer
                  Exact-Nonnegative-Integer
                  (Listof Byte)))
(define (peek-bytes in num-bytes offset)
  (cond [(zero? num-bytes)
         (list)]
        [else
         (define b (peek-byte in offset))
         (cond [(eof-object? b)
                (list)]
               [else
                (cons b (peek-bytes in (sub1 num-bytes) (add1 offset)))])]))

#;
(: peek-several (->* (Exact-Nonnegative-Integer)
                     ((Option location)
                      (Option Input-Port))
                     (Listof character-token)))
#;
(define (peek-several n [loc #f] [in #f])
  (define p (or in (current-input-port)))
  (: keep-peeking (-> Exact-Nonnegative-Integer
                      location
                      (Listof character-token)
                      (Listof character-token)))
  (define (keep-peeking num-remaining loc chars)
    (cond [(zero? num-remaining)
           (reverse chars)]
          [else
           (define offset (location-position loc))
           (define bs (peek-bytes p 4 offset))
           (cond [(null? bs)
                  (reverse chars)]
                 [(null? (cdr bs)) ; only 1 byte is available
                  (cond [(valid-utf-8-bytes/1? (car bs))
                         (define c (integer->char (car bs)))
                         (keep-peeking (sub1 num-remaining)
                                       (add-to-location loc c)
                                       (cons (make-character-token c #:start loc) chars))]
                        [else
                         (keep-peeking num-remaining
                                       (add-to-location loc 1)
                                       chars)])]
                 [(null? (cddr bs)) ; only 2 bytes available
                  (cond [(valid-utf-8-bytes/1? (car bs))
                         (define c (integer->char (car bs)))
                         (keep-peeking (sub1 num-remaining)
                                       (add-to-location loc c)
                                       (cons (make-character-token c #:start loc) chars))]
                        [(invalid-bytes/1? (car bs))
                         (keep-peeking num-remaining
                                       (add-to-location loc 1)
                                       chars)]
                        [(valid-utf-8-bytes/2? (car bs) (cadr bs))
                         (define c (integer->char (+ (cadr bs)
                                                     (* 256 (car bs)))))
                         (keep-peeking (sub1 num-remaining)
                                       (add-to-location loc c)
                                       (cons (make-character-token c #:start loc) chars))]
                        [else
                         (keep-peeking num-remaining
                                       (add-to-location loc 2)
                                       chars)])]
                 [(null? (cdddr bs)) ; only 3 bytes available
                  (cond [(valid-utf-8-bytes/1? (car bs))
                         (define c (integer->char (car bs)))
                         (keep-peeking (sub1 num-remaining)
                                       (add-to-location loc c)
                                       (cons (make-character-token c #:start loc) chars))]
                        [(invalid-bytes/1? (car bs))
                         (keep-peeking num-remaining
                                       (add-to-location loc 1)
                                       chars)]
                        [(valid-utf-8-bytes/2? (car bs) (cadr bs))
                         (define c (integer->char (+ (cadr bs)
                                                     (* 256 (car bs)))))
                         (keep-peeking (sub1 num-remaining)
                                       (add-to-location loc c)
                                       (cons (make-character-token c #:start loc) chars))]
                        [(invalid-bytes/2? (car bs) (cadr bs))
                         (keep-peeking num-remaining
                                       (add-to-location loc 2)
                                       chars)]
                        [(valid-utf-8-bytes/3? (car bs) (cadr bs) (caddr bs))
                         (define c (integer->char (+ (caddr bs)
                                                     (* 256 (cadr bs))
                                                     (* 256 256 (car bs)))))
                         (keep-peeking (sub1 num-remaining)
                                       (add-to-location loc c)
                                       (cons (make-character-token c #:start loc) chars))]
                        [else
                         (keep-peeking num-remaining
                                       (add-to-location loc 3)
                                       chars)])]
                 ; we have 4 bytes
                 [(valid-utf-8-bytes/1? (car bs))
                  (define c (integer->char (car bs)))
                  (keep-peeking (sub1 num-remaining)
                                (add-to-location loc c)
                                (cons (make-character-token c #:start loc) chars))]
                 [(invalid-bytes/1? (car bs))
                  (keep-peeking num-remaining
                                (add-to-location loc 1)
                                chars)]
                 [(valid-utf-8-bytes/2? (car bs) (cadr bs))
                  (define c (integer->char (+ (cadr bs)
                                              (* 256 (car bs)))))
                  (keep-peeking (sub1 num-remaining)
                                (add-to-location loc c)
                                (cons (make-character-token c #:start loc) chars))]
                 [(invalid-bytes/2? (car bs) (cadr bs))
                  (keep-peeking num-remaining
                                (add-to-location loc 2)
                                chars)]
                 [(valid-utf-8-bytes/3? (car bs) (cadr bs) (caddr bs))
                  (define c (integer->char (+ (caddr bs)
                                              (* 256 (cadr bs))
                                              (* 256 256 (car bs)))))
                  (keep-peeking (sub1 num-remaining)
                                (add-to-location loc c)
                                (cons (make-character-token c #:start loc) chars))]
                 [(invalid-bytes/3? (car bs) (cadr bs) (caddr bs))
                  (keep-peeking num-remaining
                                (add-to-location loc 3)
                                chars)]
                 [(valid-utf-8-bytes/4? (car bs) (cadr bs) (caddr bs) (cadddr bs))
                  (define c (integer->char (+ (cadddr bs)
                                              (* 256 (caddr bs))
                                              (* 256 256 (cadr bs))
                                              (* 256 256 256 (car bs)))))
                  (keep-peeking (sub1 num-remaining)
                                (add-to-location loc c)
                                (cons (make-character-token c #:start loc) chars))]
                 [else
                  (keep-peeking num-remaining
                                (add-to-location loc 4)
                                chars)])]))
  (keep-peeking n
                (or loc (params:current-location))
                (list)))

(: peek-several (->* (Exact-Nonnegative-Integer)
                     (Exact-Nonnegative-Integer
                      Input-Port)
                     (Listof Char)))
(define (peek-several n [offset 0] [in (current-input-port)])
  (define p (or in (current-input-port)))
  (: keep-peeking (-> Exact-Nonnegative-Integer
                      Exact-Nonnegative-Integer
                      (Listof Char)
                      (Listof Char)))
  (define (keep-peeking num-remaining offset chars)
    (cond [(zero? num-remaining)
           (reverse chars)]
          [else
           (define c (peek-char p offset))
           (cond [(eof-object? c)
                  (reverse chars)]
                 [else
                  (keep-peeking (sub1 num-remaining)
                                (+ offset (char-utf-8-length c))
                                (cons c chars))])]))
  (keep-peeking n
                offset
                (list)))

(: read-several (->* (Exact-Nonnegative-Integer)
                     ((Option Input-Port))
                     (Listof character-token)))
(define (read-several l [in #f])
  (define p (or in (current-input-port)))
  (: result (Listof (U character-token EOF)))
  (define result (for/list ([i (in-range l)])
                   (define c (peek-char p))
                   (begin0
                       (cond [(char? c)
                              (make-character-token c #:start (get-current-location))]
                             [else
                              eof])
                     (read-char))))
  (filter character-token? result))

(: add-to-location (-> location
                       (U Char
                          String
                          Exact-Nonnegative-Integer
                          (Listof (U Char String)))
                       location))
(define (add-to-location loc chars)
  (cond [(char? chars)
         (add-to-location loc (list chars))]
        [(string? chars)
         (add-to-location loc (string->list chars))]
        [(integer? chars)
         (struct-copy location
                      loc
                      [position (+ chars (location-position loc))])]
        [(null? chars)
         loc]
        [(string? (car chars))
         (add-to-location loc (append (string->list (car chars))
                                      (cdr chars)))]
        [(char=? (car chars) #\newline)
         (define new-loc (struct-copy location
                                      loc
                                      [line (add1 (location-line loc))]
                                      [column 0]
                                      [position (+ (location-position loc)
                                                   (char-utf-8-length #\newline))]))
         (add-to-location new-loc (cdr chars))]
        [else
         (define new-loc (struct-copy location
                                      loc
                                      [column (add1 (location-column loc))]
                                      [position (+ (location-position loc)
                                                   (char-utf-8-length (car chars)))]))
         (add-to-location new-loc (cdr chars))]))

(: subtract-from-location (-> location
                              Char
                              location))
(define (subtract-from-location loc c)
  (define line (location-line loc))
  (define col (location-column loc))
  (define pos (location-position loc))
  (define num-bytes (char-utf-8-length c))
  (define new-pos (max 0 (- pos num-bytes)))
  (define new-line (cond [(char=? c #\newline)
                          (max 1 (sub1 line))]
                         [else
                          line]))
  (define new-col (cond [(char=? c #\newline)
                         0]
                        [else
                         (max 0 (sub1 col))]))
  (struct-copy location
               loc
               [position new-pos]
               [line new-line]
               [column new-col]))

(: sum-of-character-lengths (-> (Listof Char)
                                Exact-Nonnegative-Integer))
(define (sum-of-character-lengths chars)
  (cond [(null? chars) 0]
        [else (+ (char-utf-8-length (car chars))
                 (sum-of-character-lengths (cdr chars)))]))

(: peek (->* ()
             (Exact-Nonnegative-Integer
              Input-Port)
             (U EOF Char)))
(define (peek [offset 0]
              [in (current-input-port)])
  (define peeked (peek-several 1 offset in))
  (cond [(null? peeked) eof]
        [else (car peeked)]))

(: get-current-location (->* ()
                             ((Option Input-Port))
                             location))
(define (get-current-location [in #f])
  (define-values (line col pos)
    (port-next-location (or in (current-input-port))))
  (when (or (eq? #f line)
            (eq? #f col)
            (eq? #f pos))
    (error "Could not get current location"))
  (location line col pos))

(define-simple-macro (with-location-increased-by c step ...)
  (parameterize ([params:current-location (add-to-location (params:current-location) c)])
    step ...))
