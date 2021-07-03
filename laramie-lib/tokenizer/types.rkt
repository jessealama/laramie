#lang typed/racket/base

(provide Token

         ; Rendering
         XExpr

         ; Characters
         ASCIIWhitespaceCodepoint
         ControlCodepoint
         NonCharacterCodepoint
         DecimalDigit
         HexadecimalDigit
         ASCIIAlphanumeric
         ASCIIUppercase
         ASCIILowercase
         ASCIIDigit
         HTML5Whitespace
         ZeroToNine
         ZeroToFifteen

         ; Structures
         (struct-out stream-error)
         (struct-out surrogate-in-input-stream)
         (struct-out noncharacter-in-input-stream)
         (struct-out control-character-in-input-stream)
         (struct-out malformed-bytes)
         (struct-out character-reference-token)
         (struct-out named-character-ref)
         (struct-out numeric-character-ref)
         (struct-out decimal-character-ref)
         (struct-out character-token)
         (struct-out string-token)
         (struct-out character-reference-token)
         (struct-out doctype-token)
         (struct-out start-tag-token)
         (struct-out tag-token)
         (struct-out end-tag-token)
         (struct-out comment-token)
         (struct-out attribute-token)
         (struct-out quoted-attr-value)
         (struct-out location)
         (struct-out span)
         (struct-out peek-result)
         (struct-out attr-state)
         (struct-out tokenizer)

         ; tokenizer errors
         (struct-out tokenizer-error)
         (struct-out unexpected-null-character)

         ; doctype errors
         (struct-out missing-whitespace-after-doctype-system-keyword)
         (struct-out missing-whitespace-after-doctype-public-keyword)
         (struct-out missing-doctype-system-identifier)
         (struct-out eof-in-doctype)
         (struct-out invalid-character-sequence-after-doctype-name)
         (struct-out missing-doctype-public-identifier)
         (struct-out unexpected-character-after-doctype-system-identifier)
         (struct-out abrupt-doctype-public-identifier)
         (struct-out abrupt-doctype-system-identifier)
         (struct-out missing-whitespace-between-doctype-public-and-system-identifiers)
         (struct-out missing-doctype-name)
         (struct-out missing-whitespace-before-doctype-name)
         (struct-out missing-quote-before-doctype-system-identifier)
         (struct-out missing-quote-before-doctype-public-identifier)

         ; character references
         (struct-out missing-semicolon-after-character-reference)
         (struct-out unknown-named-character-reference)
         (struct-out absence-of-digits-in-numeric-character-reference)
         (struct-out null-character-reference)
         (struct-out character-reference-outside-unicode-range)
         (struct-out surrogate-character-reference)
         (struct-out noncharacter-character-reference)
         (struct-out control-character-reference)

         ; comment
         (struct-out abrupt-closing-of-empty-comment)
         (struct-out eof-in-comment)
         (struct-out nested-comment)
         (struct-out incorrectly-closed-comment)

         ; tag
         (struct-out eof-in-tag)
         (struct-out unexpected-solidus-in-tag)
         (struct-out unexpected-equals-sign-before-attribute-name)
         (struct-out unexpected-character-in-attribute-name)
         (struct-out missing-attribute-value)
         (struct-out unexpected-character-in-unquoted-attribute-value)
         (struct-out missing-whitespace-between-attributes)
         (struct-out missing-end-tag-name)
         (struct-out invalid-first-character-of-tag-name)
         (struct-out eof-before-tag-name)

         ; script
         (struct-out eof-in-script-html-comment-like-text)

         ; cdata
         (struct-out eof-in-cdata)

         ; toplevel
         (struct-out unexpected-question-mark-instead-of-tag-name)
         (struct-out incorrectly-opened-comment))

(define-type Token (U character-token
                      character-reference-token
                      string-token
                      tag-token
                      comment-token
                      doctype-token
                      cdata-section-token
                      tokenizer-error))

(struct location
  ([line : Exact-Positive-Integer]
   [column : Exact-Nonnegative-Integer]
   [position : Exact-Nonnegative-Integer])
  #:transparent)

(struct span
  ([start : location]
   [stop : location])
  #:transparent)

(struct tokenizer-error
  ([location : location]
   [content : (Option (U character-token
                         (Listof character-token)))])
  #:transparent)

(struct missing-semicolon-after-character-reference tokenizer-error
  ()
  #:transparent)

(struct unknown-named-character-reference tokenizer-error
  ()
  #:transparent)

(struct absence-of-digits-in-numeric-character-reference tokenizer-error
  ()
  #:transparent)

(struct null-character-reference tokenizer-error
  ()
  #:transparent)

(struct character-reference-outside-unicode-range tokenizer-error
  ()
  #:transparent)

(struct surrogate-character-reference tokenizer-error
  ()
  #:transparent)

(struct noncharacter-character-reference tokenizer-error
  ()
  #:transparent)

(struct control-character-reference tokenizer-error
  ()
  #:transparent)

(struct unexpected-null-character tokenizer-error
  ()
  #:transparent)

(struct missing-whitespace-after-doctype-system-keyword tokenizer-error
  ()
  #:transparent)

(struct missing-quote-before-doctype-system-identifier tokenizer-error
  ()
  #:transparent)

(struct missing-whitespace-after-doctype-public-keyword tokenizer-error
  ()
  #:transparent)

(struct missing-doctype-system-identifier tokenizer-error
  ()
  #:transparent)

(struct invalid-character-sequence-after-doctype-name tokenizer-error
  ()
  #:transparent)

(struct eof-in-doctype tokenizer-error
  ()
  #:transparent)

(struct missing-doctype-public-identifier tokenizer-error
  ()
  #:transparent)

(struct unexpected-character-after-doctype-system-identifier tokenizer-error
  ()
  #:transparent)

(struct abrupt-doctype-public-identifier tokenizer-error
  ()
  #:transparent)

(struct abrupt-doctype-system-identifier tokenizer-error
  ()
  #:transparent)

(struct missing-whitespace-between-doctype-public-and-system-identifiers tokenizer-error
  ()
  #:transparent)

(struct missing-doctype-name tokenizer-error
  ()
  #:transparent)

(struct missing-quote-before-doctype-public-identifier tokenizer-error
  ()
  #:transparent)

(struct missing-whitespace-before-doctype-name tokenizer-error
  ()
  #:transparent)

;; comment errors

(struct abrupt-closing-of-empty-comment tokenizer-error
  ()
  #:transparent)

(struct eof-in-comment tokenizer-error
  ()
  #:transparent)

(struct nested-comment tokenizer-error
  ()
  #:transparent)

(struct incorrectly-closed-comment tokenizer-error
  ()
  #:transparent)

; tag errors

(struct eof-in-tag tokenizer-error
  ()
  #:transparent)

(struct unexpected-solidus-in-tag tokenizer-error
  ()
  #:transparent)

(struct unexpected-equals-sign-before-attribute-name tokenizer-error
  ()
  #:transparent)

(struct unexpected-character-in-attribute-name tokenizer-error
  ()
  #:transparent)

(struct missing-attribute-value tokenizer-error
  ()
  #:transparent)

(struct unexpected-character-in-unquoted-attribute-value tokenizer-error
  ()
  #:transparent)

(struct missing-whitespace-between-attributes tokenizer-error
  ()
  #:transparent)

(struct missing-end-tag-name tokenizer-error
  ()
  #:transparent)

(struct invalid-first-character-of-tag-name tokenizer-error
  ()
  #:transparent)

(struct eof-before-tag-name tokenizer-error
  ()
  #:transparent)

; script errors
(struct eof-in-script-html-comment-like-text tokenizer-error
  ()
  #:transparent)

; cdata errors
(struct eof-in-cdata tokenizer-error
  ()
  #:transparent)

; toplevel errors
(struct unexpected-question-mark-instead-of-tag-name tokenizer-error
  ()
  #:transparent)

(struct incorrectly-opened-comment tokenizer-error
  ()
  #:transparent)

;; Characters and character references

(struct character-token span
  ([content : (U Char
                 (Pairof Char (Option Char))
                 (Pairof (Listof Char) (Option Char)))])
  #:transparent)

(struct string-token span
  ([content : String])
  #:transparent)

(struct cdata-section-token span
  ([content : String])
  #:transparent)

(struct character-reference-token span
  ([characters : (Listof character-token)]
   [result : (U Char
                (List Char Char))]
   [upgraded? : Boolean])
  #:transparent)

(struct named-character-ref
  ([name : String]
   [codepoints : (U (List Char)
                    (List Char Char))]
   [use : String])
  #:transparent)

(struct numeric-character-ref
  ([pound : Char]
   [codepoint : (U Exact-Nonnegative-Integer
                   (Pairof Exact-Nonnegative-Integer
                           Exact-Nonnegative-Integer))])
  #:transparent)

(struct decimal-character-ref numeric-character-ref
  ()
  #:transparent)

(struct doctype-token span
  ([less-than : character-token]
   [bang : character-token]
   [doctype : (Listof character-token)]
   [name : (Listof character-token)]
   [public-keyword : (Option (Listof character-token))]
   [public : (Option (Listof character-token))]
   [system-keyword : (Option (Listof character-token))]
   [system : (Option (Listof character-token))]
   [greater-than : (Option character-token)]
   [quirky? : Boolean]
   [misc : (Listof character-token)])
  #:transparent)

(struct quoted-attr-value
  ([opener : (Option character-token)]
   [content : (Option (Listof (U character-token character-reference-token)))]
   [closer : (Option character-token)])
  #:transparent)

(struct attribute-token span
  ([name : (Listof character-token)]
   [equals : (Option character-token)]
   [value : (Option quoted-attr-value)])
  #:transparent)

(struct attr-state
  ([name : (Listof (U Char (Pairof Char Char)))]
   [separator : (Option Char)]
   [value : (Option (Listof (U Char (Pairof Char Char))))])
  #:transparent)

(struct tag-token span
  ([less-than : character-token]
   [name : (Listof character-token)]
   [attrs : (Listof attribute-token)]
   [self-closing-char : (Option character-token)]
   [greater-than : (Option character-token)]
   [misc : (Listof character-token)])
  #:transparent)

(struct start-tag-token tag-token ()
  #:transparent)

(struct end-tag-token tag-token
  ([slash : (Option character-token)])
  #:transparent)

(struct comment-token span
  ([less-than : character-token]
   [bang : character-token]
   [opening-dashes : (Option (List character-token character-token))]
   [content : (Listof character-token)]
   [bogus? : Boolean]
   [closing-dashes : (Option (Pairof character-token (U Null (List character-token))))]
   [greater-than : (Option character-token)])
  #:transparent)

(struct peek-result
  ([bytes : (Listof Byte)]
   [char : (Option (U EOF Char))]
   [position : location])
  #:transparent)

(struct stream-error
  ([location : (Option location)]
   [content : (U Exact-Nonnegative-Integer ; codepoint
                 (Listof Byte))])
  #:transparent)

(struct surrogate-in-input-stream stream-error
  ()
  #:transparent)

(struct noncharacter-in-input-stream stream-error
  ()
  #:transparent)

(struct control-character-in-input-stream stream-error
  ()
  #:transparent)

(struct malformed-bytes stream-error
  ()
  #:transparent)

(define-type ASCIIWhitespaceCodepoint (U #x0009 #x000a #x000c #x000d #x0020))

(define-type ControlCodepoint
  (U #x0000
     #x0001
     #x0002
     #x0003
     #x0004
     #x0005
     #x0006
     #x0007
     #x0008
     #x0009
     #x000a
     #x000b
     #x000c
     #x000d
     #x000e
     #x000f
     #x0010
     #x0012
     #x0013
     #x0014
     #x0015
     #x0016
     #x0017
     #x0018
     #x0019
     #x001a
     #x001b
     #x001c
     #x001d
     #x001e
     #x001f))

(define-type NonCharacterCodepoint
  (U #xfffe
     #xffff
     #x1fffe
     #x1ffff
     #x2fffe
     #x2ffff
     #x3fffe
     #x3ffff
     #x4fffe
     #x4ffff
     #x5fffe
     #x5ffff
     #x6fffe
     #x6ffff
     #x7fffe
     #x7ffff
     #x8fffe
     #x8ffff
     #x9fffe
     #x9ffff
     #xafffe
     #xaffff
     #xbfffe
     #xbffff
     #xcfffe
     #xcffff
     #xdfffe
     #xdffff
     #xefffe
     #xeffff
     #xffffe
     #xfffff
     #x10fffe
     #x10ffff))

(define-type ZeroToFifteen (U ZeroToNine 10 11 12 13 14 15))

(define-type ZeroToNine (U 0 1 2 3 4 5 6 7 8 9))

(define-type DecimalDigit
  (U #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define-type HexadecimalDigit
  (U ASCIIDigit
     #\a #\A
     #\b #\B
     #\c #\C
     #\d #\D
     #\e #\E
     #\f #\F))

(define-type ASCIIAlphanumeric (U ASCIIDigit ASCIIUppercase ASCIILowercase))

(define-type ASCIIUppercase
  (U #\A #\B #\C #\D #\E #\F #\G
     #\H #\I #\J #\K #\L #\M #\N
     #\O #\P #\Q #\R #\S #\T #\U
     #\V #\W #\X #\Y #\Z))

(define-type ASCIILowercase
  (U #\a #\b #\c #\d #\e #\f #\g
     #\h #\i #\j #\k #\l #\m #\n
     #\o #\p #\q #\r #\s #\t #\u
     #\v #\w #\x #\y #\z))

(define-type ASCIIDigit (U #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define-type HTML5Whitespace (U #\u0009 #\u000a #\u000c #\u000d #\u0020))

(struct tokenizer-step
  ([tokens : (U EOF
                (Listof (U Token EOF)))]
   [next : (Option (-> tokenizer-step))])
  #:transparent)

(define-type XExpr
  (U String Symbol Char
     (Pairof Symbol (Pairof (Listof (List Symbol String)) (Listof XExpr)))
     (Pairof Symbol (Listof XExpr))))

(struct tokenizer
  ([scanner : (-> (Listof (U Token tokenizer)))]))
