#lang typed/racket/base/optional

(provide tokenize
         CDATA
         SCRIPT
         RCDATA
         RAWTEXT
         PLAINTEXT

         make-validating-input-port
         enumerate-input-characters

         ; structs
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

(require "tokenize.rkt"
         "tokens.rkt"
         "cdata.rkt"
         "comment.rkt"
         "data.rkt"
         "doctype.rkt"
         "plaintext.rkt"
         "rawtext.rkt"
         "rcdata.rkt"
         "script.rkt"
         "types.rkt"
         "network.rkt")
