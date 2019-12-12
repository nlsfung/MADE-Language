#lang rosette/safe

(require (only-in rosette
                  identifier?
                  raise-syntax-error
                  syntax->datum))

(provide raise-if-not-identifier
         raise-if-not-symbol
         raise-if-not-unique)

; This file contains some common functions that are useful in specifying the
; syntax for creating concrete MADE models.

; Helper function for raising syntax error if input is not an identifier or a
; list of identifiers. Returns the syntax object for void if the check succeeds.
(define (raise-if-not-identifier stx src-stx)
  (syntax-case stx ()
    [(id-1 id-2 ...)
     (begin
       (raise-if-not-identifier #'id-1 src-stx)
       (raise-if-not-identifier #'(id-2 ...) src-stx))]
    [(id)
     (raise-if-not-identifier #'id src-stx)]
    [id
     (if (not (identifier? #'id))
         (raise-syntax-error #f "identifier expected." src-stx #'id)
         #'(void))]))

; Helper function for raising syntax error if input is not a symbol or a
; list of symbols. Returns the syntax object for void if the check succeeds.
(define (raise-if-not-symbol stx src-stx)
  (syntax-case stx ()
    [((q id))
     (raise-if-not-symbol #'(q id) src-stx)]
    [((q id) (... sym-2) ...)
     (begin
       (raise-if-not-symbol #'(q id) src-stx)
       (raise-if-not-symbol #'(sym-2 ...) src-stx))]
    [(q id)
     (if (not (and (eq? (syntax->datum #'q) 'quote)
                   (identifier? #'id)))
         (raise-syntax-error #f "symbol expected." src-stx #'id)
         #'(void))]))

; Helper function for raising syntax error if input is not a list of unique
; symbols. Returns the syntax object for void if the check succeeds.
(define (raise-if-not-unique stx stx-list src-stx)
  (syntax-case stx ()
    [((q id))
     (raise-if-not-unique #'(q id) stx-list src-stx)]
    [((q id) (... sym-2) ...)
     (begin
       (raise-if-not-unique #'(q id) stx-list src-stx)
       (raise-if-not-unique #'(sym-2 ...)
                            (append stx-list (list (syntax->datum #'(q id))))
                            src-stx))]
    [(q id)
     (begin
       (if (member (syntax->datum #'(q id)) stx-list)
           (raise-syntax-error #f "unique symbols expected." src-stx #'id)
           #'(void)))]))