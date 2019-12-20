#lang rosette/safe

(require (only-in rosette
                  identifier?
                  raise-syntax-error
                  syntax->datum
                  datum->syntax
                  string->symbol
                  symbol->string
                  string-append))

(provide build-getter-name
         symbol->identifier
         raise-if-not-identifier
         raise-if-not-symbol
         raise-if-not-unique
         raise-if-not-lambda
         raise-if-not-datetime
         raise-if-not-integer)

; This file contains some common functions that are useful in specifying the
; syntax for creating concrete MADE models.

; Helper function for creating a "get-x" syntax object that will become the name
; of the function for getting a symbolic instance of "x".
(define (build-getter-name id)
  (datum->syntax id
                 (string->symbol
                  (string-append "get-"
                                 (symbol->string (syntax->datum id))))
                 id))

; Helper function for removing the quotes from a symbol or a list of symbols.
; Returns the syntax objects for the resulting identifiers.
(define (symbol->identifier stx)
  (syntax-case stx ()
    [((q id))
     (list (symbol->identifier #'(q id)))]
    [((q id) (... sym-2) ...)
     (append (symbol->identifier #'((q id)))
             (symbol->identifier #'(sym-2 ...)))]
    [(q id) #'id]
    [() #'()]))

; Helper function for raising syntax error if input is not an identifier or a
; list of identifiers. Returns the syntax object for void if the check succeeds.
(define (raise-if-not-identifier stx src-stx)
  (syntax-case stx ()
    [(id)
     (raise-if-not-identifier #'id src-stx)]
    [(id-1 id-2 ...)
     (begin
       (raise-if-not-identifier #'id-1 src-stx)
       (raise-if-not-identifier #'(id-2 ...) src-stx))]
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

; Helper function for raising syntax error if input is not an integer or a
; list of integers. Returns the syntax object for void if the check succeeds.
(define (raise-if-not-integer stx src-stx)
  (syntax-case stx ()
    [(val)
     (raise-if-not-integer #'val src-stx)]
    [(val-1 val-2 ...)
     (begin
       (raise-if-not-integer #'val-1 src-stx)
       (raise-if-not-integer #'(val-2 ...) src-stx))]
    [val
     (if (not (integer? (syntax->datum #'val)))
         (raise-syntax-error #f "integer expected." src-stx #'val)
         #'(void))]))

; Helper function for raising syntax error if input is not a lambda expression
; or a list of lambda expressions of the specified arity. If the arity is not
; an integer, then it is ignored.
; Returns the syntax object for void if the check succeeds.
(define (raise-if-not-lambda stx arity src-stx)
  (syntax-case stx ()
    [((lambda (... arg) rest ...))
     (raise-if-not-lambda #'(lambda (... arg) rest ...) arity src-stx)]
    [((lambda (... arg) rest ...) (... lambda-2) ...)
     (begin
       (raise-if-not-lambda #'(lambda (... arg) rest ...) arity src-stx)
       (raise-if-not-lambda #'(lambda-2 ...) arity src-stx))]
    [(lambda (... arg) rest ...)
     (cond [(not (eq? (syntax->datum #'lambda) 'lambda))
            (raise-syntax-error #f "lambda expected." src-stx #'lambda)]
           [(not (implies (integer? arity)
                          (eq? (length (syntax->datum #'arg)) arity)))
            (raise-syntax-error #f
                                (format "arity of ~a expected." arity)
                                src-stx
                                #'arg)]
           [else (begin
                   (raise-if-not-identifier #'arg src-stx)
                   #'(void))])]))

; Helper function for raising syntax error if input is not a datetime or a list
; of datetime values. Returns the syntax object for void if the check succeeds.
(define (raise-if-not-datetime stx src-stx)
  (syntax-case stx ()
    [((datetime int-1 int-2 int-3 int-4 int-5 int-6))
     (raise-if-not-datetime #'(datetime int-1 int-2 int-3 int-4 int-5 int-6) src-stx)]
    [((datetime int-1 int-2 int-3 int-4 int-5 int-6) (... datetime-2) ...)
     (begin
       (raise-if-not-datetime #'(datetime int-1 int-2 int-3 int-4 int-5 int-6) src-stx)
       (raise-if-not-datetime #'(datetime-2 ...) src-stx))]
    [(datetime int-1 int-2 int-3 int-4 int-5 int-6)
     (cond [(not (eq? (syntax->datum #'datetime) 'datetime))
            (raise-syntax-error #f "datetime expected." src-stx #'datetime)]
           [else (begin (raise-if-not-integer #'(int-1 int-2 int-3 int-4 int-5 int-6) src-stx)
                        #'(void))])]))
