#lang rosette/safe

(require (only-in rosette symbol? syntax->datum eval-syntax check-duplicates))
(require "../rim/BasicDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

; This file contains the syntax for specifying MADE information models.

; define-nominal creates a new nominal datatype.
; It requires two inputs:
; 1) An identifier for the new nominal datatype.
; 2) A list of its possible values.
;    a) Entries must be symbols.
;    b) Entries must be unique.
(define-syntax (define-nominal stx)       
  (syntax-case stx ()
    [(define-nominal id value-list)
     (let* ([eval-list (eval-syntax #'value-list)]
            [not-id? (not (identifier? #'id))]
            [not-list? (not (list? eval-list))]
            [not-symbols? (if not-list?
                              #t
                              (findf (lambda (v) (not (symbol? v))) eval-list))]
            [not-unique? (if (or not-list? not-symbols?)
                             #t
                             (check-duplicates eval-list))])

       (cond [not-id?
              (raise-syntax-error #f "identifier expected." stx #'id)]
             [not-list?
              (raise-syntax-error #f "list expected." stx #'value-list)]
             [not-symbols?
              (syntax-case #'value-list ()
                [(_ val-1 ...) (raise-symbol-error #'(val-1 ...) not-symbols? stx)])]
             [not-unique?
              (syntax-case #'value-list ()
                [(_ val-1 ...) (raise-unique-error #'(val-1 ...) not-unique? stx)])]
             [else #'(struct id nominal ()
                       #:transparent
                       #:methods gen:typed
                       [(define (get-type self) id)
                        (define (valid? self)
                          (list? (member (get-value self) value-list)))])]))]
    
    [(define-nominal val-1 ...)
     (raise-syntax-error 'define-nominal
                         "bad syntax. expected: (define-nominal id value-list).")]))

; Helper function for raising an error when creating a nominal datatype
; with a non-symbol value.
(define-for-syntax (raise-symbol-error stx target src-stx)
    (syntax-case stx ()
      [(val-1)
       (cond [(eq? target (eval-syntax #'val-1))
              (raise-syntax-error #f "symbol expected." src-stx #'val-1)
              (raise-syntax-error 'define-nominal "unknown error.")])]
      
      [(val-1 val-2 ...)
       (cond [(eq? target (eval-syntax #'val-1))
              (raise-syntax-error #f "symbol expected." src-stx #'val-1)]
             [else
              (raise-symbol-error #'(val-2 ...) target src-stx)])]))

; Helper function for raising an error when creating a nominal datatype
; with non-unique values.
(define-for-syntax (raise-unique-error stx target src-stx)
    (syntax-case stx ()
      [(val-1)
       (cond [(eq? target (eval-syntax #'val-1))
              (raise-syntax-error #f "duplicate symbol." src-stx #'val-1)
              (raise-syntax-error 'define-nominal "unknown error.")])]
      
      [(val-1 val-2 ...)
       (cond [(eq? target (eval-syntax #'val-1))
              (raise-syntax-error #f "duplicate symbol." src-stx #'val-1)]
             [else
              (raise-unique-error #'(val-2 ...) target src-stx)])]))
