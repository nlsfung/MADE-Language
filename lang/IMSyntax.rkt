#lang rosette/safe

(require (only-in rosette symbol? syntax->datum eval-syntax check-duplicates raise-argument-error))
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
              (raise-syntax-error #f "list of symbols expected." stx #'value-list)]
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

; define-enumerated creates a new enumerated datatype.
; It requires two inputs:
; 1) An identifier for the new enumerated datatype.
; 2) A list of its possible values, ordered from minimum to maximum.
;    a) Entries must be symbols.
;    b) Entries must be unique.
(define-syntax (define-enumerated stx)       
  (syntax-case stx ()
    [(define-enumerated id value-list)
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
              (raise-syntax-error #f "list of symbols expected." stx #'value-list)]
             [not-symbols?
              (syntax-case #'value-list ()
                [(_ val-1 ...) (raise-symbol-error #'(val-1 ...) not-symbols? stx)])]
             [not-unique?
              (syntax-case #'value-list ()
                [(_ val-1 ...) (raise-unique-error #'(val-1 ...) not-unique? stx)])]
             
             [else #'(struct id enumerated ()
                       #:transparent
                       #:methods gen:typed
                       [(define (get-type self) id)
                        (define (valid? self)
                          (list? (member (get-value self) value-list)))]
                       #:methods gen:enum
                       [(define (check-comparability self elem op-sym)
                          (cond [(not (enumerated? elem))
                                 (raise-argument-error op-sym "enumerated?" elem)]
                                [(not (eq? (get-type self) (get-type elem)))
                                 (raise-argument-error op-sym (format "~a?" (quote id)) elem)]
                                [(not (valid? self))
                                 (raise-argument-error op-sym (format "member of ~a" (quote value-list)) self)]
                                [(not (valid? elem))
                                 (raise-argument-error op-sym (format "member of ~a" (quote value-list)) elem)]))

                        (define (enum>? self elem)
                          (check-comparability self elem 'enum>?)
                          (< (length (member (get-value self) value-list))
                             (length (member (get-value elem) value-list))))

                        (define (enum<? self elem)
                          (check-comparability self elem 'enum<?)
                          (> (length (member (get-value self) value-list))
                             (length (member (get-value elem) value-list))))])]))]
    
    [(define-enumerated val-1 ...)
     (raise-syntax-error 'define-enumerated
                         "bad syntax. expected: (define-enumerated id value-list).")]))

; Helper function for raising an error when creating a nominal or enumerated 
; datatype with a non-symbol value.
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

; Helper function for raising an error when creating a nominal or enumerated
; datatype with non-unique values.
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
