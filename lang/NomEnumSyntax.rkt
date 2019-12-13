#lang rosette/safe

(require (only-in rosette raise-argument-error))
(require (for-syntax "./SyntaxUtil.rkt"))
(require "../rim/BasicDataTypes.rkt")

(provide define-nominal
         define-enumerated)

; This file contains the syntax for specifying new nominal and enumerated datatypes.

; define-nominal creates a new nominal datatype.
; It requires two inputs:
; 1) An identifier for the new nominal datatype.
; 2) A list of its possible values.
;    a) Entries must be symbols.
;    b) Entries must be unique.
(define (get-nom-val) (define-symbolic* nom-val integer?) nom-val)
(define-syntax (define-nominal stx)       
  (syntax-case stx ()
    [(define-nominal id val ...)
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-symbol #'(val ...) stx)
       (raise-if-not-unique #'(val ...) null stx)
       (with-syntax ([get-id (build-getter-name #'id)])
         #'(begin
             (struct id nominal ()
               #:transparent
               #:methods gen:typed
               [(define (get-type self) id)
                (define (valid? self)
                  (list? (member (get-value self) (list val ...))))])
             (define get-id
               (lambda ()
                 (id (list-ref (list val ...) (get-nom-val))))))))]))

; define-enumerated creates a new enumerated datatype.
; It requires two inputs:
; 1) An identifier for the new enumerated datatype.
; 2) A list of its possible values, ordered from minimum to maximum.
;    a) Entries must be symbols.
;    b) Entries must be unique.
(define (get-enum-val) (define-symbolic* enum-val integer?) enum-val)
(define-syntax (define-enumerated stx)       
  (syntax-case stx ()
    [(define-enumerated id val ...)
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-symbol #'(val ...) stx)
       (raise-if-not-unique #'(val ...) null stx)
       (with-syntax ([get-id (build-getter-name #'id)]) 
         #'(begin
             (struct id enumerated ()
               #:transparent
               #:methods gen:typed
               [(define (get-type self) id)
                (define (valid? self)
                  (list? (member (get-value self) (list val ...))))]
               #:methods gen:enum
               [(define (check-comparability self elem op-sym)
                  (cond [(not (enumerated? elem))
                         (raise-argument-error op-sym "enumerated?" elem)]
                        [(not (eq? (get-type self) (get-type elem)))
                         (raise-argument-error op-sym
                                               (format "~a?" (quote id))
                                               elem)]
                        [(not (valid? self))
                         (raise-argument-error op-sym
                                               (format "member of ~a" (list val ...))
                                               self)]
                        [(not (valid? elem))
                         (raise-argument-error op-sym
                                               (format "member of ~a" (list val ...))
                                               elem)]))

                (define (enum>? self elem)
                  (check-comparability self elem 'enum>?)
                  (< (length (member (get-value self) (list val ...)))
                     (length (member (get-value elem) (list val ...)))))

                (define (enum<? self elem)
                  (check-comparability self elem 'enum<?)
                  (> (length (member (get-value self) (list val ...)))
                     (length (member (get-value elem) (list val ...)))))])

             (define get-id
               (lambda ()
                 (define-symbolic* enum-val integer?)
                 (id (list-ref (list val ...) (get-enum-val))))))))]))
