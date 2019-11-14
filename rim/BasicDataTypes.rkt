#lang rosette/safe

(require (only-in rosette symbol? rational? raise-argument-error))

(provide gen:typed get-type valid?)
(provide gen:basic basic? get-value)
(provide (struct-out nominal))
(provide (struct-out enumerated) gen:enum enum>? enum<?)
(provide (struct-out dimensioned))
(provide (struct-out bool))

; The complete MADE reference information model (RIM) is constructed from 10
; primitive data types: Id, Boolean, Nominal, Enumerated, Count, Proportion,
; Dimensioned, DateTime, Duration and Schedule. 

; This file contains the specification of the first 7 data types. The last 3
; (DateTime, Duration and Schedule) are specified in TemporalDataTypes.rkt.

; To enable support for type-checking (in the future), all primitive datatypes
; inherit the typed interface with two methods:
; 1) get-type returns the type of the data item.
; 2) valid? checks whether the data item satisfies the type's invariant.
(define-generics typed
  [get-type typed]
  [valid? typed])

; For the 7 data types specified here, a get-value method is also implemented
; for convenience to return their value.
(define-generics basic
  [get-value basic])

; Id is equivalent to the Symbol type in Racket, Boolean (renamed Bool) to Boolean,
; Count to Integer (positive) and Proportion to Rational.
(struct id (value)
  #:transparent
  #:methods gen:basic
  [(define (get-value self) (id-value self))]
  #:methods gen:typed
  [(define (get-type self) id)
   (define (valid? self) (symbol? (id-value self)))])

(struct bool (value)
  #:transparent
  #:methods gen:basic
  [(define (get-value self) (bool-value self))]
  #:methods gen:typed
  [(define (get-type self) bool)
   (define (valid? self) (or (eq? (bool-value self) #t)
                             (eq? (bool-value self) #f)))])

(struct count (value)
  #:transparent
  #:methods gen:basic
  [(define (get-value self) (count-value self))]
  #:methods gen:typed
  [(define (get-type self) count)
   (define (valid? self) (and (integer? (count-value self))
                              (>= (count-value self) 0)))])

(struct proportion (value)
  #:transparent
  #:methods gen:basic
  [(define (get-value self) (proportion-value self))]
  #:methods gen:typed
  [(define (get-type self) proportion)
   (define (valid? self) (rational? (proportion-value self)))])

; Nominal is a family of datatypes, each of which consists of a finite number
; of unordered values (see ISO/IEC 11404 standard). It is therefore specified
; as a structure to be specialised later.
(struct nominal (value)
  #:transparent
  #:methods gen:basic
  [(define (get-value self) (nominal-value self))])

; Enumerated is equivalent to nominal except that its values are ordered (see
; ISO/IEC 11404). As a result, it requires an interface for greater-than and
; less-than comparisons.
(define-generics enum
  [enum>? enum elem]
  [enum<? enum elem])

(struct enumerated (value)
  #:transparent
  #:methods gen:enum []
  #:methods gen:basic
  [(define (get-value self) (enumerated-value self))])

; Dimensioned is equivalent to DV_Quantity class in OpenEHR for representing
; quantities expressed as a magnitude and units. Automatic conversion of
; units is not supported, thus all mathematical operations must be provided
; with the output units (as a symbol). Furthermore, all comparators require
; the units to be equal.
(define-generics dim
  [dim>? dim elem]
  [dim<? dim elem]
  [dim+ dim elem units]
  [dim- dim elem units]
  [dim* dim elem units]
  [dim/ dim elem units])

(struct dimensioned (value units)
  #:transparent
  #:methods gen:basic
  [(define (get-value self) (dimensioned-value self))]
  #:methods gen:typed
  [(define (get-type self) dimensioned)
   (define (valid? self) (and (real? (dimensioned-value self))
                              (symbol? (dimensioned-units self))))]
  #:methods gen:dim
  [(define-syntax-rule (compare-with-units op dim elem op-sym)
     (cond [(not (dimensioned? elem))
            (raise-argument-error op-sym "dimensioned?" elem)]
           [(not (valid? elem))
            (raise-argument-error op-sym "(dimensioned real? symbol?)" elem)]
           [(not (valid? dim))
            (raise-argument-error op-sym "(dimensioned real? symbol?)" dim)]
           [(not (eq? (dimensioned-units dim) (dimensioned-units elem)))
            (raise-argument-error op-sym
                                  (format "~a" (dimensioned-units dim))
                                  (dimensioned-units elem))]
           [else (op (dimensioned-value dim) (dimensioned-value elem))]))
   (define (dim>? dim elem) (compare-with-units > dim elem 'dim>?))
   (define (dim<? dim elem) (compare-with-units < dim elem 'dim<?))

   (define-syntax-rule (arithmetic-with-units op dim elem units)
     (if (dimensioned? elem)
         (dimensioned (op (dimensioned-value dim) (dimensioned-value elem)) units)
         (dimensioned (op (dimensioned-value dim) elem) units)))
   (define (dim+ dim elem units) (arithmetic-with-units + dim elem units))
   (define (dim- dim elem units) (arithmetic-with-units - dim elem units))
   (define (dim* dim elem units) (arithmetic-with-units * dim elem units))
   (define (dim/ dim elem units) (arithmetic-with-units / dim elem units))])
