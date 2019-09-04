#lang rosette

; The complete MADE reference information model (RIM) is constructed from 10
; primitive data types: Id, Boolean, Nominal, Enumerated, Count, Proportion,
; Dimensioned, DateTime, Duration and Schedule. 

; This file contains the specification of the first 7 data types. The last 3
; (DateTime, Duration and Schedule) are specified in TemporalDataTypes.rkt.

; Id is equivalent to the Symbol type in Racket, Boolean to Boolean, Count to
; Integer and Proportion to Rational, thus these types need not be specified
; further except for the addition of syntax to recognise their equivalence.
(define-syntax-rule (id? x) (symbol? x))
(define-syntax-rule (count? x) (integer? x))
(define-syntax-rule (proportion? x) (rational? x))

; Nominal is a family of datatypes, each of which consists of a finite number
; of unordered values (see ISO/IEC 11404 standard). It is therefore specified
; as a generic interface with an equals operator. For simplicity, "nom" will
; be used in place of "nominal" in the code.
(define-generics nom
  [nom=? nom elem])

; Enumerated is equivalent to nominal except that its values are ordered (see
; ISO/IEC 11404). As a result, its interface contains operators for other
; types of comparison (viz. larger and smaller than). For simplicity, "enum"
; will be used in place of "enumerated".
(define-generics enum
  [enum=? enum elem]
  [enum>? enum elem]
  [enum<? enum elem])

; Dimensioned is equivalent to DV_Quantity class in OpenEHR for representing
; quantities expressed as a magnitude and units. Automatic conversion of
; units is not supported, thus all mathematical operations must be provided
; with the output units (as a symbol). Furthermore, all comparators require
; the units to be equal.
(define-generics dim
  [dim=? dim elem]
  [dim>? dim elem]
  [dim<? dim elem]
  [dim+ dim elem units]
  [dim- dim elem units]
  [dim* dim elem units]
  [dim/ dim elem units])

(struct dimensioned (value units)
  #:methods gen:dim
  [(define-syntax-rule (compare-with-units op dim elem)
     (if (eq? (dimensioned-units dim) (dimensioned-units elem))
         (op (dimensioned-value dim) (dimensioned-value elem))
         #f))   
   (define (dim=? dim elem) (compare-with-units = dim elem))
   (define (dim>? dim elem) (compare-with-units > dim elem))
   (define (dim<? dim elem) (compare-with-units < dim elem))

   (define-syntax-rule (arithmetic-with-units op dim elem units)
     (if (dimensioned? elem)
         (dimensioned (op (dimensioned-value dim) (dimensioned-value elem)) units)
         (dimensioned (op (dimensioned-value dim) elem) units)))
   (define (dim+ dim elem units) (arithmetic-with-units + dim elem units))
   (define (dim- dim elem units) (arithmetic-with-units - dim elem units))
   (define (dim* dim elem units) (arithmetic-with-units * dim elem units))
   (define (dim/ dim elem units) (arithmetic-with-units / dim elem units))])
