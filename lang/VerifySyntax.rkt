#lang rosette/safe

(require (for-syntax "./SyntaxUtil.rkt"))
(require "../rim/BasicDataTypes.rkt")
(require "../rim/TemporalDataTypes.rkt")

(provide define-datetime-generator
         get-duration
         get-proxy
         get-dimensioned
         get-bool
         get-count
         get-proportion)

; This file contains the syntax of functions for verifying concrete MADE models.

; define-datetime-generator creates a get-datetime function for creating
; symbolic datetime values. It accepts as input two concrete datetime values to
; indicate the range of possible datetime values.
(define-syntax (define-datetime-generator stx)
  (define (gen-stx-part lo hi)
    (if (eq? (syntax->datum lo) (syntax->datum hi))
        lo
        (with-syntax ([min lo]
                      [max hi])
          #'(gen-dt-part min max))))

  (syntax-case stx ()
    [(_ (dt-1 yr-1 mth-1 d-1 hr-1 min-1 sec-1)
        (dt-2 yr-2 mth-2 d-2 hr-2 min-2 sec-2))
     (begin
       (raise-if-not-datetime #'(dt-1 yr-1 mth-1 d-1 hr-1 min-1 sec-1) stx)
       (raise-if-not-datetime #'(dt-2 yr-2 mth-2 d-2 hr-2 min-2 sec-2) stx)
       (with-syntax ([get-dt (build-getter-name #'dt-1)]
                     [year (gen-stx-part #'yr-1 #'yr-2)]
                     [month (gen-stx-part #'mth-1 #'mth-2)]
                     [day (gen-stx-part #'d-1 #'d-2)]
                     [hour (gen-stx-part #'hr-1 #'hr-2)]
                     [minute (gen-stx-part #'min-1 #'min-2)]
                     [second (gen-stx-part #'sec-1 #'sec-2)])
         #'(define get-dt
             (lambda ()
               (define (gen-dt-part lo hi)
                 (define-symbolic* dt-part integer?)
                 (assert (and (>= dt-part lo) (<= dt-part hi)))
                 dt-part)
               (datetime year month day hour minute second)))))]))

; get-duration creates a symbolic duration value.
(define (get-duration)
  (define (get-dur-part)
    (define-symbolic* dur-part integer?)
    dur-part)
  (duration (get-dur-part) (get-dur-part) (get-dur-part) (get-dur-part)))

; get-proxy creates a symbolic boolean value for the proxy.
(define (get-proxy)
  (define-symbolic* proxy boolean?)
  proxy)

; get-dimensioned creates a symbolic dimensioned value.
; It requires the appropriate units to be provided.
(define (get-dimensioned units)
  (define-symbolic* dim real?)
  (dimensioned dim units))

; get-bool creates a symbolic bool value.
(define (get-bool)
  (define-symbolic* bool-val boolean?)
  (bool bool-val))

; get-count creates a symbolic count value.
(define (get-count)
  (define-symbolic* count-val integer?)
  (count count-val))

; get-proportion creates a symbolic proportion value.
(define (get-proportion)
  (define-symbolic* prop-val real?)
  (proportion prop-val))