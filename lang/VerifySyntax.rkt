#lang rosette/safe

(require (only-in rosette for/list))
(require (for-syntax "./SyntaxUtil.rkt"))
(require "../rim/BasicDataTypes.rkt")
(require "../rim/TemporalDataTypes.rkt")

(provide define-datetime-generator
         get-duration
         get-schedule
         get-status
         get-proxy
         get-dimensioned
         get-bool
         get-count
         get-proportion
         get-datetime
         verify-getter)

; This file contains the syntax of functions for verifying concrete MADE models.

; define-datetime-generator creates a get-datetime function for creating
; symbolic datetime values. It accepts as input two concrete datetime values to
; indicate the range of possible datetime values.
(define get-datetime (define-datetime-generator))
(define-syntax (define-datetime-generator stx)
  (define (gen-stx-part lo hi)
    (if (eq? (syntax->datum lo) (syntax->datum hi))
        lo
        (with-syntax ([min lo]
                      [max hi])
          #'(gen-dt-part min max))))

  (syntax-case stx ()
    [(define-dt-gen (dt-1 yr-1 mth-1 d-1 hr-1 min-1 sec-1)
                    (dt-2 yr-2 mth-2 d-2 hr-2 min-2 sec-2))
     (begin
       (raise-if-not-datetime #'(dt-1 yr-1 mth-1 d-1 hr-1 min-1 sec-1) stx)
       (raise-if-not-datetime #'(dt-2 yr-2 mth-2 d-2 hr-2 min-2 sec-2) stx)
       (with-syntax ([get-dt (datum->syntax #'define-dt-gen
                                            'get-datetime
                                            #'define-dt-gen)]
                     [year (gen-stx-part #'yr-1 #'yr-2)]
                     [month (gen-stx-part #'mth-1 #'mth-2)]
                     [day (gen-stx-part #'d-1 #'d-2)]
                     [hour (gen-stx-part #'hr-1 #'hr-2)]
                     [minute (gen-stx-part #'min-1 #'min-2)]
                     [second (gen-stx-part #'sec-1 #'sec-2)])
         #'(lambda ()
             (define (gen-dt-part lo hi)
               (define-symbolic* dt-part integer?)
               (assert (and (>= dt-part lo) (<= dt-part hi)))
               dt-part)
             (datetime year month day hour minute second))))]
    [(define-dt-gen) #'(define-dt-gen
                         (datetime 2019 12 15 0 0 0)
                         (datetime 2019 12 15 23 0 0))]))

; get-duration creates a symbolic duration value.
(define (get-duration)
  (define (get-dur-part)
    (define-symbolic* dur-part integer?)
    dur-part)
  (duration (get-dur-part) (get-dur-part) (get-dur-part) (get-dur-part)))

; get-schedule creates a symbolic schedule value.
; Accepts as argument the number of datetime values in the starting pattern.
(define-syntax (get-schedule stx)
  (syntax-case stx ()
    [(get-schedule max)
     #'(schedule
        (for/list ([n (- max 1)]) (get-datetime))
        (get-interval))]))

; get-interval creates a symbolic value for the repeat interval of a schedule.
(define (get-interval)
  (define-symbolic* int boolean?)
  (if int (get-duration) #f))

; get-proxy creates a symbolic boolean value for the proxy.
(define (get-proxy)
  (define-symbolic* proxy boolean?)
  proxy)

; get-status creates a symbolic boolean value for the process status.
(define (get-status)
  (define-symbolic* status boolean?)
  status)

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

; verify-getter checks whether the input getter can return a valid instance. If
; not a warning is displayed. If yes, then an example is displayed.
(define (verify-getter get-id id)
  (let* ([example-1 (get-id)]
                    [example-2 (get-id)]
                    [solution (solve (assert (and (valid? example-1)
                                                  (valid? example-2)
                                                  (not (eq? example-1 example-2)))))])
               (if (eq? solution (unsat))
                   (displayln (format "Valid ~a cannot be generated from specification." id))
                   (begin
                     (displayln (format "Example ~a: " id))
                     (displayln (evaluate example-1 solution))
                     (displayln "")))))