#lang rosette/safe

(require (only-in rosette for/list))
(require (for-syntax "./SyntaxUtil.rkt"))
(require "../rim/BasicDataTypes.rkt")
(require "../rim/TemporalDataTypes.rkt")

(provide get-duration
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

; get-datetime creates a symbolic datetime values. It accepts as input two
; optional concrete datetime values to indicate the range of possible datetime
; values.
(define get-datetime
  (case-lambda
    [()
     (get-datetime (datetime 2019 12 15 0 0 0)
                   (datetime 2019 12 15 23 0 0))]
    [(start-dt end-dt)
     (datetime (get-datetime-part (datetime-year start-dt)
                                  (datetime-year end-dt))
               (get-datetime-part (datetime-month start-dt)
                                  (datetime-month end-dt))
               (get-datetime-part (datetime-day start-dt)
                                  (datetime-day end-dt))
               (get-datetime-part (datetime-hour start-dt)
                                  (datetime-hour end-dt))
               (get-datetime-part (datetime-minute start-dt)
                                  (datetime-minute end-dt))
               (get-datetime-part (datetime-second start-dt)
                                  (datetime-second end-dt)))]))

(define (get-datetime-part lo hi)
  (if (= lo hi)
      lo
      (begin
  (define-symbolic* dt-part integer?)
  (assert (and (>= dt-part lo) (<= dt-part hi)))
  dt-part)))

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
  (define-symbolic* bool-int? boolean?)
  (define-symbolic* bool-val boolean?)
  (if bool-int? bool-val (get-duration)))

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
          (displayln "")))
    (clear-asserts!)))