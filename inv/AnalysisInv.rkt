#lang rosette/safe

(require "../rpm/AnalysisProcess.rkt"
         "../rpm/MadeProcess.rkt"
         "../rim/BasicDataTypes.rkt"
         "../rim/TemporalDataTypes.rkt"
         "../rim/MadeDataStructures.rkt")

; Symbolic constants for verifying generate data.
(define (gen-dt-part) (define-symbolic* dt-part integer?) dt-part)
(define (gen-datetime)
  (let ([hour (gen-dt-part)])
    (assert (and (>= hour 0) (< hour 24)))
    (datetime 7 9 12 hour 0 0)))

(struct room-temperature observed-property () #:transparent)
(define (gen-temp-proxy) (define-symbolic* proxy boolean?) proxy)
(define (gen-temp-value) (define-symbolic* temp integer?) temp)
(define (gen-temp)
  (room-temperature (gen-temp-proxy) (gen-datetime) (gen-temp-value)))

(struct room-temperature-grade abstraction () #:transparent)
(define (grade-temp-low d-list)
  (if (>= (length (filter (lambda (d)
                            (and (room-temperature? d)
                                 (< (observed-property-value d) 5)))
                         d-list)) 3)
      'low
      (void)))
(define (grade-temp-high d-list)
  (if (>= (length (filter (lambda (d)
                            (and (room-temperature? d)
                                 (> (observed-property-value d) 10)))
                         d-list)) 4)
      'high
      (void)))

(define d-state (list (gen-temp) (gen-temp) (gen-temp) (gen-temp) (gen-temp)))
(define sched-dt (gen-datetime))
(define cur-dt (gen-datetime))
(define-symbolic proc-status boolean?)
(define-symbolic win-length integer?)
(assert (eq? (length d-state)
             (length
              (remove-duplicates
               (map (lambda (d) (observed-property-valid-datetime d)) d-state)))))
(assert (eq? (length d-state)
             (length
              (remove-duplicates
               (map (lambda (d) (observed-property-value d)) d-state)))))
(assert (eq? (length d-state)
             (length (filter
                      (lambda (d) (<= (datetime-hour
                                       (observed-property-valid-datetime d))
                                      (datetime-hour cur-dt)))
                      d-state))))
(assert (> win-length 0))

(define c-state (control-state (schedule (list sched-dt) #f) proc-status))
(define t-window (duration 0 win-length 0 0))
(define out-type room-temperature-grade)
(define ab-spec
  (list (abstraction-pair t-window grade-temp-low)
           (abstraction-pair t-window grade-temp-high)))
(define proc-proxy (gen-temp-proxy))
  
(struct sample-process analysis-process ()
  #:methods gen:analysis
  [(define (analysis-process-output-type self) room-temperature-grade)
   (define (analysis-process-output-specification self) ab-spec)
   (define (analysis-process-proxy-flag self) proc-proxy)]

  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) sample-process)
   (define (valid? self)
     (and (valid-spec? self)
          (super-valid? (made-process (made-process-data-state self)
                                      (made-process-control-state self)))))])

(define room-temp-proc (sample-process d-state c-state))

(define output (generate-data room-temp-proc null cur-dt))

;; Verify the implementation of the control state.
;(define (verify-is-executed)
;  (verify (assert (implies (or (not (eq? sched-dt cur-dt))
;                               (not proc-status))
;                           (null? output)))))
;
;; Verify the implementation of the time window.
;(define (verify-time-window)
;  (verify (assert (implies (< win-length 2) (null? output)))))
;
;; Verify the implementation of the abstraction functions.
;(define (verify-ab-funcs)
;  (verify
;   (assert
;    (implies (eq? (length d-state)
;                  (length (filter
;                           (lambda (d)
;                             (and (< (observed-property-value d) 14)
;                                  (> (observed-property-value d) 2)))
;                           d-state)))
;             (null? output)))))
;
;; Verify the implementation of the proxy flags.
;(define (verify-data-proxy)
;  (verify
;   (assert
;    (implies (<= 3 (length (filter (lambda (d) (made-data-proxy-flag d)) d-state)))
;             (null? output)))))
;
;(define (verify-proc-proxy)
;  (verify #:assume
;          (assert (and (not (null? output))
;                       (room-temperature-grade? (list-ref output 0))))
;          #:guarantee
;          (assert
;           (implies (proxy? room-temp-proc)
;                    (made-data-proxy-flag (list-ref output 0))))))
;
;; Verify the implementation of determining abstraction validity range.
;(define new-dt (datetime 7 9 12 (gen-dt-part) 0 0))
;(assert (normalized? new-dt))
;(define new-output (execute-analysis-body d-state new-dt out-type ab-spec #f))
;(define (verify-valid-range)
;  (verify (assert
;           (implies (and (not (null? output))
;                         (room-temperature-grade? (list-ref output 0))
;                         (dt>? new-dt (datetime-range-start
;                                       (abstraction-valid-datetime-range (list-ref output 0))))
;                         (dt<? new-dt (datetime-range-end
;                                       (abstraction-valid-datetime-range (list-ref output 0)))))
;                    (and (not (null? new-output))
;                         (room-temperature-grade? (list-ref new-output 0))
;                         (eq? (abstraction-value (list-ref new-output 0))
;                              (abstraction-value (list-ref output 0))))))))
