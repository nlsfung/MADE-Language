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

(define (grade-temp-low-pred d-list)
  (>= (length (filter (lambda (d)
                        (and (room-temperature? d)
                             (< (observed-property-value d) 5)))
                      d-list)) 3))
(define (grade-temp-low-func d-list) 'low)

(define (grade-temp-high-pred d-list)
  (>= (length (filter (lambda (d)
                        (and (room-temperature? d)
                             (> (observed-property-value d) 10)))
                      d-list)) 4))
(define (grade-temp-high-func d-list) 'high)

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
  (list (abstraction-triplet t-window grade-temp-low-pred grade-temp-low-func)
        (abstraction-triplet t-window grade-temp-high-pred grade-temp-high-func)))
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

; Verify implementation of generate-data for Analysis processes.
(define dt-mid
  (let ([hour (gen-dt-part)]
        [day (gen-dt-part)])
    (assert (and (>= hour 0) (< hour 24)))
    (assert (and (>= day 1) (< day 31)))
    (datetime 7 9 day hour 0 0)))

(define (filter-ext dSet dt-start dt-end)
  (filter-observations
   (remove-duplicates
    (filter (lambda (d)
              (not (made-data-proxy-flag d)))
            dSet))
   dt-start
   dt-end))

(define (verify-ab-pair-necessity)
  (verify
   (assert
    (implies (not (null? output))
             (ormap (lambda (ab-pair)
                      ((abstraction-triplet-abstraction-predicate ab-pair)
                       (filter-ext
                        d-state
                        (dt- cur-dt (abstraction-triplet-time-window ab-pair))
                        cur-dt)))
                    ab-spec)))))

(define (verify-ab-pair-sufficiency)
  (verify
   (assert
    (implies (and (null? output)
                  (is-proc-executed? c-state cur-dt))
             (andmap (lambda (ab-pair)
                       (not
                        ((abstraction-triplet-abstraction-predicate ab-pair)
                         (filter-ext
                          d-state
                          (dt- cur-dt (abstraction-triplet-time-window ab-pair))
                          cur-dt))))
                     ab-spec)))))

(define (verify-abstraction-id)
  (verify
   (assert
    (implies (not (null? output))
             (and (room-temperature-grade? (list-ref output 0))
                  (eq? proc-proxy
                       (made-data-proxy-flag (list-ref output 0))))))))

(define (verify-abstraction-value)
  (verify
   (assert
    (implies (not (null? output))
             (and (eq? (datetime-range-start
                        (abstraction-valid-datetime-range
                         (list-ref output 0)))
                       cur-dt)
                  (implies (and (dt>=? dt-mid
                                       (datetime-range-start
                                        (abstraction-valid-datetime-range
                                         (list-ref output 0))))
                                (dt<=? dt-mid
                                       (datetime-range-end
                                        (abstraction-valid-datetime-range
                                         (list-ref output 0)))))
                           (and (implies (eq? (abstraction-value (list-ref output 0))
                                              'low)
                                         (grade-temp-low-pred
                                          (filter-ext d-state
                                                      (dt- dt-mid (duration 0 win-length 0 0))
                                                      dt-mid)))
                                (implies (eq? (abstraction-value (list-ref output 0))
                                              'high)
                                         (grade-temp-high-pred
                                          (filter-ext d-state
                                                      (dt- dt-mid (duration 0 win-length 0 0))
                                                      dt-mid))))))))))
