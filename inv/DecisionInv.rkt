#lang rosette/safe

(require "../rpm/DecisionProcess.rkt"
         "../rpm/AnalysisProcess.rkt"
         "../rpm/MadeProcess.rkt"
         "../rim/BasicDataTypes.rkt"
         "../rim/TemporalDataTypes.rkt"
         "../rim/MadeDataStructures.rkt")

; Symbolic constants for verifying generate data.
(define (gen-dt-part)
  (define-symbolic* dt-part integer?)
  dt-part)

(define (gen-datetime)
  (let ([hour (gen-dt-part)])
    (assert (and (>= hour 0) (< hour 24)))
    (datetime 7 9 21 hour 0 0)))

(define (gen-dt-range)
  (let* ([start-hour (gen-dt-part)]
         [end-hour (gen-dt-part)])
    (assert (and (>= start-hour 0) (<= start-hour end-hour) (< end-hour 24)))
    (datetime-range (datetime 7 9 21 start-hour 0 0)
                    (datetime 7 9 21 end-hour 0 0))))

(define (gen-proxy)
  (define-symbolic* proxy boolean?)
  proxy)

(define headache-grades (list 'none 'low 'medium 'high))
(define (gen-headache-grade)
  (define-symbolic* h-grade integer?)
  (assert (and (>= h-grade 0) (<= h-grade 3)))
  (list-ref headache-grades h-grade))

(struct headache-level abstraction ()
  #:transparent
  #:methods gen:typed
  [(define (get-type self) headache-level)])

(define (gen-temp) (define-symbolic* temp integer?) temp)

(struct avg-body-temp abstraction ()
  #:transparent
  #:methods gen:typed
  [(define (get-type self) avg-body-temp)])

(define (gen-headache-level)
  (headache-level (gen-proxy) (gen-dt-range) (gen-headache-grade)))
(define (gen-avg-body-temp)
  (avg-body-temp (gen-proxy) (gen-dt-range) (gen-temp)))

(struct fever-treatment action-plan () #:transparent)

(define (gen-round) (define-symbolic* rounding integer?) rounding)
(define r-fact (duration (gen-round) 0 0 0))
(define offset (duration (gen-round) 0 0 0))
(define r-patt (duration (gen-round) 0 0 0))

(define ibuprofen-rel-sched
  (relative-schedule r-fact offset (list r-patt) #f))
(define ibuprofen-template
  (culminating-action-template
   'ibuprofen
   ibuprofen-rel-sched
   (bool #t)))

(define treadmill-template
  (homogeneous-action-template
   'treadmill-exercise
   (relative-schedule
    (duration 1 0 0 0)
    (duration 0 0 0 0)
    (list (duration 0 13 0 0)
          (duration 0 21 0 0))
    (duration 2 0 0 0))
   (dimensioned 10 'units)
   (duration 0 1 0 0)))

(define analyze-heart-rate-template
  (control-template
   'analyze-heart-rate
   (relative-schedule (duration 0 0 0 0) (duration 0 0 0 0) null #f)
   #f))

(define fever-treatment-template
  (plan-template
   fever-treatment
   (list ibuprofen-template treadmill-template analyze-heart-rate-template)))

(define (decision-criterion-one d-list)
  (and (findf (lambda (d) (and (avg-body-temp? d)
                               (> (abstraction-value d) 37)))
              d-list)
       (findf (lambda (d) (and (headache-level? d)
                               (eq? 'high (abstraction-value d))))
              d-list)))

(define (decision-criterion-two d-list)
  (findf (lambda (d) (and (avg-body-temp? d)
                          (> (abstraction-value d) 40)))
           d-list))

(define d-state
  (let* ([headaches (list (gen-headache-level) (gen-headache-level))]
         [body-temps (list (gen-avg-body-temp) (gen-avg-body-temp))])
    (assert (= 2 (length (remove-duplicates headaches))))
    (assert (= 2 (length (remove-duplicates body-temps))))
    (append headaches body-temps)))

(define sched-dt (gen-datetime))
(define cur-dt (gen-datetime))
(define-symbolic* proc-status boolean?)
(define c-state (control-state (schedule (list sched-dt) #f) proc-status))
(define proc-proxy (gen-proxy))

(struct sample-process decision-process ()
  #:transparent
  #:methods gen:decision
  [(define (decision-process-plan-template self) fever-treatment-template)
   (define (decision-process-decision-criteria self)
     (list decision-criterion-one decision-criterion-two))
   (define (decision-process-proxy-flag self) proc-proxy)]

  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) sample-process)
   (define (valid? self)
     (and (valid-spec? self)
          (super-valid? (made-process (made-process-data-state self)
                                      (made-process-control-state self)))))])

(define d-proc (sample-process d-state c-state))

(define output (generate-data d-proc null cur-dt))

; Verify implementation of generate-data for Decision processes.
(define (filter-ext dSet dt)
  (filter-abstractions
   (remove-duplicates
    (filter (lambda (d)
              (not (made-data-proxy-flag d)))
            dSet))
   dt))

(define (verify-d-crit-necessity)
  (verify
   (assert
    (implies (not (null? output))
             (ormap (lambda (d-crit)
                      (d-crit (filter-ext d-state cur-dt)))
                    (list decision-criterion-one decision-criterion-two))))))

(define (verify-d-crit-sufficiency)
  (verify
   (assert
    (implies (and (null? output)
                  (is-proc-executed? c-state cur-dt))
             (andmap (lambda (d-crit)
                       (not (d-crit (filter-ext d-state cur-dt))))
                     (list decision-criterion-one decision-criterion-two))))))

(define (verify-plan-template)
  (verify
   (assert
    (implies (not (null? output))
             (and (eq? output
                       (list (plan-instantiate
                              fever-treatment-template
                              cur-dt
                              proc-proxy)))
                  (fever-treatment? (list-ref output 0))
                  (eq? (action-plan-valid-datetime (list-ref output 0))
                       cur-dt))))))


; Verify implementation of schedule instantiation.
(define (verify-rounding-factor)
  (verify #:assume
          (assert
           (normalized?
            (list-ref (schedule-pattern
                       (sched-instantiate ibuprofen-rel-sched cur-dt))
                      0)))

          #:guarantee
          (assert
           (let* ([sched-patt (list-ref
                               (schedule-pattern
                                (sched-instantiate ibuprofen-rel-sched cur-dt))
                               0)])
             (implies (and (dur=? offset (duration 0 0 0 0))
                           (dur=? r-patt (duration 0 0 0 0)))
                      (and (implies (ormap (lambda (n) (dur=? r-fact (duration n 0 0 0)))
                                           (list 0 1 2 3 4 5 6 7 8 9 10))
                                    (or (dt>? sched-patt cur-dt)
                                        (dt=? sched-patt cur-dt)))
                           (implies (ormap (lambda (n) (dur=? r-fact (duration (- n) 0 0 0)))
                                           (list 0 1 2 3 4 5 6 7 8 9 10))
                                    (or (dt<? sched-patt cur-dt)
                                        (dt=? sched-patt cur-dt)))))))))

(define (verify-relative-schedule)
  (verify #:assume
          (assert
           (normalized?
            (list-ref (schedule-pattern
                       (sched-instantiate ibuprofen-rel-sched cur-dt))
                      0)))

          #:guarantee
          (assert
           (let* ([sched-patt (list-ref
                               (schedule-pattern
                                (sched-instantiate ibuprofen-rel-sched cur-dt))
                               0)])
             (implies (dur=? r-fact (duration 0 0 0 0))
                      (implies (and (ormap (lambda (n) (dur=? offset (duration n 0 0 0)))
                                           (list 0 1 2 3 4 5 6 7 8 9 10))
                                    (ormap (lambda (n) (dur=? r-patt (duration n 0 0 0)))
                                           (list 0 1 2 3 4 5 6 7 8 9 10)))
                               (dt=? sched-patt (dt+ cur-dt (dur+ offset r-patt)))))))))