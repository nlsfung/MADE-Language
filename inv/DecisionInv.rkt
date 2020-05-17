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

(struct body-acceleration measurement () #:transparent)
(define (gen-body-acc)
  (body-acceleration #f (gen-datetime) (gen-temp)))

(struct fever-treatment action-plan () #:transparent)

(define (gen-round) (define-symbolic* rounding integer?) rounding)
(define r-fact (duration (gen-round) 0 0 0))
(define r-patt (duration (gen-round) 0 0 0))

(define ibuprofen-rel-sched
  (relative-schedule r-fact (list r-patt) #f))
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
    (list (duration 0 13 0 0)
          (duration 0 21 0 0))
    (duration 2 0 0 0))
   (dimensioned 10 'units)
   (duration 0 1 0 0)))

(define analyze-heart-rate-template
  (control-template
   'analyze-heart-rate
   (relative-schedule (duration 0 0 0 0) null #f)
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

; Inv. 3.6 - Verify relevance of abstractions for Decision processes.
(define output-alt (generate-data d-proc (list (gen-body-acc)) cur-dt))
(define (verify-abstraction-relevance)
  (verify #:assume (assert (not (null? output)))
          #:guarantee (assert (eq? output output-alt))))

; Inv. 3.7 - Verify type of output data.
(define (verify-output-type)
  (verify (assert (implies (not (null? output))
                           (andmap (lambda (d) (action-plan? d)) output)))))

; Inv. 3.8 - Verify implementation of proxy flag.
(define (verify-proxy-flag)
  (verify (assert (implies (not (null? output))
                           (andmap (lambda (d) (eq? (made-data-proxy-flag d)
                                                    (decision-process-proxy-flag d-proc)))
                                   output)))))

; Inv. 6.7 - Verify length of output.
(define (verify-output-length)
  (verify (assert (<= (length output) 1))))

; Inv. 6.18 - Verify implementation of filter abstractions.
(define (verify-filter)
  (verify
   (assert
    (andmap (lambda (d)
              (and (abstraction? d)
                   (dt-between? cur-dt
                                (datetime-range-start
                                 (abstraction-valid-datetime-range d))
                                (datetime-range-end
                                 (abstraction-valid-datetime-range d)))
                   (andmap (lambda (d-inner)
                             (implies (eq? (get-type d) (get-type d-inner))
                                      (or (not (dt-between?
                                                cur-dt
                                                (datetime-range-start
                                                 (abstraction-valid-datetime-range d-inner))
                                                (datetime-range-end
                                                 (abstraction-valid-datetime-range d-inner))))
                                          (dt>=? (transaction-datetime d)
                                                 (transaction-datetime d-inner)))))
                           (append d-state (list (made-data #f))))))
            (filter-abstractions (append d-state (list (made-data #f)))
                                 cur-dt)))))

; Inv. 6.19 - Verify implementation of plan-instantiate.
(define (verify-plan-instantiate)
  (verify
   (assert (eq? (plan-instantiate fever-treatment-template cur-dt #f)
                (fever-treatment
                 #f
                 cur-dt
                 (map (lambda (inst) (instantiate inst cur-dt))
                      (plan-template-instruction-set
                       fever-treatment-template)))))))

; Inv. 6.20 - Verify implementation of sched-instantiate.
(define (verify-sched-instantiate)
  (verify
   (assert (let* ([rounded-dur (get-round-amount cur-dt r-fact)]
                  [pattern-dt (map (lambda (dur)
                                     (dt+ cur-dt (dur+ dur rounded-dur)))
                                   (list r-patt))]
                  [sched-orig (sched-instantiate (relative-schedule r-fact (list r-patt) #f)
                                                 cur-dt)])
             (dt=? (list-ref (schedule-pattern sched-orig) 0)
                   (list-ref pattern-dt 0))))))

; The following are two extra invariants for verifying the implementation
; of the rounding factor and relative pattern.
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
             (implies (dur=? r-patt (duration 0 0 0 0))
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
                      (implies (ormap (lambda (n) (dur=? r-patt (duration n 0 0 0)))
                                      (list 0 1 2 3 4 5 6 7 8 9 10))
                               (dt=? sched-patt (dt+ cur-dt r-patt))))))))

; Inv. 6.21 - Verify instantiation of homogeneous action templates.
(define (verify-homogeneous-template)
  (verify
   (assert (eq? (instantiate treadmill-template cur-dt)
                (scheduled-homogeneous-action
                 'treadmill-exercise
                 (sched-instantiate
                  (homogeneous-action-template-relative-schedule treadmill-template)
                  cur-dt)
                 (dimensioned 10 'units)
                 (duration 0 1 0 0))))))

; Inv. 6.22 - Verify instantiation of culminating action template.
(define (verify-culminating-template)
  (verify
   (assert (eq? (instantiate ibuprofen-template cur-dt)
                (scheduled-culminating-action
                 'ibuprofen
                 (sched-instantiate
                  (culminating-action-template-relative-schedule ibuprofen-template)
                  cur-dt)
                 (bool #t))))))

; Inv. 6.23 - Verify instantiation of control instruction template.
(define (verify-control-template)
  (verify
   (assert (eq? (instantiate analyze-heart-rate-template cur-dt)
                (scheduled-control
                 'analyze-heart-rate
                 (sched-instantiate
                  (control-template-relative-schedule analyze-heart-rate-template)
                  cur-dt)
                 #f)))))

; Inv. 6.24 - Verify condition for outputting an action plan.
(define (filter-ext dSet dt)
  (filter-abstractions
   (remove-duplicates
    (filter (lambda (d)
              (not (made-data-proxy-flag d)))
            dSet))
   dt))

(define (verify-d-crit-sufficiency)
  (verify
   #:assume
   (assert (is-proc-activated? c-state cur-dt))

   #:guarantee
   (assert
    (eq? (null? output)
         (not (not (andmap (lambda (d-crit)
                             (not (d-crit (filter-ext d-state cur-dt))))
                           (list decision-criterion-one decision-criterion-two))))))))

; Inv. 6.25 - Verify output of Decision process.
(define (verify-plan-template)
  (verify
   (assert
    (implies (not (null? output))
             (eq? output
                       (list (plan-instantiate
                              fever-treatment-template
                              cur-dt
                              proc-proxy)))))))
