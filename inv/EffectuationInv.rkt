#lang rosette/safe

(require "../rpm/EffectuationProcess.rkt"
         "../rpm/AnalysisProcess.rkt"
         "../rpm/MadeProcess.rkt"
         "../rim/BasicDataTypes.rkt"
         "../rim/TemporalDataTypes.rkt"
         "../rim/MadeDataStructures.rkt")

; Symbolic constants for verifying generate data.
(define (gen-dt-part) (define-symbolic* dt-part integer?) dt-part)
(define (gen-datetime)
  (let ([hour (gen-dt-part)])
    (assert (and (>= hour 0) (< hour 24)))
    (datetime 7 10 7 hour 0 0)))

(define (gen-proxy) (define-symbolic* proxy boolean?) proxy)

(define (gen-schedule)
  (let* ([pattern (list (gen-datetime) (gen-datetime))]
         [status #f])
    (schedule pattern status)))

(struct fever-treatment action-plan ()
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) fever-treatment)
   (define (valid? self)
     (super-valid? (action-plan (made-data-proxy-flag self)
                                (action-plan-valid-datetime self)
                                (action-plan-instruction-set self))))])

(struct exercise-regimen action-plan ()
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) exercise-regimen)
   (define (valid? self)
     (super-valid? (action-plan (made-data-proxy-flag self)
                                (action-plan-valid-datetime self)
                                (action-plan-instruction-set self))))])
(struct ibuprofen culminating-action () #:transparent)
(struct treadmill-exercise homogeneous-action () #:transparent)
(struct control-analyze-heart-rate control-instruction () #:transparent)
(struct analyze-heart-rate analysis-process () #:transparent)

(define (gen-fever-treatment-one)
  (fever-treatment
   (gen-proxy)
   (gen-datetime)
   (list (scheduled-culminating-action
          ibuprofen (gen-schedule) (bool #t))
         (scheduled-homogeneous-action
          treadmill-exercise
          (gen-schedule)
          (dimensioned 10 'units)
          (duration 0 1 0 0)))))

(define (gen-fever-treatment-two)
  (fever-treatment
   (gen-proxy)
   (gen-datetime)
   (list (scheduled-control
          analyze-heart-rate (gen-schedule) (void)))))

(define (gen-exercise-regimen-one)
   (exercise-regimen
    (gen-proxy)
    (gen-datetime)
    (list (scheduled-homogeneous-action
           treadmill-exercise
           (gen-schedule)
           (dimensioned 15 'units)
           (duration 0 2 0 0)))))

(define d-state
  (let* ([fever-one (list (gen-fever-treatment-one) (gen-fever-treatment-one))]
         [fever-two (list (gen-fever-treatment-two) (gen-fever-treatment-two))]
         [exercise-one (list (gen-exercise-regimen-one) (gen-exercise-regimen-one))])
    (assert (not (eq? (list-ref fever-one 0) (list-ref fever-one 1))))
    (assert (not (eq? (list-ref fever-two 0) (list-ref fever-two 1))))
    (assert (not (eq? (list-ref exercise-one 0) (list-ref exercise-one 1))))
    (append fever-one fever-two exercise-one)))
  
(define sched-dt (gen-datetime))
(define cur-dt (gen-datetime))
(define-symbolic proc-status boolean?)
(define c-state (control-state (schedule (list sched-dt) #f) proc-status))

(define (gen-inst-pred)
  (define-symbolic* inst-pred boolean?)
  (lambda (i) inst-pred))

(define proc-proxy (gen-proxy))
(define proc-target-schedules
  (list (target-schedule fever-treatment treadmill-exercise (gen-inst-pred))
        (target-schedule exercise-regimen treadmill-exercise (gen-inst-pred))))

(struct sample-process effectuation-process ()
  #:transparent
  #:methods gen:effectuation
  [(define (effectuation-process-target-schedules self) proc-target-schedules)
   (define (effectuation-process-output-type self) treadmill-exercise)
   (define (effectuation-process-proxy-flag self) proc-proxy)]

  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) sample-process)
   (define (valid? self)
     (and (valid-spec? self)
          (super-valid? (made-process (made-process-data-state self)
                                      (made-process-control-state self)))))])

(define e-proc (sample-process d-state c-state)) 

(define output (generate-data e-proc null cur-dt))

;; Verify implementation of extract instruction.
;(define-symbolic x y integer?)
;(define d (list-ref d-state x))
;(define t-sched (list-ref (effectuation-process-target-schedules e-proc) y))
;(define inst (extract-instruction d cur-dt t-sched))
;(define (verify-extract-instruction)
;  (verify
;   (assert
;    (let* ([plan-match?
;            (eq? (target-schedule-plan-type t-sched) (get-type d))]
;           [inst-matches
;            (filter (lambda (i)
;                      (cond [(scheduled-control? i)
;                             (and (eq? (target-schedule-instruction-type t-sched)
;                                       (scheduled-control-target-process i))
;                                  ((target-schedule-instruction-predicate t-sched) i))]
;                            [(scheduled-homogeneous-action? i)
;                             (and (eq? (target-schedule-instruction-type t-sched)
;                                       (scheduled-homogeneous-action-action-type i))
;                                  ((target-schedule-instruction-predicate t-sched) i))]
;                            [(scheduled-culminating-action? i)
;                             (and (eq? (target-schedule-instruction-type t-sched)
;                                       (scheduled-culminating-action-action-type i))
;                                  ((target-schedule-instruction-predicate t-sched) i))]))
;                    (action-plan-instruction-set d))]
;           [sched-matches
;            (filter (lambda (i)
;                      (cond [(scheduled-control? i)
;                             (dt=? (action-plan-valid-datetime d) cur-dt)]
;                            [(scheduled-homogeneous-action? i)
;                             (on-schedule? (scheduled-homogeneous-action-schedule i) cur-dt)]
;                            [(scheduled-culminating-action? i)
;                             (on-schedule? (scheduled-culminating-action-schedule i) cur-dt)]))
;                    (action-plan-instruction-set d))])
;      (implies (not inst)
;               (or (not plan-match?)
;                   (eq? (length inst-matches)
;                        (length (remove* sched-matches inst-matches)))))))))
;
;; Verify the implementation of the proxy flags.
;(define (verify-data-proxy)
;  (verify
;   (assert
;    (implies (= 6 (length (filter (lambda (d) (made-data-proxy-flag d)) d-state)))
;             (null? output)))))
;
;(define (verify-proc-proxy)
;  (verify #:assume
;          (assert (not (null? output)))
;          #:guarantee
;          (assert
;           (implies (proxy? e-proc)
;                    (made-data-proxy-flag (list-ref output 0))))))
;
;; Verify implementation of execute effectuation body.
;(define-symbolic v integer?)
;(define d-2 (list-ref d-state v))
;(define cur-dt-2 (gen-datetime))
;(define output-2 (execute-effectuation-body
;                  (list d-2)
;                  cur-dt-2
;                  (effectuation-process-target-schedules e-proc)
;                  (effectuation-process-output-type e-proc)
;                  (effectuation-process-proxy-flag e-proc)))
;
;(define (verify-effectuation-body)
;  (verify
;   #:assume
;   (assert (and (= 6 (length (filter
;                              (lambda (d) (not (made-data-proxy-flag d)))
;                              d-state)))
;                (= 6 (length (remove-duplicates
;                              (map (lambda (d) (action-plan-valid-datetime d))
;                                   d-state))))))
;
;   #:guarantee   
;   (assert (implies (and (not (null? output))
;                         (not (null? output-2))
;                         (not (eq? output output-2)))
;                    (or (not (dt=? cur-dt cur-dt-2))
;                        (dt>? (action-plan-valid-datetime d-2) cur-dt)
;                        (findf (lambda (d)
;                                 (or (and (eq? (get-type d) (get-type d-2))
;                                          (dt>? (action-plan-valid-datetime d)
;                                                (action-plan-valid-datetime d-2)))
;                                     (and (eq? output
;                                           (execute-effectuation-body
;                                            (list d)
;                                            cur-dt
;                                            (effectuation-process-target-schedules e-proc)
;                                            (effectuation-process-output-type e-proc)
;                                            (effectuation-process-proxy-flag e-proc)))
;                                          (> (length (member d d-state))
;                                             (length (member d-2 d-state))))))
;                               d-state))))))