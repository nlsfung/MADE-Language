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

(struct body-acceleration measurement () #:transparent)
(define (gen-body-acc)
  (body-acceleration #f (gen-datetime) 100))

(define d-state
  (let* ([fever-one (list (gen-fever-treatment-one))]
         [fever-two (list (gen-fever-treatment-two) (gen-fever-treatment-two))]
         [exercise-one (list (gen-exercise-regimen-one))])
    (assert (not (eq? (list-ref fever-two 0) (list-ref fever-two 1))))
    (append fever-one fever-two exercise-one)))
  
(define sched-dt (gen-datetime))
(define cur-dt (gen-datetime))
(define-symbolic proc-status boolean?)
(define c-state (control-state (schedule (list sched-dt) #f) proc-status))

(define (gen-inst-pred)
  (define-symbolic* inst-pred boolean?)
  (lambda (i) inst-pred))

(define-symbolic target-branch integer?)
(assert (and (>= target-branch 0) (< target-branch 3)))
(define target-inst (cond [(eq? target-branch 0) treadmill-exercise]
                          [(eq? target-branch 1) ibuprofen]
                          [(eq? target-branch 2) analyze-heart-rate]))
(define target-output (cond [(eq? target-branch 0) treadmill-exercise]
                          [(eq? target-branch 1) ibuprofen]
                          [(eq? target-branch 2) control-analyze-heart-rate]))
(define proc-proxy (gen-proxy))
(define proc-target-schedules
  (list (target-schedule fever-treatment target-inst (gen-inst-pred))
        (target-schedule exercise-regimen target-inst (gen-inst-pred))))

(struct sample-process effectuation-process ()
  #:transparent
  #:methods gen:effectuation
  [(define (effectuation-process-target-schedules self) proc-target-schedules)
   (define (effectuation-process-output-type self) target-output)
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

; Inv. 3.6 - Verify relevance of action plans for Effectuation processes.
(define output-alt (generate-data e-proc (list (gen-body-acc)) cur-dt))
(define (verify-plan-relevance)
  (verify #:assume (assert (not (null? output)))
          #:guarantee (assert (eq? output output-alt))))

; Inv. 3.7 - Verify type of output data.
(define (verify-output-type)
  (verify (assert (implies (not (null? output))
                           (andmap (lambda (d) (or (action-instruction? d)
                                                   (control-instruction? d)))
                                   output)))))

; Inv. 3.8 - Verify implementation of proxy flag.
(define (verify-proxy-flag)
  (verify (assert (implies (not (null? output))
                           (andmap (lambda (d) (eq? (made-data-proxy-flag d)
                                                    (effectuation-process-proxy-flag e-proc)))
                                   output)))))

; Verify implementation of generate-data for Effectuation processes.
(define (filter-ext dSet dt)
  (filter-plans
   (remove-duplicates
    (filter (lambda (d)
              (not (made-data-proxy-flag d)))
            dSet))
   dt))

(define (verify-target-necessity)
  (verify
   (assert
    (implies (not (null? output))
             (ormap (lambda (target)
                      (ormap (lambda (plan)
                               (ormap (lambda (sched-inst)
                                        (cond [(scheduled-control? sched-inst)
                                               (and (eq? (target-schedule-plan-type target)
                                                         (get-type plan))
                                                    (eq? (action-plan-valid-datetime plan) cur-dt)
                                                    (eq? (target-schedule-instruction-type target)
                                                         (scheduled-control-target-process sched-inst))
                                                    ((target-schedule-instruction-predicate target) sched-inst))]

                                              [(scheduled-homogeneous-action? sched-inst)
                                               (and (eq? (target-schedule-plan-type target)
                                                         (get-type plan))
                                                    (eq? (target-schedule-instruction-type target)
                                                         (scheduled-homogeneous-action-action-type sched-inst))
                                                    (on-schedule? (scheduled-homogeneous-action-schedule sched-inst)
                                                                  cur-dt)
                                                    ((target-schedule-instruction-predicate target) sched-inst))]

                                              [(scheduled-culminating-action? sched-inst)
                                               (and (eq? (target-schedule-plan-type target)
                                                         (get-type plan))
                                                    (eq? (target-schedule-instruction-type target)
                                                         (scheduled-culminating-action-action-type sched-inst))
                                                    (on-schedule? (scheduled-culminating-action-schedule sched-inst)
                                                                  cur-dt)
                                                    ((target-schedule-instruction-predicate target) sched-inst))]))
                                      (action-plan-instruction-set plan)))
                             (filter-ext d-state cur-dt)))
                    (effectuation-process-target-schedules e-proc))))))

(define (verify-target-sufficiency)
  (verify
   (assert
    (implies (and (null? output)
                  (is-proc-activated? c-state cur-dt))
             (not (ormap (lambda (target)
                           (ormap (lambda (plan)
                                    (ormap (lambda (sched-inst)
                                             (cond [(scheduled-control? sched-inst)
                                                    (and (eq? (target-schedule-plan-type target)
                                                              (get-type plan))
                                                         (eq? (action-plan-valid-datetime plan) cur-dt)
                                                         (eq? (target-schedule-instruction-type target)
                                                              (scheduled-control-target-process sched-inst))
                                                         ((target-schedule-instruction-predicate target) sched-inst))]

                                                   [(scheduled-homogeneous-action? sched-inst)
                                                    (and (eq? (target-schedule-plan-type target)
                                                              (get-type plan))
                                                         (eq? (target-schedule-instruction-type target)
                                                              (scheduled-homogeneous-action-action-type sched-inst))
                                                         (on-schedule? (scheduled-homogeneous-action-schedule sched-inst)
                                                                       cur-dt)
                                                         ((target-schedule-instruction-predicate target) sched-inst))]

                                                   [(scheduled-culminating-action? sched-inst)
                                                    (and (eq? (target-schedule-plan-type target)
                                                              (get-type plan))
                                                         (eq? (target-schedule-instruction-type target)
                                                              (scheduled-culminating-action-action-type sched-inst))
                                                         (on-schedule? (scheduled-culminating-action-schedule sched-inst)
                                                                       cur-dt)
                                                         ((target-schedule-instruction-predicate target) sched-inst))]))
                                           (action-plan-instruction-set plan)))
                                  (filter-ext d-state cur-dt)))
                  (effectuation-process-target-schedules e-proc)))))))

(define (verify-control)
  (verify
   (assert
    (implies (and (not (null? output))
                  (control-instruction? (list-ref output 0)))
             (and (control-analyze-heart-rate? (list-ref output 0))
                  (eq? (made-data-proxy-flag (list-ref output 0))
                       proc-proxy)
                  (eq? (control-instruction-valid-datetime (list-ref output 0))
                       cur-dt)                  
                  (ormap (lambda (target)
                           (ormap (lambda (plan)
                                    (ormap (lambda (sched-inst)
                                             (and (scheduled-control? sched-inst)
                                                  (and (eq? (target-schedule-plan-type target)
                                                            (get-type plan))
                                                       (eq? (action-plan-valid-datetime plan) cur-dt)
                                                       (eq? (target-schedule-instruction-type target)
                                                            (scheduled-control-target-process sched-inst))
                                                       ((target-schedule-instruction-predicate target) sched-inst)
                                                       (eq? (control-instruction-schedule (list-ref output 0))
                                                            (scheduled-control-schedule sched-inst))
                                                       (eq? (control-instruction-status (list-ref output 0))
                                                            (scheduled-control-status sched-inst)))))
                                           (action-plan-instruction-set plan)))
                                  (filter-ext d-state cur-dt)))
                         (effectuation-process-target-schedules e-proc)))))))

(define (verify-homogeneous-action)
  (verify
   (assert
    (implies (and (not (null? output))
                  (homogeneous-action? (list-ref output 0)))
             (and (treadmill-exercise? (list-ref output 0))
                  (eq? (made-data-proxy-flag (list-ref output 0))
                       proc-proxy)
                  (eq? (homogeneous-action-start-datetime (list-ref output 0))
                       cur-dt)                  
                  (ormap (lambda (target)
                           (ormap (lambda (plan)
                                    (ormap (lambda (sched-inst)
                                             (and (scheduled-homogeneous-action? sched-inst)
                                                  (and (eq? (target-schedule-plan-type target)
                                                            (get-type plan))
                                                       (eq? (target-schedule-instruction-type target)
                                                            (scheduled-homogeneous-action-action-type sched-inst))
                                                       (on-schedule? (scheduled-homogeneous-action-schedule sched-inst)
                                                                     cur-dt)
                                                       ((target-schedule-instruction-predicate target) sched-inst)
                                                       (eq? (homogeneous-action-rate (list-ref output 0))
                                                            (scheduled-homogeneous-action-rate sched-inst))
                                                       (eq? (homogeneous-action-duration (list-ref output 0))
                                                            (scheduled-homogeneous-action-duration sched-inst)))))
                                           (action-plan-instruction-set plan)))
                                  (filter-ext d-state cur-dt)))
                         (effectuation-process-target-schedules e-proc)))))))

(define (verify-culminating-action)
  (verify
   (assert
    (implies (and (not (null? output))
                  (culminating-action? (list-ref output 0)))
             (and (ibuprofen? (list-ref output 0))
                  (eq? (made-data-proxy-flag (list-ref output 0))
                       proc-proxy)
                  (eq? (culminating-action-start-datetime (list-ref output 0))
                       cur-dt)                  
                  (ormap (lambda (target)
                           (ormap (lambda (plan)
                                    (ormap (lambda (sched-inst)
                                             (and (scheduled-culminating-action? sched-inst)
                                                  (and (eq? (target-schedule-plan-type target)
                                                            (get-type plan))
                                                       (eq? (target-schedule-instruction-type target)
                                                            (scheduled-culminating-action-action-type sched-inst))
                                                       (on-schedule? (scheduled-culminating-action-schedule sched-inst)
                                                                     cur-dt)
                                                       ((target-schedule-instruction-predicate target) sched-inst)
                                                       (eq? (culminating-action-goal-state (list-ref output 0))
                                                            (scheduled-culminating-action-goal-state sched-inst)))))
                                           (action-plan-instruction-set plan)))
                                  (filter-ext d-state cur-dt)))
                         (effectuation-process-target-schedules e-proc)))))))

; Verify implementation of filter-plans
(define (verify-filter-plans)
  (verify
   (assert
    (andmap (lambda (p1)
              (and (dt<=? (action-plan-valid-datetime p1) cur-dt)
                   (andmap (lambda (p2)
                             (or (dt<=? (action-plan-valid-datetime p2)
                                        (action-plan-valid-datetime p1))
                                 (dt>? (action-plan-valid-datetime p2)
                                        cur-dt)))
                           (filter (lambda (d)
                                     (and (action-plan? d)
                                          (not (made-data-proxy-flag d))
                                          (eq? (get-type d) (get-type p1))))
                                   d-state))))
            (filter-ext d-state cur-dt)))))
