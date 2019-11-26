#lang rosette/safe

(require "./MadeProcess.rkt")
(require "../rim/BasicDataTypes.rkt")
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

; This file contains the implementation of Effectuation processes.

; Effectuation process inherit from the generic MADE process, extending it with
; a main body that comprises a list of targets identifying which scheduled
; instructions to instantiate as well as the output type of the process. 
(struct effectuation-process made-process (target-schedules output-type)
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) effectuation-process)
   (define (valid? self)
     (and (valid-spec? self)
          (list? (made-process-data-state self))
          (andmap (lambda (d)
                    (and (made-data? d)
                         (super-valid? d)))
                  (made-process-data-state self))))]
  
  #:methods gen:made-proc
  [(define (execute self in-data datetime)
     (gen-proc-execute self in-data datetime))

   (define (update-data-state self in-data)
     (gen-proc-update-data-state self in-data))

   (define (generate-data self datetime)
     (gen-proc-generate-data
      (lambda (d-state dt)
        (execute-effectuation-body
         d-state
         dt
         (effectuation-process-target-schedules self)
         (effectuation-process-output-type self)
         (made-process-proxy-flag self)))
      self
      datetime))
   
   (define (update-control-state self in-data datetime)
     (gen-proc-update-control-state self in-data datetime))

   (define (make-copy self elem)
     (let ([d-state (if (list? elem)
                        elem
                        (made-process-data-state self))]
           [c-state (if (control-state? elem)
                        elem
                        (made-process-control-state self))]
           [p-flag (made-process-proxy-flag self)]
           [t-scheds (effectuation-process-target-schedules self)]
           [o-type (effectuation-process-output-type self)])
       
       (effectuation-process d-state c-state p-flag t-scheds o-type)))

   (define/generic super-valid-spec? valid-spec?)
   (define (valid-spec? self)
     (and (super-valid-spec? (made-process null
                                           (made-process-control-state self)
                                           (made-process-proxy-flag self)))
          (list? (effectuation-process-target-schedules self))
          (andmap (lambda (t) (and (target-schedule? t)
                                   (valid? t)))
                  (effectuation-process-target-schedules self))
          (procedure? (effectuation-process-output-type self))))])

; Target schedules are identified by a plan type, an instruction type (i.e. an
; action or target process type) as well as a predicate on the properties of
; the target scheduled instruction.
(struct target-schedule (plan-type instruction-type instruction-predicate)
  #:transparent
  #:methods gen:typed
  [(define (get-type self) target-schedule)
   (define (valid? self)
     (and (procedure? (target-schedule-plan-type self))
          (procedure? (target-schedule-instruction-type self))
          (procedure? (target-schedule-instruction-predicate self))))])

; Helper function for defining the main behaviour of effectuation processes.
(define (execute-effectuation-body d-state dt t-scheds o-type proxy-flag)
  ; To generate output data, an Effectuation process follows the following steps:
  ; 1) Filter out expired action plans
  ; 2) Identify a scheduled instruction (if any) to instantiate.
  ; 3) Instantiate it as an instruction of the output type. 
  (let* ([filtered-data (filter-expired-data d-state dt)]
         [scheduled-inst
          (ormap (lambda (d)
                   (ormap (lambda (t-sched)
                            (extract-instruction d dt t-sched))
                          t-scheds))
                 filtered-data)])
    (if scheduled-inst
        (cond [(scheduled-control? scheduled-inst)
               (o-type
                proxy-flag
                (scheduled-control-target-process scheduled-inst)
                dt
                (scheduled-control-schedule scheduled-inst)
                (scheduled-control-status scheduled-inst))]
              [(scheduled-homogeneous-action? scheduled-inst)
               (o-type
                proxy-flag
                dt
                (scheduled-homogeneous-action-rate scheduled-inst)
                (scheduled-homogeneous-action-duration scheduled-inst))]
              [(scheduled-culminating-action? scheduled-inst)
               (o-type
                proxy-flag
                dt
                (scheduled-culminating-action-goal-state scheduled-inst))]
              [else (void)])
        (void))))

; Helper function for filtering out irrelevant action plans. It involves:
; 1) Removing all data that are not action plans.
; 2) Identifying all types of action plans.
; 3) Finding for each plan type the most up-to-date plan (if any).
(define (filter-expired-data d-state dt)
  (let* ([plan-list (filter (lambda (d) (action-plan? d)) d-state)]
         [type-list (remove-duplicates
                     (map (lambda (d) (get-type d)) plan-list))]
         [plan-buckets
          (filter (lambda (d-list) (< 0 (length d-list)))
                  (map (lambda (t)
                         (filter (lambda (d)
                                   (and (eq? t (get-type d))
                                        (<= (datetime->number (action-plan-valid-datetime d))
                                            (datetime->number dt))))
                                 plan-list))
                       type-list))])
    (map (lambda (d-list)
           (argmax (lambda (d) (datetime->number (action-plan-valid-datetime d)))
                   d-list))
         plan-buckets)))         
  
; Helper function for determining if the input data item corresponds to the
; action plan specified in the input target schedule, and if yes, whether
; the current datetime matches the appropriate scheduled instruction. The 
; function returns the scheduled instruction that meets all these conditions.
(define (extract-instruction d dt t-sched)
  (let* ([relevant-plan? (eq? (target-schedule-plan-type t-sched) (get-type d))]
         [scheduled-inst
          (if relevant-plan?
              (findf (lambda (i)
                       (cond [(scheduled-control? i)
                              (and (eq? (target-schedule-instruction-type t-sched)
                                        (scheduled-control-target-process i))
                                   ((target-schedule-instruction-predicate t-sched) i)
                                   (dt=? (action-plan-valid-datetime d) dt))]
                             [(scheduled-homogeneous-action? i)
                              (and (eq? (target-schedule-instruction-type t-sched)
                                        (scheduled-homogeneous-action-action-type i))
                                   ((target-schedule-instruction-predicate t-sched) i)
                                   (on-schedule? (scheduled-homogeneous-action-schedule i) dt))]
                             [(scheduled-culminating-action? i)
                              (and (eq? (target-schedule-instruction-type t-sched)
                                        (scheduled-culminating-action-action-type i))
                                   ((target-schedule-instruction-predicate t-sched) i)
                                   (on-schedule? (scheduled-culminating-action-schedule i) dt))]
                             [else #f]))
                     (action-plan-instruction-set d))
              #f)])
    scheduled-inst))

;; Symbolic constants for verifying generate data.
;(require "./AnalysisProcess.rkt")
;(define (gen-dt-part) (define-symbolic* dt-part integer?) dt-part)
;(define (gen-datetime)
;  (let ([hour (gen-dt-part)])
;    (assert (and (>= hour 0) (< hour 24)))
;    (datetime 7 10 7 hour 0 0)))
;
;(define (gen-proxy) (define-symbolic* proxy boolean?) proxy)
;
;(define (gen-schedule)
;  (let* ([pattern (list (gen-datetime) (gen-datetime))]
;         [status #f])
;    (schedule pattern status)))
;
;(struct fever-treatment action-plan ()
;  #:transparent
;  #:methods gen:typed
;  [(define/generic super-valid? valid?)
;   (define (get-type self) fever-treatment)
;   (define (valid? self)
;     (super-valid? (action-plan (made-data-proxy-flag self)
;                                (action-plan-valid-datetime self)
;                                (action-plan-instruction-set self))))])
;
;(struct exercise-regimen action-plan ()
;  #:transparent
;  #:methods gen:typed
;  [(define/generic super-valid? valid?)
;   (define (get-type self) exercise-regimen)
;   (define (valid? self)
;     (super-valid? (action-plan (made-data-proxy-flag self)
;                                (action-plan-valid-datetime self)
;                                (action-plan-instruction-set self))))])
;(struct ibuprofen culminating-action () #:transparent)
;(struct treadmill-exercise homogeneous-action () #:transparent)
;(struct control-analyze-heart-rate control-instruction () #:transparent)
;(struct analyze-heart-rate analysis-process () #:transparent)
;
;(define (gen-fever-treatment-one)
;  (fever-treatment
;   (gen-proxy)
;   (gen-datetime)
;   (list (scheduled-culminating-action
;          ibuprofen (gen-schedule) (bool #t))
;         (scheduled-homogeneous-action
;          treadmill-exercise
;          (gen-schedule)
;          (dimensioned 10 'units)
;          (duration 0 1 0 0)))))
;
;(define (gen-fever-treatment-two)
;  (fever-treatment
;   (gen-proxy)
;   (gen-datetime)
;   (list (scheduled-control
;          analyze-heart-rate (gen-schedule) (void)))))
;
;(define (gen-exercise-regimen-one)
;   (exercise-regimen
;    (gen-proxy)
;    (gen-datetime)
;    (list (scheduled-homogeneous-action
;           treadmill-exercise
;           (gen-schedule)
;           (dimensioned 15 'units)
;           (duration 0 2 0 0)))))
;
;(define d-state
;  (let* ([fever-one (list (gen-fever-treatment-one) (gen-fever-treatment-one))]
;         [fever-two (list (gen-fever-treatment-two) (gen-fever-treatment-two))]
;         [exercise-one (list (gen-exercise-regimen-one) (gen-exercise-regimen-one))])
;    (assert (not (eq? (list-ref fever-one 0) (list-ref fever-one 1))))
;    (assert (not (eq? (list-ref fever-two 0) (list-ref fever-two 1))))
;    (assert (not (eq? (list-ref exercise-one 0) (list-ref exercise-one 1))))
;    (append fever-one fever-two exercise-one)))
;  
;(define sched-dt (gen-datetime))
;(define cur-dt (gen-datetime))
;(define-symbolic proc-status boolean?)
;(define c-state (control-state (schedule (list sched-dt) #f) proc-status))
;
;(define (gen-inst-pred)
;  (define-symbolic* inst-pred boolean?)
;  (lambda (i) inst-pred))
;
;(define e-proc
;  (effectuation-process d-state c-state (gen-proxy)
;                        (list (target-schedule fever-treatment
;                                               treadmill-exercise
;                                               (gen-inst-pred))
;                              (target-schedule exercise-regimen
;                                               treadmill-exercise
;                                               (gen-inst-pred)))
;                        treadmill-exercise))
;
;(define output (generate-data e-proc cur-dt))
;
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
;             (void? output)))))
;
;(define (verify-proc-proxy)
;  (verify #:assume
;          (assert (not (void? output)))
;          #:guarantee
;          (assert
;           (implies (made-process-proxy-flag e-proc)
;                    (made-data-proxy-flag output)))))
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
;                  (made-process-proxy-flag e-proc)))
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
;   (assert (implies (and (not (void? output))
;                         (not (void? output-2))
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
;                                            (made-process-proxy-flag e-proc)))
;                                          (> (length (member d d-state))
;                                             (length (member d-2 d-state))))))
;                               d-state))))))
