#lang rosette/safe

(require "./MadeProcess.rkt")
(require "../rim/BasicDataTypes.rkt")
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

(provide gen:effectuation
         effectuation-process-target-schedules
         effectuation-process-output-type
         effectuation-process-proxy-flag)
(provide (struct-out effectuation-process)
         (struct-out target-schedule)
         filter-plans)
(provide verify-effectuation
         (struct-out action-plan-generator)
         generate-action-plan-list
         execute-effectuation-body)

; This file contains the implementation of Effectuation processes.

; Effectuation process inherit from the generic MADE process, extending it with
; a main body that comprises a list of targets identifying which scheduled
; instructions to instantiate as well as the output type of the process.
(define-generics effectuation
  [effectuation-process-target-schedules effectuation]
  [effectuation-process-output-type effectuation]
  [effectuation-process-proxy-flag effectuation])

(struct effectuation-process made-process ()
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
  [(define (proxy? self) (effectuation-process-proxy-flag self))
   
   (define (generate-data self in-data datetime)
     (gen-proc-generate-data
      (lambda (d-list dt)
        (execute-effectuation-body
         d-list
         dt
         (effectuation-process-target-schedules self)
         (effectuation-process-output-type self)
         (effectuation-process-proxy-flag self)))
      self
      in-data
      datetime))
   
   (define/generic super-valid-spec? valid-spec?)
   (define (valid-spec? self)
     (and (super-valid-spec? (made-process null (made-process-control-state self)))
          (list? (effectuation-process-target-schedules self))
          (andmap (lambda (t) (and (target-schedule? t)
                                   (valid? t)))
                  (effectuation-process-target-schedules self))
          (procedure? (effectuation-process-output-type self))
          (boolean? (proxy? self))))])

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
(define (execute-effectuation-body d-list dt t-scheds o-type proxy-flag)
  ; To generate output data, an Effectuation process follows the following steps:
  ; 1) Filter out expired action plans
  ; 2) Identify a scheduled instruction (if any) to instantiate.
  ; 3) Instantiate it as an instruction of the output type. 
  (let* ([filtered-data (filter-plans d-list dt)]
         [scheduled-inst
          (ormap (lambda (d)
                   (ormap (lambda (t-sched)
                            (extract-instruction d dt t-sched))
                          t-scheds))
                 filtered-data)])
    (if scheduled-inst
        (cond [(scheduled-control? scheduled-inst)
               (list (o-type
                      proxy-flag
                      (scheduled-control-target-process scheduled-inst)
                      dt
                      (scheduled-control-schedule scheduled-inst)
                      (scheduled-control-status scheduled-inst)))]
              [(scheduled-homogeneous-action? scheduled-inst)
               (list (o-type
                      proxy-flag
                      dt
                      (scheduled-homogeneous-action-rate scheduled-inst)
                      (scheduled-homogeneous-action-duration scheduled-inst)))]
              [(scheduled-culminating-action? scheduled-inst)
               (list (o-type
                      proxy-flag
                      dt
                      (scheduled-culminating-action-goal-state scheduled-inst)))]
              [else null])
        null)))

; Helper function for filtering out irrelevant action plans. It involves:
; 1) Removing all data that are not action plans.
; 2) Identifying all types of action plans.
; 3) Finding for each plan type the most up-to-date plan (if any).
(define (filter-plans d-list dt)
  (let* ([plan-list (filter (lambda (d) (action-plan? d)) d-list)]
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

; verify-effectuation helps verify a Effectuation process. 
; It accepts as input:
; 1) The struct-constructor for the effectuation process.
; 2) A list of action plan generators.
; 3) The execution datetime (which can be symbolic).
; The verifier outputs a model (if any) for each of the following conditions:
; 1) The input action plans satisfy one target schedule.
;    (A seperate model is produced for each target).
; 2) The input action plans satisfy two targets with different types or targets.
(define (verify-effectuation proc-constructor plan-gen-list dt)
  (define (display-solution d-list dt sol d-crit-1 d-crit-2 output-1 output-2)
    (if (eq? d-crit-1 d-crit-2)
        (displayln (format "Model for target criterion: ~a" d-crit-1))
        (displayln (format "Model for target criteria: ~a and ~a" d-crit-1 d-crit-2)))
    (if (eq? sol (unsat))
        (displayln (unsat))
        (begin
          (displayln "Input data:")
          (displayln (evaluate d-list sol))
          (displayln "Current date-time:")
          (displayln (evaluate dt sol))
          (displayln "Output data:")
          (if (<= d-crit-1 d-crit-2)
              (displayln (evaluate output-1 sol))
              (displayln (evaluate output-2 sol)))))
    (displayln ""))
  
  (let* ([c-state (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t)]
         [proc (proc-constructor null c-state)]
         [o-type (effectuation-process-output-type proc)]
         [target-schedules (effectuation-process-target-schedules proc)]
         [proxy-flag (effectuation-process-proxy-flag proc)]
         [output-num (map (lambda (t-sched)
                            (- (length target-schedules) (length (member t-sched target-schedules))))
                          target-schedules)])

    (for-each
     (lambda (n)
       (for-each
        (lambda (m)
          (let* ([t-scheds (list (list-ref target-schedules n) (list-ref target-schedules m))]
                 [inst-types (map (lambda (t) (target-schedule-instruction-type t))
                                  t-scheds)]
                 [d-list (foldl (lambda (generator result)
                                  (append result
                                          (generate-action-plan-list
                                           (action-plan-generator-getter generator)
                                           (action-plan-generator-start-datetime generator)
                                           (action-plan-generator-end-datetime generator)
                                           (action-plan-generator-frequency generator)
                                           inst-types)))
                                null
                                plan-gen-list)]
                 [outputs (map (lambda (t)
                                 (execute-effectuation-body
                                  d-list dt (list t) o-type proxy-flag))
                               t-scheds)]
                 [output-0 (list-ref outputs 0)]
                 [output-1 (list-ref outputs 1)]
                 [sol (if (= n m)
                          (solve (assert (and (not (null? output-0))
                                              (valid? (list-ref output-0 0)))))
                          (solve (assert (and (not (null? output-0))
                                              (valid? (list-ref output-0 0))
                                              (not (null? output-1))
                                              (valid? (list-ref output-1 0))
                                              (not (eq? (list-ref inst-types 0)
                                                        (list-ref inst-types 1)))))))])
            (display-solution d-list dt sol n m output-0 output-1)
            (clear-asserts!)))
        (member n output-num)))
     output-num)))

; Action plan generator contains the specification for generating a list of
; symbolic action plans (for verification purposes). It comprises:
; 1) An action plan getter.
; 2) The earliest date-time for the action plans.
; 3) The latest date-time for the action plans.
; 4) A frequency which can either be:
;    a) A duration indicating how often the action plans should be repeated.
;    b) A positive integer indicating the number of action plans to generate.
(struct action-plan-generator
  (getter start-datetime end-datetime frequency)
  #:transparent)

; generate-action-plan generates a list of action plans. It accepts an extra
; list of symbols indicating specific instruction types to include in the plan.
; If set to #f, all instruction types will be included.
(define (generate-action-plan-list getter start-datetime end-datetime frequency target-list)
  (define (generate-count total)
    (if (or (<= total 0) (dt>? start-datetime end-datetime))
        null
        (let ([data (if (not target-list)
                        (getter start-datetime end-datetime)
                        (getter start-datetime end-datetime target-list))])
          (assert (valid? data))
          (append (list data)
                  (generate-count (- total 1))))))
  
  (define (generate-interval cur-dt)  
    (if (dt>? cur-dt end-datetime)
        null
        (let ([data (if (not target-list)
                        (getter cur-dt cur-dt)
                        (getter cur-dt cur-dt target-list))]
              [next-dt (dt+ cur-dt frequency)])
          (assert (valid? data))
          (append (list data)
                  (generate-interval next-dt)))))
  
  (let ([d-list (cond [(integer? frequency) (generate-count frequency)]
                      [(duration? frequency) (generate-interval start-datetime)])])
    (assert (eq? (length d-list)
                      (length (remove-duplicates
                               (map (lambda (d)
                                      (action-plan-valid-datetime d))
                                    d-list)))))
    d-list))
