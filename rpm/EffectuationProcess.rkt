#lang rosette/safe

(require "./MadeProcess.rkt")
(require "./AnalysisProcess.rkt")
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

; This file contains the implementation of Effectuation processes.

; Effectuation process inherit from the generic MADE process, extending it with
; a main body that comprises a mapping from an input action plan and scheduled
; instruction to the appropriate action or control instruction type.
(struct effectuation-process made-process (main-body)
  #:transparent
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
         (effectuation-process-main-body self)
         (made-process-proxy-flag self)))
      self
      datetime))
   
   (define (update-control-state self in-data datetime)
     (gen-proc-update-control-state self in-data datetime))

   (define (make-copy self elem)
     (let ([id (made-process-id self)]
           [d-state (if (list? elem)
                        elem
                        (made-process-data-state self))]
           [c-state (if (control-state? elem)
                        elem
                        (made-process-control-state self))]
           [p-flag (made-process-proxy-flag self)]
           [body (effectuation-process-main-body self)])
       
       (effectuation-process id d-state c-state p-flag body)))])

(struct effectuation-triple (plan-type target-type instruction-type) #:transparent)

; Helper function for defining the main behaviour of effectuation processes.
(define (execute-effectuation-body d-state dt body proxy-flag)
  ; To generate output data, an Effectuation process follows the following steps:
  ; 1) Filter out data that are not of the relevant types.
  ; 2) Filter out expired action plans.
  ; 3) Extract, if possible, an instruction from the relevant action plans.
  (let* ([type-list
          (remove-duplicates
           (map (lambda (e-triple)
                  (effectuation-triple-plan-type e-triple))
                body))]
         [plan-buckets
          (map (lambda (t?) (filter (lambda (d) (t? d)) d-state)) type-list)]
         [filtered-data
          (map (lambda (d-list)
                 (argmax (lambda (d) (datetime->number (action-plan-valid-datetime d)))
                         d-list))
               type-list)])
    (findf (lambda (d)
             (findf (lambda (e-triple)
                      (extract-instruction d dt e-triple proxy-flag)) body))
           filtered-data)))

; Helper function for determining if the input data item corresponds to the
; action plan specified in the input effectuation triple, and if yes, whether
; the current datetime matches the appropriate scheduled instruction. If also
; yes, the function returns a pair containing the plan's valid datetime and the
;instantiated instruction. Otherwise the function returns false.
(define (extract-instruction d dt e-triple proxy-flag)
  (let* ([relevant-plan? ((effectuation-triple-plan-type e-triple) d)]
         [scheduled-inst
          (if relevant-plan?
              (findf (lambda (i)
                       (cond [(scheduled-control? i)
                              (eq? (effectuation-triple-target-type e-triple)
                                   (scheduled-control-target-process i))]
                             [(scheduled-homogeneous-action? i)
                              (eq? (effectuation-triple-target-type e-triple)
                                   (scheduled-homogeneous-action-action-type i))]
                             [(scheduled-culminating-action? i)
                              (eq? (effectuation-triple-target-type e-triple)
                                   (scheduled-culminating-action-action-type i))]
                             [else #f]))
                     (action-plan-instruction-set d))
              #f)]
         [is-instantiated?
          (if scheduled-inst
              (cond [(scheduled-control? scheduled-inst)
                     (dt=? (action-plan-valid-datetime d) dt)]
                    [(scheduled-homogeneous-action? scheduled-inst)
                     (on-schedule? (scheduled-homogeneous-action-schedule scheduled-inst) dt)]
                    [(scheduled-culminating-action? scheduled-inst)
                     (on-schedule? (scheduled-culminating-action-schedule scheduled-inst) dt)]
                    [else #f])
              #f)])
    (if is-instantiated?
        (cond [(scheduled-control? scheduled-inst)
               ((effectuation-triple-instruction-type e-triple)
                proxy-flag
                (scheduled-control-target-process scheduled-inst)
                dt
                (scheduled-control-schedule scheduled-inst)
                (scheduled-control-status scheduled-inst))]
              [(scheduled-homogeneous-action? scheduled-inst)
               ((effectuation-triple-instruction-type e-triple)
                proxy-flag
                dt
                (scheduled-homogeneous-action-rate scheduled-inst)
                (scheduled-homogeneous-action-duration scheduled-inst))]
              [(scheduled-culminating-action? scheduled-inst)
               ((effectuation-triple-instruction-type e-triple)
                proxy-flag
                dt
                (scheduled-culminating-action-goal-state scheduled-inst))]
              [else #f])
        #f)))

; Symbolic constants for verifying generate data.
(define (gen-dt-part) (define-symbolic* dt-part integer?) dt-part)
(define (gen-datetime)
  (let ([hour (gen-dt-part)])
    (assert (and (>= hour 0) (< hour 24)))
    (datetime 7 10 7 hour 0 0)))

(define (gen-proxy) (define-symbolic* proxy boolean?) proxy)

(define (gen-schedule)
  (let* ([pattern (list (gen-datetime))]
         [status #f])
    (schedule pattern status)))

(struct fever-treatment action-plan () #:transparent)
(struct exercise-regimen action-plan () #:transparent)
(struct ibuprofen culminating-action () #:transparent)
(struct treadmill-exercise homogeneous-action () #:transparent)
(struct control-analyze-heart-rate control-instruction () #:transparent)
(struct analyze-heart-rate analysis-process () #:transparent)

(define (gen-fever-treatment-one)
  (fever-treatment
   (gen-proxy)
   (gen-datetime)
   (list (scheduled-culminating-action
          ibuprofen (gen-schedule) #t)
         (scheduled-homogeneous-action
          treadmill-exercise (gen-schedule) 'rate 'duration))))

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
           treadmill-exercise (gen-schedule) 'rate 'duration))))

(define d-state
  (let* ([fever-one (list (gen-fever-treatment-one) (gen-fever-treatment-one))]
         [fever-two (list (gen-fever-treatment-two) (gen-fever-treatment-two))]
         [exercise-one (list (gen-exercise-regimen-one) (gen-exercise-regimen-one))])
    (assert (eq? (list-ref fever-one 0) (list-ref fever-one 1)))
    (assert (eq? (list-ref fever-two 0) (list-ref fever-two 1)))
    (assert (eq? (list-ref exercise-one 0) (list-ref exercise-one 1)))
    (append fever-one fever-two exercise-one)))
  
(define sched-dt (gen-datetime))
(define cur-dt (gen-datetime))
(define-symbolic proc-status boolean?)
(define c-state (control-state (schedule (list sched-dt) #f) proc-status))

(define (gen-e-triple?) (define-symbolic* e-triple? boolean?) e-triple?)
(define e-proc
  (effectuation-process 'id d-state c-state (gen-proxy)
                        (list (effectuation-triple fever-treatment?
                                                   ibuprofen
                                                   ibuprofen)
                              (effectuation-triple fever-treatment?
                                                   analyze-heart-rate
                                                   control-analyze-heart-rate)
                              (effectuation-triple exercise-regimen?
                                                   treadmill-exercise
                                                   treadmill-exercise))))

(define output (generate-data e-proc cur-dt))

; Verify implementation of extract instruction.
(define-symbolic x y integer?)
(define d (list-ref d-state x))
(define e-triple (list-ref (effectuation-process-main-body e-proc) y))
(define (verify-extract-instruction)
  (verify
   (assert
    (let* ([plan-match?
            ((effectuation-triple-plan-type e-triple) d)]
           [inst-matches
            (filter (lambda (i)
                     (eq? (effectuation-triple-target-type e-triple)
                          (cond [(scheduled-control? i)
                                 (scheduled-control-target-process i)]
                                [(scheduled-homogeneous-action? i)
                                 (scheduled-homogeneous-action-action-type i)]
                                [(scheduled-culminating-action? i)
                                 (scheduled-culminating-action-action-type i)])))
                   (action-plan-instruction-set d))]
           [sched-matches
            (filter (lambda (i)
                      (cond [(scheduled-control? i)
                             (dt=? (action-plan-valid-datetime d) cur-dt)]
                            [(scheduled-homogeneous-action? i)
                             (on-schedule? (scheduled-homogeneous-action-schedule i) cur-dt)]
                            [(scheduled-culminating-action? i)
                             (on-schedule? (scheduled-culminating-action-schedule i) cur-dt)]))
                    (action-plan-instruction-set d))])
      (implies (not (extract-instruction d cur-dt e-triple #f))
             (or (not plan-match?)
                 (eq? (length inst-matches)
                           (length (remove* sched-matches inst-matches)))))))))
                                                