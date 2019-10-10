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

(struct effectuation-triple (plan-type target-type instruction-type))

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
              #f)]
         [inst-instance
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
              #f)])
    (if inst-instance
        (list (action-plan-valid-datetime d) inst-instance)
        #f)))

; Symbolic constants for verifying generate data.
(define (gen-dt-part) (define-symbolic* dt-part integer?) dt-part)
(define (gen-datetime)
  (let ([hour (gen-dt-part)])
    (assert (and (>= hour 0) (< hour 24)))
    (datetime 7 10 7 hour 0 0)))

(define (gen-proxy) (define-symbolic* proxy boolean?) proxy)

(define (gen-schedule)
  (define-symbolic* repeating? boolean?)
  (define-symbolic* pat-size integer?)
  (let* ([dt1 (gen-datetime)]
         [dt2 (gen-datetime)]
         [pattern (cond [(= 1 (remainder pat-size 3)) (list dt1)]
                        [(= 2 (remainder pat-size 3)) (list dt1 dt2)]
                        [else null])]
         [status (if repeating?
                     (duration 0 (gen-dt-part) 0 0)
                     #f)])
    (assert (not (eq? dt1 dt2)))
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
    (assert (= 2 (length (remove-duplicates fever-one))))
    (assert (= 2 (length (remove-duplicates fever-two))))
    (assert (= 2 (length (remove-duplicates exercise-one))))
    (append fever-one fever-two exercise-one)))
  
(define sched-dt (gen-datetime))
(define cur-dt (gen-datetime))
(define-symbolic proc-status boolean?)
(define c-state (control-state (schedule (list sched-dt) #f) proc-status))

(define (gen-e-triple?) (define-symbolic* e-triple? boolean?) e-triple?)
(define e-proc
  (effectuation-process 'id d-state c-state (gen-proxy)
                    (append (if (gen-e-triple?)
                                (list (effectuation-triple fever-treatment?
                                                           ibuprofen
                                                           ibuprofen))
                                null)
                            (if (gen-e-triple?)
                                (list (effectuation-triple fever-treatment?
                                                           treadmill-exercise
                                                           treadmill-exercise))
                                null)
                            (if (gen-e-triple?)
                                (list (effectuation-triple fever-treatment?
                                                           analyze-heart-rate
                                                           control-analyze-heart-rate))
                                null)
                            (if (gen-e-triple?)
                                (list (effectuation-triple exercise-regimen?
                                                           treadmill-exercise
                                                           treadmill-exercise))
                                null))))

(define output (generate-data e-proc cur-dt))

; Verify implementation of extract instruction.
(define (verify-extract-instruction)
  (verify (assert (andmap (lambda (d) (andmap (lambda (e-triple) (implies (extract-instruction d (gen-datetime) e-triple (gen-proxy))
                                                                          (and ((effectuation-triple-plan-type e-triple) d)
                                                                               (findf (lambda (i) (eq? (effectuation-triple-target-type e-triple)
                                                                                                       (cond [(scheduled-control? i) (scheduled-control-target-process i)]
                                                                                                             [(scheduled-homogeneous-action? i) (scheduled-homogeneous-action-action-type i)]
                                                                                                             [(scheduled-culminating-action? i) (scheduled-culminating-action-action-type i)])))
                                                                                      (action-plan-instruction-set d)))))
                                              (effectuation-process-main-body e-proc)))
                          d-state))))
                                                