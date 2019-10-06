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
  ; To generate output data, a Decision process follows the following steps:
  ; 1) Extract, if possible, an instruction from each entry in the data state.
  ;    The generated instruction is paired with the valid datetime of its plan.
  ; 2) Filter out failed attempts.
  ; 3) Return the instruction that was derived from an action plan with the
  ;    latest valid datetime.
  (let* ([filtered-insts
          (filter-map
           (lambda (d) (findf (lambda (e-triple)
                                (extract-instruction d dt e-triple proxy-flag))
                              body))
           d-state)])
    (second (argmax (lambda (d) (datetime->number (first d))) filtered-insts))))

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
