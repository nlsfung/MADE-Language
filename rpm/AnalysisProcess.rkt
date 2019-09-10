#lang rosette/safe

(require "./MadeProcess.rkt")

; This file contains the implementation of Analysis processes.

; Analysis process inherit from the generic MADE process, extending it with
; a time window, the output type identifier and a main function body.
(struct analysis-process made-process (time-window output-type main-body)
  #:transparent
  #:methods gen:made-proc
  [(define (execute self in-data datetime)
     (gen-proc-execute self in-data datetime))

   (define (update-data-state self in-data)
     (gen-proc-update-data-state self in-data))

   (define (generate-data self datetime)
     (gen-proc-generate-data (lambda (p) #f) self datetime))
   
   (define (update-control-state self in-data datetime)
     (gen-proc-update-control-state self in-data datetime))

   (define (make-copy self elem)
     (let ([id (made-process-id self)]
           [d-state (if (list? elem) elem (made-process-data-state self))]
           [c-state (if (control-state? elem) elem (made-process-control-state self))]
           [p-flag (made-process-proxy-flag self)]
           [t-window (analysis-process-time-window self)]
           [out-type (analysis-process-output-type self)]
           [body (analysis-process-main-body self)])
       
       (analysis-process id d-state c-state p-flag t-window out-type body)))])

; Requirements for testing analysis processes
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

; Symbolic constants for verifying the implementation.
(define-symbolic p-id integer?)
(define-symbolic p-data integer?)
(define-symbolic p-sched p-stat integer?)
(define a-proc (analysis-process p-id (list p-data) (control-state p-sched p-stat) 1 2 3 4))

(define-symbolic in1 in2 integer?)
(define in-data (list in1 in2))

(define new-data-proc (update-data-state a-proc in-data))

(define-symbolic t-id integer?)
(define-symbolic v-dt-y v-dt-mth v-dt-d v-dt-h v-dt-min v-dt-s integer?)
(define-symbolic c-inst-sched c-inst-stat integer?)
(define v-dt (datetime v-dt-y v-dt-mth v-dt-d v-dt-h v-dt-min v-dt-s))
(define c-inst (control-instruction t-id v-dt c-inst-sched c-inst-stat))

(define-symbolic dt-y dt-mth dt-d dt-h dt-min dt-s integer?)
(define cur-dt (datetime dt-y dt-mth dt-d dt-h dt-min dt-s))

(define new-ctrl-proc (update-control-state a-proc (list c-inst) cur-dt))

; Verify implementation of update data state.
(define (verify-update-data-state)
  (verify (assert (implies (< (length (made-process-data-state
                                       new-data-proc)) 3)
                           (or (= p-data in1)
                               (= p-data in2)
                               (= in1 in2))))))

; Verify implementation of update control state.
; Executed with bitwidth 10.
(define (verify-update-control-state)
  (verify #:assume (assert (and (normalized? v-dt)
                                (normalized? cur-dt)))
          #:guarantee (assert (implies (eq? a-proc new-ctrl-proc)
                                       (or (not (dt=? v-dt cur-dt))
                                           (not (= p-id t-id))
                                           (and (= p-sched c-inst-sched)
                                                (= p-stat c-inst-stat)))))))
