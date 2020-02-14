#lang rosette/safe

(require "../rpm/MadeProcess.rkt"
         "../rim/BasicDataTypes.rkt"
         "../rim/TemporalDataTypes.rkt"
         "../rim/MadeDataStructures.rkt")

; Symbolic constants for verifying update data state and update control state
; on a sample MADE process.
(struct sample-process made-process ()
  #:transparent
  #:methods gen:typed
  [(define (get-type self) sample-process)]
  
  #:methods gen:made-proc
  [(define (proxy? self) #f)
   (define (generate-data self in-data datetime)
     (gen-proc-generate-data (lambda (d-state dt) #f) self in-data datetime))])   

(define-symbolic p-data integer?)
(define-symbolic p-sched p-stat integer?)

(define a-proc (sample-process (list p-data) (control-state p-sched p-stat)))

(define-symbolic in1 in2 integer?)
(define in-data (list in1 in2))

(define new-data-proc (update-data-state a-proc in-data))

(define-symbolic t-id-num integer?)
(define t-id (if (eq? t-id-num 0) 'sample-process t-id-num))
(define-symbolic v-dt-h v-dt-min v-dt-s integer?)
(define-symbolic c-inst-sched c-inst-stat integer?)
(define-symbolic c-inst-sched-void c-inst-stat-void boolean?)
(define-symbolic c-proxy boolean?)

(define v-dt (datetime 2019 9 18 v-dt-h v-dt-min v-dt-s))
(define c-inst (control-instruction c-proxy t-id v-dt
                                    (if c-inst-sched-void (void) c-inst-sched)
                                    (if c-inst-stat-void (void) c-inst-stat)))

(define-symbolic dt-h dt-min dt-s integer?)
(define cur-dt (datetime 2019 9 18 dt-h dt-min dt-s))

(define new-ctrl-proc (update-control-state a-proc (list c-inst) cur-dt))

; Verify implementation of update data state.
(define (verify-update-data-state)
  (verify (assert (implies (< (length (made-process-data-state
                                       new-data-proc)) 3)
                           (or (eq? p-data in1)
                               (eq? p-data in2)
                               (eq? in1 in2))))))

; Verify implementation of update control state.
(define (verify-update-control-state)
  (verify #:assume (assert (and (normalized? v-dt)
                                (normalized? cur-dt)))
          #:guarantee (assert (implies (eq? a-proc new-ctrl-proc)
                                       (or c-proxy
                                           (not (dt=? v-dt cur-dt))
                                           (not (eq? 'sample-process t-id))
                                           (and (or (= p-sched c-inst-sched)
                                                    c-inst-sched-void)
                                                (or (= p-stat c-inst-stat)
                                                    c-inst-stat-void)))))))
