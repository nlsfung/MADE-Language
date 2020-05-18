#lang rosette/safe

(require "../rpm/MadeNetwork.rkt"
         "../rpm/MadeProcess.rkt"
         "../rim/BasicDataTypes.rkt"
         "../rim/TemporalDataTypes.rkt"
         "../rim/MadeDataStructures.rkt")

; This file contains the definitions and procedures used to verify all invariants related
; to MADE networks. For details of each invariant, please refer to the PhD thesis.

; Symbolic constants for verifying an example MADE network.
(struct sample-process-1 made-process ()
  #:transparent
  #:methods gen:typed
  [(define (get-type self) sample-process-1)]
  
  #:methods gen:made-proc
  [(define (proxy? self) #f)
   (define (generate-data self in-data cur-dt)
     (gen-proc-generate-data
      (lambda (dSet dt)
        (list (abstraction #f
                           (datetime-range (datetime 1 0 0 0 0 0)
                                           (datetime 1 0 0 0 0 0))
                           (proportion (length (filter (lambda (d) (observation? d)) dSet))))))
      self
      in-data
      cur-dt))])

(struct sample-process-2 made-process ()
  #:transparent
  #:methods gen:typed
  [(define (get-type self) sample-process-2)]
  
  #:methods gen:made-proc
  [(define (proxy? self) #f)
   (define (generate-data self in-data cur-dt)
     (gen-proc-generate-data
      (lambda (dSet dt)
        (list (action-plan #f
                           (datetime 1 0 0 0 0 0)
                           (proportion (- (length (filter (lambda (d) (abstraction? d)) dSet)))))))
      self
      in-data
      cur-dt))])

(define-symbolic sched-hr-1 sched-hr-2 integer?)
(define-symbolic sched-int-1 sched-int-2 proc-status-1 proc-status-2 boolean?)
(define proc-sched-1 (schedule (list (datetime 2019 9 18 sched-hr-1 0 0)) sched-int-1))
(define proc-sched-2 (schedule (list (datetime 2019 9 18 sched-hr-2 0 0)) sched-int-2))
(define c-state-1 (control-state proc-sched-1 proc-status-1))
(define c-state-2 (control-state proc-sched-2 proc-status-2))

(define-symbolic proc-data-1 real?)
(define-symbolic proc-data-2 real?)
(define d-state-1 (list (observed-property #f (datetime 2019 9 18 0 0 0) (proportion proc-data-1))))
(define d-state-2 (list (observed-property #f (datetime 2019 9 18 0 0 0) (proportion proc-data-2))))

(define proc-1 (sample-process-1 d-state-1 c-state-1))
(define proc-2 (sample-process-2 d-state-2 c-state-2))
(define sample-network (made-network null (list proc-1) (list proc-2) null))

(define-symbolic in-datum-hr-1 in-datum-hr-2 integer?)
(define-symbolic val-1 val-2 real?)
(define in-datum-1 (observed-property #f (datetime 2019 9 18 in-datum-hr-1 0 0) (proportion val-1)))
(define in-datum-2 (abstraction #f
                                (datetime-range (datetime 2019 9 18 in-datum-hr-2 0 0)
                                                (datetime 2019 9 18 in-datum-hr-2 0 0))
                                (proportion val-2)))

(define-symbolic target-proc-branch integer?)
(define-symbolic c-inst-hr c-inst-sched-hr integer?)
(define-symbolic c-inst-proxy boolean?)
(define-symbolic c-inst-sched-void c-inst-status-void boolean?)
(define-symbolic c-inst-sched-int c-inst-status boolean?)
(define target-id (if (eq? target-proc-branch 0)
                      'sample-process-1
                      'another-process))
(define c-inst-sched (schedule
                      (list (datetime 2019 9 18 c-inst-sched-hr 0 0))
                      c-inst-sched-int))
(define c-inst (control-instruction
                c-inst-proxy
                target-id
                (datetime 2019 9 18 c-inst-hr 0 0)
                (if c-inst-sched-void
                    (void)
                    c-inst-sched)
                (if c-inst-status-void
                    (void)
                    c-inst-status)))

(define in-data (list in-datum-1 in-datum-2 c-inst))

(define-symbolic dt-h dt-min dt-s integer?)
(define cur-dt (datetime 2019 9 18 dt-h dt-min dt-s))

; Inv. 3.4 - Verify the generation of data when executing a MADE network
(define exec-out (execute-network sample-network in-data cur-dt))
(define d-set-out (list-ref exec-out 1))
(define (verify-gen-data)
  (verify (assert (eq? d-set-out
                       (append (generate-data proc-1 (append in-data d-set-out) cur-dt)
                               (generate-data proc-2 (append in-data d-set-out) cur-dt))))))

; Inv. 3.5 - Verify the update of process states when executing a MADE network.
(define p-net-out (list-ref exec-out 0))
(define (verify-update-state)
  (verify
   (assert
    (eq? p-net-out
         (made-network null
                       (list (update-control-state
                              (update-data-state proc-1
                                                 (append in-data d-set-out)
                                                 cur-dt)
                              (append in-data d-set-out)
                              cur-dt))
                       (list (update-control-state
                              (update-data-state proc-2
                                                 (append in-data d-set-out)
                                                 cur-dt)
                              (append in-data d-set-out)
                              cur-dt))
                       null)))))
