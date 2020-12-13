#lang rosette/safe

(require "../rpm/MadeProcess.rkt"
         "../rim/BasicDataTypes.rkt"
         "../rim/TemporalDataTypes.rkt"
         "../rim/MadeDataStructures.rkt")

; This file contains the definitions and procedures used to verify all invariants related
; to MADE processes. For details of each invariant, please refer to the PhD thesis.

; Symbolic constants for verifying update data state and update control state
; on a sample MADE process.
(struct sample-process made-process ()
  #:transparent
  #:methods gen:typed
  [(define (get-type self) sample-process)]
  
  #:methods gen:made-proc
  [(define (proxy? self) #f)
   (define (generate-data self in-data datetime)
     (gen-proc-generate-data
      (lambda (dSet dt) (list (length dSet)))
      self
      in-data
      datetime))])

(define-symbolic sched-hr integer?)
(define-symbolic sched-int proc-status boolean?)
(define proc-sched (schedule (list (datetime 2019 9 18 sched-hr 0 0)) sched-int))
(define c-state (control-state proc-sched proc-status))

(define-symbolic proc-data real?)
(define d-state (list (observed-property #f (datetime 2019 9 18 0 0 0) (proportion proc-data))))

(define proc (sample-process d-state c-state))

(define-symbolic in-datum-hr-1 in-datum-hr-2 integer?)
(define-symbolic val-1 val-2 real?)
(define in-datum-1 (observed-property #f (datetime 2019 9 18 in-datum-hr-1 0 0) (proportion val-1)))
(define in-datum-2 (observed-property #f (datetime 2019 9 18 in-datum-hr-2 0 0) (proportion val-2)))

(define-symbolic target-proc-branch integer?)
(define-symbolic c-inst-hr c-inst-sched-hr integer?)
(define-symbolic c-inst-proxy boolean?)
(define-symbolic c-inst-sched-void c-inst-status-void boolean?)
(define-symbolic c-inst-sched-int c-inst-status boolean?)
(define target-id (if (eq? target-proc-branch 0)
                      'sample-process
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

;(assert (valid? c-state))
;(assert (andmap (lambda (d) (valid? d)) d-state))
;(assert (valid? cur-dt))
;(assert (valid? c-inst-sched))

; Inv. 3.1 - Verify implementation of execute.
(define d-set-out (generate-data proc in-data cur-dt))
(define (verify-execute)
  (verify (assert (eq? (execute proc in-data cur-dt)
                       (list (update-control-state
                              (update-data-state proc (append in-data d-set-out) cur-dt)
                              (append in-data d-set-out)
                              cur-dt)
                             d-set-out)))))

; Inv. 3.2 - Verify process behaviour when is-proc-activated? evaluates to false.
(define (verify-proc-activated-impl)
  (verify
   (assert
    (implies (not (is-proc-activated? c-state cur-dt))
             (null? d-set-out)))))

; Inv. 3.3 - Verify process behaviour when is-proc-activated? evaluates to true.
(define-symbolic sched-hr-2 integer?)
(define-symbolic sched-int-2 proc-status-2 boolean?)
(define proc-sched-2 (schedule (list (datetime 2019 9 18 sched-hr-2 0 0)) sched-int-2))
(define c-state-2 (control-state proc-sched-2 proc-status-2))

(define (verify-proc-activated)
  (verify (assert (implies (and (is-proc-activated? c-state cur-dt)
                                (is-proc-activated? c-state-2 cur-dt))
                           (eq? (generate-data proc in-data cur-dt)
                                (generate-data (sample-process d-state c-state-2) in-data cur-dt))))))

; Verify treatment of duplicate data items.
; This is an extra invariant to ensure that lists are treated as sets.
(define (verify-execute-duplicates)
  (verify (assert (implies (and (eq? in-datum-hr-1 in-datum-hr-2)
                                (eq? val-1 val-2))
                           (eq? (execute proc in-data cur-dt)
                                (execute proc (list in-datum-1 c-inst) cur-dt))))))

; Inv. 6.1 - Verify implementation of update-data-state.
(define (verify-update-data-state-impl)
  (verify (assert (implies (< (length (made-process-data-state proc-post-data))
                              (+ (length d-state) (length in-data)))
                           (or (eq? in-datum-1 in-datum-2)
                               (eq? in-datum-1 (list-ref d-state 0))
                               (eq? in-datum-2 (list-ref d-state 0)))))))

; Inv. 6.2 - Verify independence of process behaviour to re-arrangments of data state and input data.
(define d-state-2 (list in-datum-1 c-inst))
(define in-data-2 (list in-datum-2 (observed-property #f (datetime 2019 9 18 0 0 0) (proportion proc-data))))
(define proc-2 (sample-process d-state-2 c-state))
(define (verify-state-data-combo)
  (verify (assert (let* ([output (execute proc in-data cur-dt)]
                         [output-2 (execute proc-2 in-data-2 cur-dt)])
                    (implies (not (null? (list-ref output 1)))
                             (and (eq? (list-ref output 1)
                                       (list-ref output-2 1))
                                  (eq? (made-process-control-state proc)
                                       (made-process-control-state proc-2))))))))

; Inv. 6.3 - Verify implementation of is-proc-activated?
(define (verify-activated)
  (verify (assert (and (implies (is-proc-activated? c-state cur-dt)
                                (and (on-schedule? proc-sched cur-dt)
                                     proc-status))
                       (implies (and (on-schedule? proc-sched cur-dt)
                                     proc-status)
                                (is-proc-activated? c-state cur-dt))))))

; Inv. 6.4 - Verify implementation of on-schedule?
(define (verify-schedule)
  (verify (assert (eq? (on-schedule? proc-sched cur-dt)
                       (and (implies (not sched-int)
                                     (ormap (lambda (pat-dt) (dt=? cur-dt pat-dt))
                                            (schedule-pattern proc-sched)))
                            (implies sched-int
                                     (ormap (lambda (pat-dt) (dt>=? cur-dt pat-dt))
                                            (schedule-pattern proc-sched))))))))

; Inv. 6.5 and Inv. 6.6 - Verify update of control state
; Note: These two assertions are organised slightly differently than presented in the thesis.
(define (verify-update-control-state-impl)
  (verify (assert (implies (eq? proc proc-post-ctrl)
                           (or c-inst-proxy
                               (not (eq? target-id 'sample-process))
                               (not (eq? (control-instruction-valid-datetime c-inst)
                                         cur-dt))
                               (and (or c-inst-sched-void
                                        (eq? c-inst-sched proc-sched))
                                    (or c-inst-status-void
                                        (eq? c-inst-status proc-status))))))))

(define (verify-update-control-state-impl-reverse)
  (verify (assert (implies (or c-inst-proxy
                               (not (eq? target-id 'sample-process))
                               (not (dt=? (control-instruction-valid-datetime c-inst)
                                          cur-dt))
                               (and (or c-inst-sched-void
                                        (eq? c-inst-sched proc-sched))
                                    (or c-inst-status-void
                                        (eq? c-inst-status proc-status))))
                           (eq? proc proc-post-ctrl)))))

; Verify separation of concerns between data state and control state.
; These are two extra invariants to ensure that update data state does not affect
; the control state of the process and vice versa.
(define proc-post-data (update-data-state proc in-data cur-dt))
(define (verify-update-data-state)
  (verify (assert (and (sample-process? proc-post-data)
                       (eq? (made-process-control-state proc-post-data)
                            (made-process-control-state proc))))))

(define proc-post-ctrl (update-control-state proc in-data cur-dt))
(define (verify-update-control-state)
  (verify (assert (and (sample-process? proc-post-ctrl)
                       (eq? (made-process-data-state proc-post-ctrl)
                            (made-process-data-state proc))))))
