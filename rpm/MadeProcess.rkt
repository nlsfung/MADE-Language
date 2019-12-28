#lang rosette/safe

(require (only-in rosette string-contains?))
(require "../rim/BasicDataTypes.rkt")
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

(provide gen:made-proc execute update-data-state generate-data
         update-control-state valid-spec? proxy?)
(provide gen-proc-execute gen-proc-update-data-state
         gen-proc-generate-data gen-proc-update-control-state)
(provide is-proc-executed?)
(provide (struct-out made-process))
(provide (struct-out control-state))

; This file contains the base implementation of a generic MADE process that
; will be inherited by the different types of MADE processes.
  
; All MADE processes contain an Id, data state, control state, proxy flag and
; main body. They support four main operations: execute, update data state,
; generate data and update control state. Two helper functions, make-super-copy
; and valid-spec?, are also supported.
(define-generics made-proc
  [proxy? made-proc]
  [execute made-proc in-data datetime]
  [update-data-state made-proc in-data]
  [generate-data made-proc in-data datetime]
  [update-control-state made-proc in-data datetime]
  [valid-spec? made-proc]

  #:fallbacks
  [(define (execute made-proc in-data datetime)
     (gen-proc-execute made-proc in-data datetime))
   (define (update-data-state made-proc in-data)
     (gen-proc-update-data-state made-proc in-data))
   (define (update-control-state made-proc in-data datetime)
     (gen-proc-update-control-state made-proc in-data datetime))])

(struct made-process (data-state control-state)
  #:transparent
  #:methods gen:made-proc
  [(define (valid-spec? made-proc)
     (and (control-state? (made-process-control-state made-proc))
          (valid? (made-process-control-state made-proc))))]
  
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) made-process)
   (define (valid? self)
     (and (valid-spec? self)
          (list? (made-process-data-state self))
          (andmap (lambda (d)
                    (and (made-data? d)
                         (super-valid? d)))
                  (made-process-data-state self))))])

; Control state contains a schedule as well as a status flag which indicates
; if the process is running or paused.
(struct control-state (schedule status)
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) control-state)
   (define (valid? self)
     (and (schedule? (control-state-schedule self))
          (super-valid? (control-state-schedule self))
          (boolean? (control-state-status self))))])

; The following are generic implementations of execute, update data state,
; generate data and update control, the latter three of which must be
; specialised for each type of process.
(define (gen-proc-execute made-proc in-data datetime)
  ; Executing a process comprise three steps:
  ; 1) Generate data given the input data and current date-time.
  ; 2) Update its data state given the input and output data.
  ; 3) Update its control state given the input and output data and current date-time.
  (let* ([proc-output (generate-data made-proc in-data datetime)]
         [updated-proc (update-data-state made-proc (append in-data proc-output))]
         [new-proc (update-control-state updated-proc
                                         (append in-data proc-output)
                                         datetime)])

    ; Output the updated process and the generated data
    (list new-proc proc-output)))  

(define (gen-proc-update-data-state made-proc in-data)
  ; Updating the data state involves adding the input data into the data state,
  ; ignoring any duplicates.
  ((get-type made-proc) (remove-duplicates (append (made-process-data-state made-proc) in-data))
                        (made-process-control-state made-proc)))

(define (gen-proc-generate-data proc-func made-proc in-data datetime)
  ; Generating new data involves:
  ; 1) Checking that the input datetime lies on the process schedule.
  ; 2) Filtering out all data for which the proxy flag is set (to #t).
  ; 3) Executing the input process function (or return void if not executed). 
  (if (is-proc-executed? (made-process-control-state made-proc) datetime)
      (let ([input
             (filter (lambda (d) (not (made-data-proxy-flag d)))
                     (remove-duplicates
                      (append (made-process-data-state made-proc)
                              in-data)))])
        (proc-func input datetime))
      null))

; Helper function for determining if the process is executed or not given its
; control state as well as the current datetime.
(define (is-proc-executed? c-state datetime)
  (let ([sched (control-state-schedule c-state)]
        [stat (control-state-status c-state)])
    (and stat (on-schedule? sched datetime))))

(define (gen-proc-update-control-state made-proc in-data datetime)
  ; Updating the control state involves:
  ; 1) Finding a control instruction with the appropriate id and valid datetime
  ;    and is not a proxy.
  ; 2) Updating the control state of the process if such instruction is found.
  (let* ([old-schedule (control-state-schedule (made-process-control-state made-proc))]
         [old-status (control-state-status (made-process-control-state made-proc))]
         
         [new-control (findf (lambda (d) (and (control-instruction? d)
                                              (not (made-data-proxy-flag d))
                                              (for/all ([target (control-instruction-target-process d)])
                                                (string-contains? (format "~a" (get-type made-proc))
                                                                  (format "#<procedure:~a>" target)))
                                              (dt=? datetime
                                                    (control-instruction-valid-datetime d))))
                             in-data)]
         
         [new-schedule (if (and (control-instruction? new-control)
                                (not (void? (control-instruction-schedule new-control))))
                           (control-instruction-schedule new-control)
                           old-schedule)]
         
         [new-status (if (and (control-instruction? new-control)
                              (not (void? (control-instruction-status new-control))))
                         (control-instruction-status new-control)
                         old-status)]        
         
         [new-state (control-state new-schedule new-status)])

    ((get-type made-proc) (made-process-data-state made-proc)
                          new-state)))

;; Symbolic constants for verifying update data state and update control state
;; on a sample MADE process.
;(struct sample-process made-process ()
;  #:transparent
;  #:methods gen:typed
;  [(define (get-type self) sample-process)]
;  
;  #:methods gen:made-proc
;  [(define (proxy? self) #f)
;   (define (generate-data self in-data datetime)
;     (gen-proc-generate-data (lambda (d-state dt) #f) self in-data datetime))])   
;
;(define-symbolic p-data integer?)
;(define-symbolic p-sched p-stat integer?)
;
;(define a-proc (sample-process (list p-data) (control-state p-sched p-stat)))
;
;(define-symbolic in1 in2 integer?)
;(define in-data (list in1 in2))
;
;(define new-data-proc (update-data-state a-proc in-data))
;
;(define-symbolic t-id-num integer?)
;(define t-id (if (eq? t-id-num 0) 'sample-process t-id-num))
;(define-symbolic v-dt-h v-dt-min v-dt-s integer?)
;(define-symbolic c-inst-sched c-inst-stat integer?)
;(define-symbolic c-inst-sched-void c-inst-stat-void boolean?)
;(define-symbolic c-proxy boolean?)
;
;(define v-dt (datetime 2019 9 18 v-dt-h v-dt-min v-dt-s))
;(define c-inst (control-instruction c-proxy t-id v-dt
;                                    (if c-inst-sched-void (void) c-inst-sched)
;                                    (if c-inst-stat-void (void) c-inst-stat)))
;
;(define-symbolic dt-h dt-min dt-s integer?)
;(define cur-dt (datetime 2019 9 18 dt-h dt-min dt-s))
;
;(define new-ctrl-proc (update-control-state a-proc (list c-inst) cur-dt))
;
;; Verify implementation of update data state.
;(define (verify-update-data-state)
;  (verify (assert (implies (< (length (made-process-data-state
;                                       new-data-proc)) 3)
;                           (or (eq? p-data in1)
;                               (eq? p-data in2)
;                               (eq? in1 in2))))))
;
;; Verify implementation of update control state.
;(define (verify-update-control-state)
;  (verify #:assume (assert (and (normalized? v-dt)
;                                (normalized? cur-dt)))
;          #:guarantee (assert (implies (eq? a-proc new-ctrl-proc)
;                                       (or c-proxy
;                                           (not (dt=? v-dt cur-dt))
;                                           (not (eq? 'sample-process t-id))
;                                           (and (or (= p-sched c-inst-sched)
;                                                    c-inst-sched-void)
;                                                (or (= p-stat c-inst-stat)
;                                                    c-inst-stat-void)))))))