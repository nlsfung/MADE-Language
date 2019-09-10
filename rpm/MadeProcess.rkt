#lang rosette/safe

(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

(provide gen:made-proc execute update-data-state generate-data
         update-control-state make-copy)
(provide gen-proc-execute gen-proc-update-data-state
         gen-proc-generate-data gen-proc-update-control-state)
(provide (struct-out made-process))
(provide (struct-out control-state))

; This file contains the base implementation of a generic MADE process that
; will be inherited by the different types of MADE processes.
  
; All MADE processes contain an Id, data state, control state, proxy flag and
; main body. They support four main operations: execute, update data state,
; generate data and update control state. One helper functions, make-copy,
; is also supported.
(define-generics made-proc
  [execute made-proc in-data datetime]
  [update-data-state made-proc in-data]
  [generate-data made-proc datetime]
  [update-control-state made-proc in-data datetime]
  [make-copy made-proc elem])

(struct made-process (id data-state control-state proxy-flag)
  #:transparent
  #:methods gen:made-proc [])

; Control state contains a schedule as well as a status flag which indicates
; if the process is running or paused.
(struct control-state (schedule status) #:transparent)

; The following are generic implementations of execute, update data state,
; generate data and update control, the latter three of which must be
; specialised for each type of process.
(define (gen-proc-execute self in-data datetime)
  ; Executing a process comprise three steps:
  ; 1) Updating its data state given the input data.
  ; 2) Generate data given its new data state and current date-time.
  ; 3) Update its control state given the input data and current date-time.
  (let* ([updated-proc (update-data-state self in-data)]
         [proc-output (generate-data updated-proc datetime)]
         [new-proc (update-control-state updated-proc
                                         (append in-data proc-output)
                                         datetime)])

    ; Output the updated process and the generated data
    (list new-proc proc-output)))  

(define (gen-proc-update-data-state made-proc in-data)
  ; Updating the data state involves adding the input data into the data state,
  ; ignoring any duplicates.
  (make-copy made-proc (remove-duplicates
                        (append (made-process-data-state made-proc) in-data))))

(define (gen-proc-generate-data proc-func made-proc datetime)
  ; Generating new data involves:
  ; 1) Checking if input datetime lies on the process schedule.
  ; 2) If yes, execute the input process function. Otherwise, return null. 
  (if (is-proc-executed? (made-process-control-state made-proc) datetime)
      (proc-func (made-process-data-state made-proc))
      null))

; Helper function for determining if the process is executed or not given its
; control state as well as the current datetime.
(define (is-proc-executed? c-state datetime)
  (let ([sched (control-state-schedule c-state)]
        [stat (control-state-status c-state)])
    (and stat (on-schedule? sched datetime))))
        

(define
  (gen-proc-update-control-state made-proc in-data datetime)
  ; Updating the control state involves:
  ; 1) Finding a control instruction with the appropriate id and valid datetime.
  ; 2) Updating the control state of the process if such instruction is found.
  (let* ([old-schedule (control-state-schedule (made-process-control-state made-proc))]
         [old-status (control-state-status (made-process-control-state made-proc))]
         
         [new-control (findf (lambda (d) (and (control-instruction? d)
                                              (eq? (made-process-id made-proc)
                                                   (control-instruction-target-process d))
                                              (dt=? datetime
                                                    (control-instruction-valid-datetime d))))
                             in-data)]
         
         [new-schedule (if (and (control-instruction? new-control)
                                (not (eq? null (control-instruction-schedule new-control))))
                           (control-instruction-schedule new-control)
                           old-schedule)]
         
         [new-status (if (and (control-instruction? new-control)
                              (not (eq? null (control-instruction-status new-control))))
                         (control-instruction-status new-control)
                         old-status)]        
         
         [new-state (control-state new-schedule new-status)])
    
    (make-copy made-proc new-state)))
