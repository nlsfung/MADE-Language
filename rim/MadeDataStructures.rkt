#lang rosette/safe

(require (only-in rosette symbol?))
(require "./BasicDataTypes.rkt")
(require "./TemporalDataTypes.rkt")

(provide
 (struct-out made-data)
 (struct-out measurement)
 (struct-out observation)
 (struct-out observed-property)
 (struct-out observed-event)
 (struct-out datetime-range)
 (struct-out abstraction)
 (struct-out control-instruction)
 (struct-out action-instruction)
 (struct-out culminating-action)
 (struct-out homogeneous-action)
 (struct-out action-plan)
 (struct-out scheduled-control)
 (struct-out scheduled-homogeneous-action)
 (struct-out scheduled-culminating-action)
 gen:transaction transaction-datetime)

; This file contains the implementation of the 6 main datatypes in the MADE RIM,
; viz. measurement, observation, abstraction, action plan, action instruction
; and control instruction.

; Measurement is implemented as containing a valid date time and a value. Thus,
; it follows the MADE RIM with the following exceptions:
; 1) Id is not a field as it is inherently provided by Racket.
; 2) Proxy flag is added to indicate whether the data can be used automatically
;    as an input to the MADE processes. 
; 3) Transaction date-time is not a field as this implementation focuses
;    on the clinical requirements only.
; 4) An function (required for abstractions) is added to retrieve the specific
;    structure of MADE data (e.g. blood glucose).
(define-generics transaction
  [transaction-datetime transaction])

(struct made-data (proxy-flag)
  #:transparent
  #:methods gen:typed
  [(define (get-type self) made-data)
   (define (valid? self) (boolean? (made-data-proxy-flag self)))])

(struct measurement made-data (valid-datetime value)
  #:transparent
  #:methods gen:transaction
  [(define (transaction-datetime self) (measurement-valid-datetime self))]
  
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) measurement)
   (define (valid? self) 
     (and (datetime? (measurement-valid-datetime self))
          (dimensioned? (measurement-value self))
          (super-valid? (measurement-valid-datetime self))
          (super-valid? (measurement-value self))
          (super-valid? (made-data (made-data-proxy-flag self)))))])

; Observation is implemented as an empty structure that is inherited by
; observed property and observed event, which in turn is implemented
; similarly to measurement.
(struct observation made-data () #:transparent)
(struct observed-property observation (valid-datetime value)
  #:transparent
  #:methods gen:transaction
  [(define (transaction-datetime self) (observed-property-valid-datetime self))]
  
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) observed-property)
   (define (valid? self)
     (and (datetime? (observed-property-valid-datetime self))
          (basic? (observed-property-value self))
          (super-valid? (observed-property-valid-datetime self))
          (super-valid? (observed-property-value self))
          (super-valid? (made-data (made-data-proxy-flag self)))))])

(struct observed-event observation (valid-datetime-range value)
  #:transparent
  #:methods gen:transaction
  [(define (transaction-datetime self)
     (datetime-range-start (observed-event-valid-datetime-range self)))]
  
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) observed-event)
   (define (valid? self)
     (and (datetime-range? (observed-event-valid-datetime-range self))
          (bool? (observed-event-value self))
          (super-valid? (observed-event-valid-datetime-range self))
          (super-valid? (observed-event-value self))
          (super-valid? (made-data (made-data-proxy-flag self)))))])

; A date-time range comprises a start and end datetime.
(struct datetime-range (start end)
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) datetime-range)
   (define (valid? self)
     (and (datetime? (datetime-range-start self))
          (datetime? (datetime-range-end self))
          (super-valid? (datetime-range-start self))
          (super-valid? (datetime-range-end self))
          (or (dt=? (datetime-range-start self) (datetime-range-end self))
              (dt<? (datetime-range-start self) (datetime-range-end self)))))])

; Abstraction is implemented similarly to observation.
(struct abstraction made-data (valid-datetime-range value)
  #:transparent
  #:methods gen:transaction
  [(define (transaction-datetime self)
     (datetime-range-start (abstraction-valid-datetime-range self)))]
  
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) abstraction)
   (define (valid? self)
     (and (datetime-range? (abstraction-valid-datetime-range self))
          (basic? (abstraction-value self))
          (super-valid? (abstraction-valid-datetime-range self))
          (super-valid? (abstraction-value self))
          (super-valid? (made-data (made-data-proxy-flag self)))))])

; Like observation, action instruction is implemented as an empty structure
; that is inherited by homogeneous action and culminating action, which in
; turn are implemented similarly in accordnace to the MADE RIM.
(struct action-instruction made-data () #:transparent)

(struct homogeneous-action action-instruction (start-datetime rate duration)
  #:transparent
  #:methods gen:transaction
  [(define (transaction-datetime self) (homogeneous-action-start-datetime self))]
  
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) homogeneous-action)
   (define (valid? self)
     (and (datetime? (homogeneous-action-start-datetime self))
          (dimensioned? (homogeneous-action-rate self))
          (duration? (homogeneous-action-duration self))
          (super-valid? (homogeneous-action-start-datetime self))
          (super-valid? (homogeneous-action-rate self))
          (super-valid? (homogeneous-action-duration self))
          (super-valid? (made-data (made-data-proxy-flag self)))))])

(struct culminating-action action-instruction (start-datetime goal-state)
  #:transparent
  #:methods gen:transaction
  [(define (transaction-datetime self) (culminating-action-start-datetime self))]
  
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) culminating-action)
   (define (valid? self)
     (and (datetime? (culminating-action-start-datetime self))
          (basic? (culminating-action-goal-state self))
          (super-valid? (culminating-action-start-datetime self))
          (super-valid? (culminating-action-goal-state self))
          (super-valid? (made-data (made-data-proxy-flag self)))))])

; Control instruction is implemented as containing the Id of the target process,
; the valid date-time, an optional schedule and an optional process status, 
; which can take the values #t for running, #f for paused or void if not set.
; For simplicity, the target process is identified with its constructor procedure.
(struct control-instruction made-data (target-process valid-datetime schedule status)
  #:transparent
  #:methods gen:transaction
  [(define (transaction-datetime self) (control-instruction-valid-datetime self))]
  
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) control-instruction)
   (define (valid? self)
     (and (symbol? (control-instruction-target-process self))
          (datetime? (control-instruction-valid-datetime self))
          (super-valid? (control-instruction-valid-datetime self))
          (or (and (schedule? (control-instruction-schedule self))
                   (super-valid? (control-instruction-schedule self)))
              (void? (control-instruction-schedule self)))
          (or (boolean? (control-instruction-status self))
              (void? (control-instruction-status self)))
          (implies (void? (control-instruction-schedule self))
                   (not (void? (control-instruction-status self))))
          (implies (void? (control-instruction-status self))
                   (not (void? (control-instruction-schedule self))))
          (super-valid? (made-data (made-data-proxy-flag self)))))])

; Action plan is implemented as containing a valid date-time as well as a set of
; schedule control and action instructions.
(struct action-plan made-data (valid-datetime instruction-set)
  #:transparent
  #:methods gen:transaction
  [(define (transaction-datetime self) (action-plan-valid-datetime self))]
  
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) action-plan)
   (define (valid? self)
     (and (datetime? (action-plan-valid-datetime self))
          (super-valid? (action-plan-valid-datetime self))
          (list? (action-plan-instruction-set self))
          (andmap (lambda (i)
                    (and (or (scheduled-control? i)
                             (scheduled-homogeneous-action? i)
                             (scheduled-culminating-action? i))
                         (super-valid? i)))
                  (action-plan-instruction-set self))
          (super-valid? (made-data (made-data-proxy-flag self)))))])

; A scheduled control comprises a target process, a schedule and a status.
; The schedule or status can be optional (i.e. set to void), but not both.
; For simplicity, the target process is identified with its constructor procedure.
(struct scheduled-control (target-process schedule status)
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) scheduled-control)
   (define (valid? self)
     (and (symbol? (scheduled-control-target-process self))
          (or (and (schedule? (scheduled-control-schedule self))
                   (super-valid? (scheduled-control-schedule self)))
              (void? (scheduled-control-schedule self)))
          (or (boolean? (scheduled-control-status self))
              (void? (scheduled-control-status self)))
          (implies (void? (scheduled-control-schedule self))
                   (not (void? (scheduled-control-status self))))
          (implies (void? (scheduled-control-status self))
                   (not (void? (scheduled-control-schedule self))))))])          

; As with action instructions, there are two types of scheduled actions,
; scheduled homogeneous and scheduled culminating actions. The former comprises
; an action type, a schedule, rate and duration, while the latter comprises
; an action type, a schedule and a goal state.
; For simplicity, action types are identified with their constructor procedure.
(struct scheduled-homogeneous-action (action-type schedule rate duration)
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) scheduled-homogeneous-action)
   (define (valid? self)
     (and (symbol? (scheduled-homogeneous-action-action-type self))
          (schedule? (scheduled-homogeneous-action-schedule self))
          (dimensioned? (scheduled-homogeneous-action-rate self))
          (duration? (scheduled-homogeneous-action-duration self))
          (super-valid? (scheduled-homogeneous-action-schedule self))
          (super-valid? (scheduled-homogeneous-action-rate self))
          (super-valid? (scheduled-homogeneous-action-duration self))))])

(struct scheduled-culminating-action (action-type schedule goal-state)
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) scheduled-culminating-action)
   (define (valid? self)
     (and (symbol? (scheduled-culminating-action-action-type self))
          (schedule? (scheduled-culminating-action-schedule self))
          (basic? (scheduled-culminating-action-goal-state self))
          (super-valid? (scheduled-culminating-action-schedule self))
          (super-valid? (scheduled-culminating-action-goal-state self))))])