#lang rosette/safe

(provide
 gen:made-d get-type
 (struct-out made-data)
 (struct-out observed-property)
 (struct-out observed-event)
 (struct-out datetime-range)
 (struct-out abstraction)
 (struct-out control-instruction)
 (struct-out culminating-action)
 (struct-out homogeneous-action)
 (struct-out action-plan)
 (struct-out scheduled-control)
 (struct-out scheduled-homogeneous-action)
 (struct-out scheduled-culminating-action))

; This file contains the implementation of the 6 main datatypes in the MADE RIM,
; viz. measurement, observation, abstraction, action plan, action instruction
; and control instruction.

; Measurement is implemented as containing a valid date time and a value. Thus,
; it follows the MADE RIM with the following exceptions:
; 1) Id is not a field as it is inherently provided by Racket.
; 2) Proxy flag is added to indicate whether the data can be used automatically
;    as an input to by the MADE processes. 
; 3) Transaction date-time is not a field as this implementation focuses
;    on the clinical requirements only.
; 4) An function (required for abstractions) is added to retrieve the specific
;    structure of MADE data (e.g. blood glucose).
(define-generics made-d [get-type made-d])
(struct made-data (proxy-flag) #:transparent #:methods gen:made-d [])
(struct measurement made-data (valid-datetime value) #:transparent)

; Observation is implemented as an empty structure that is inherited by
; observed property and observed event, which in turn is implemented
; similarly to measurement.
(struct observation made-data () #:transparent)
(struct observed-property observation (valid-datetime value) #:transparent)
(struct observed-event observation (valid-datetime-range value) #:transparent)

; A date-time range comprises a start and end datetime.
(struct datetime-range (start end) #:transparent)

; Abstraction is implemented similarly to observation.
(struct abstraction made-data (valid-datetime-range value) #:transparent)

; Like observation, action instruction is implemented as an empty structure
; that is inherited by homogeneous action and culminating action, which in
; turn are implemented similarly in accordnace to the MADE RIM.
(struct action-instruction made-data () #:transparent)
(struct homogeneous-action action-instruction (start-datetime rate duration) #:transparent)
(struct culminating-action action-instruction (start-datetime goal-state) #:transparent)

; Control instruction is implemented as containing the Id of the target process,
; the valid date-time, an optional schedule and an optional process status, 
; which can take the values #t for running, #f for paused or void if not set.
(struct control-instruction made-data (target-process valid-datetime schedule status) #:transparent)

; Action plan is implemented as containing a valid date-time as well as a set of
; schedule control and action instructions.
(struct action-plan made-data (valid-datetime instruction-set) #:transparent)

; A scheduled control comprises a target process, a schedule and a status.
; The schedule or status can be optional (but not both).
(struct scheduled-control (target-process schedule status) #:transparent)

; As with action instructions, there are two types of scheduled actions,
; scheduled homogeneous and scheduled culminating actions. The former comprises
; an action type, a schedule, rate and duration, while the latter comprises
; an action type, a schedule and a goal state.
(struct scheduled-homogeneous-action (action-type schedule rate duration) #:transparent)
(struct scheduled-culminating-action (action-type schedule goal-state) #:transparent)