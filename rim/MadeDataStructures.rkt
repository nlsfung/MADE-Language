#lang rosette/safe

(provide (struct-out control-instruction))

; This file contains the implementation of the 6 main datatypes in the MADE RIM,
; viz. measurement, observation, abstraction, action plan, action instruction
; and control instruction.

; Measurement is implemented as containing a valid date time and a value. Thus,
; it follows the MADE RIM with the following two exceptions:
; 1) Id is not a field as it is inherently provided by Racket.
; 2) Transaction date-time is not a field as this implementation focuses
;    on the clinical requirements only.
(struct measurement (valid-datetime value) #:transparent)

; Observation is implemented as an empty structure that is inherited by
; observed property and observed event, which in turn is implemented
; similarly to measurement.
(struct observation ())
(struct observed-property observation (valid-datetime value) #:transparent)
(struct observed-event observation (valid-datetime-range value) #:transparent)

; Abstraction is implemented similarly to observation.
(struct abstraction (valid-datetime-range value) #:transparent)

; Like observation, action instruction is implemented as an empty structure
; that is inherited by homogeneous action and culminating action, which in
; turn are implemented similarly in accordnace to the MADE RIM.
(struct action-instruction ())
(struct homogeneous-action action-instruction (start-datetime rate duration))
(struct culminating-action action-instruction (start-datetime goal-state))

; Control instruction is implemented as containing the Id of the target process,
; the valid date-time, an optional schedule and an optional process status, 
; which can take the values #t for running, #f for paused or null if not set.
(struct control-instruction (target-process valid-datetime schedule status))

; Action plan is implemented as containing a valid date-time as well as a set of
; schedule control and action instructions.
(struct action-plan (valid-datetime instruction-set))
