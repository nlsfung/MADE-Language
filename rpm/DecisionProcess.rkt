#lang rosette/safe

(require "./MadeProcess.rkt")
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

; This file contains the implementation of Decision processes.

; Decision process inherit from the generic MADE process, extending it with
; a main body that comprises a mapping from input abstraction values to
; plan templates that can be instantiated into action plans.
(struct decision-process made-process (main-body)
  #:transparent
  #:methods gen:made-proc
  [(define (execute self in-data datetime)
     (gen-proc-execute self in-data datetime))

   (define (update-data-state self in-data)
     (gen-proc-update-data-state self in-data))

   (define (generate-data self datetime)
     (gen-proc-generate-data
      (lambda (d-state dt)
        (execute-decision-body d-state
                               dt
                               (decision-process-main-body self)
                               (made-process-proxy-flag)))
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
           [body (decision-process-main-body self)])
       
       (decision-process id d-state c-state p-flag body)))])

(struct decision-pair (abstraction-predicate plan-template) #:transparent)

; Helper function for defining the main behaviour of decision processes.
(define (execute-decision-body d-state dt body proxy-flag)
  ; To generate output data, a Decision process follows the following steps:
  ; 1) Filter out all data that are not valid at the input date-time.
  ; 2) Find the predicate (if any) that returns true given the filtered data.
  ; 3) Retrieve the corresponding plan template.
  ; 4) Instantiate from the plan template the corresponding action plan.
  (let* ([filtered-data (filter-expired-data d-state dt)]
         [decision-pair (findf (lambda (dp)
                                 ((decision-pair-abstraction-predicate dp)
                                  filtered-data))
                               body)]
         [plan-template (if (decision-pair)
                            (cdr decision-pair)
                            (void))])
    (if (void? plan-template)
        (void)
        ((plan-template-plan-type plan-template)
         proxy-flag
         dt
         (map (lambda (inst) (instantiate inst dt))
              (plan-template-instruction-set plan-template))))))
         

; Helper function for filtering out data that not valid at the input datetime.
(define (filter-expired-data d-state dt)
  (filter
   (lambda (d)
     (and (abstraction? d)
          (dt-between? dt
                       (datetime-range-start
                        (abstraction-valid-datetime-range d))
                       (datetime-range-end
                        (abstraction-valid-datetime-range d)))))
   d-state))

; A plan template comprises a plan type and a set of three different types of
; instruction templates, one for control instructions, homogeneous action
; instructions and culminating action instructions. Each type follows the same
; structure as their instantiated counterpart, except that their schedules are
; replaced with a relative one.
(struct plan-template (plan-type instruction-set) #:transparent)
(define-generics inst-template [instantiate inst-template dt])

(struct control-template (target-process relative-schedule status)
  #:transparent
  #:methods gen:inst-template
  [(define (instantiate self dt)
     (scheduled-control
      (control-template-target-process self)
      (instantiate (control-template-relative-schedule self) dt)
      (control-template-status self)))])

(struct homogeneous-action-template (action-type relative-schedule rate duration)
  #:transparent
  #:methods gen:inst-template
  [(define (instantiate self dt)
     (scheduled-homogeneous-action
      (homogeneous-action-template-action-type self)
      (instantiate (homogeneous-action-template-relative-schedule self) dt)
      (homogeneous-action-template-rate self)
      (homogeneous-action-template-duration self)))])

(struct culminating-action-template (action-type relative-schedule goal-state)
  #:transparent
  #:methods gen:inst-template
  [(define (instantiate self dt)
     (scheduled-culminating-action
      (culminating-action-template-action-type self)
      (instantiate (culminating-action-template-relative-schedule self) dt)
      (culminating-action-template-goal-state self)))])

; A relative schedule contains a rounding factor, an offset, a relative starting
; pattern, and a repeat interval (all of which are expressed as durations).
(struct relative-schedule (rounding-factor offset relative-pattern interval)
  #:transparent
  #:methods gen:inst-template
  [(define (instantiate self dt)
     ; To following steps are required to instantiate a schedule:
     ; 1) Round the input datetime using the rounding factor.
     ; 2) Add the offset to the rounded datetime.
     ; 3) Added the offset datetime to the relative pattern.
     ; 4) Create a schedule from the interval and resulting pattern.
     (let* ([rounded-dt (dt (relative-schedule-rounding-factor self))]
            [offset-dt (dt+ rounded-dt (relative-schedule-offset self))]
            [pattern (map (lambda (dur) (dt+ offset-dt dur))
                          (relative-schedule-relative-pattern self))])
       (schedule pattern (relative-schedule-interval self))))])

; Helper function for rounding up a datetime given a rounding factor.
; The greatest non-zero unit in the rounding factor determines the
; reference at which the rounding starts.
; E.g. (round-dt (datetime 2019 9 18 6 52 0) (duration 0 7 0 0)) returns
;      (datetime 2019 9 18 7 0 0), but (round-dt (datetime 2019 9 18 6 52 0)
;      (duration 7/24 0 0 0)) returns (datetime 2019 9 18 9 0 0).
(define (round-dt dt rounding-factor)
  (let* ([round-sec (duration->second rounding-factor)]
         [dt-sec (duration->second
                  (duration (if (= (duration-day rounding-factor) 0)
                                0 (datetime-day dt))
                            (if (and (= (duration-day rounding-factor) 0)
                                     (= (duration-hour rounding-factor) 0))
                                0 (datetime-hour dt))
                            (if (and (= (duration-day rounding-factor) 0)
                                     (= (duration-hour rounding-factor) 0)
                                     (= (duration-minute rounding-factor) 0))
                                0 (datetime-minute dt))
                            (datetime-second dt)))]
         [extra-sec (- (* round-sec (ceiling (/ dt-sec round-sec))) dt-sec)])
    (dt+ dt (duration 0 0 0 extra-sec))))
