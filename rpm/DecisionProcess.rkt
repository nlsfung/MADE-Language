#lang rosette/safe

(require "./MadeProcess.rkt")
(require "./AnalysisProcess.rkt")
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
                               (made-process-proxy-flag self)))
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
         [plan-template (if decision-pair
                            (decision-pair-plan-template decision-pair)
                            (void))])
    (if (void? plan-template)
        (void)
        ((plan-template-plan-type plan-template)
         proxy-flag
         dt
         (map (lambda (inst) (instantiate inst dt))
              (plan-template-instruction-set plan-template))))))
         
; Helper function for filtering out abstractions that are:
; 1) Not valid at the input datetime.
; 2) Overriden by another abstraction.
(define (filter-expired-data d-state dt)
  (let* ([filtered-data
          (filter
           (lambda (d)
             (and (abstraction? d)
                  (dt-between?
                   dt
                   (datetime-range-start (abstraction-valid-datetime-range d))
                   (datetime-range-end (abstraction-valid-datetime-range d)))))
           d-state)]

         [sorted-data
          (sort filtered-data dt>?
                #:key (lambda (d) (datetime-range-start
                                   (abstraction-valid-datetime-range d))))]

         [type-list (remove-duplicates
                     (map (lambda (d) (get-type d)) sorted-data))])
    
    (map (lambda (t) (findf (lambda (d) (eq? (get-type d) t)) sorted-data))
         type-list)))

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
      (sched-instantiate (control-template-relative-schedule self) dt)
      (control-template-status self)))])

(struct homogeneous-action-template (action-type relative-schedule rate duration)
  #:transparent
  #:methods gen:inst-template
  [(define (instantiate self dt)
     (scheduled-homogeneous-action
      (homogeneous-action-template-action-type self)
      (sched-instantiate (homogeneous-action-template-relative-schedule self) dt)
      (homogeneous-action-template-rate self)
      (homogeneous-action-template-duration self)))])

(struct culminating-action-template (action-type relative-schedule goal-state)
  #:transparent
  #:methods gen:inst-template
  [(define (instantiate self dt)
     (scheduled-culminating-action
      (culminating-action-template-action-type self)
      (sched-instantiate (culminating-action-template-relative-schedule self) dt)
      (culminating-action-template-goal-state self)))])

; A relative schedule contains a rounding factor, an offset, a relative starting
; pattern, and a repeat interval (all of which are expressed as durations).
(define-generics rel-sched [sched-instantiate rel-sched dt])
(struct relative-schedule (rounding-factor offset relative-pattern interval)
  #:transparent
  #:methods gen:rel-sched
  [(define (sched-instantiate self dt)
     ; To following steps are required to instantiate a schedule:
     ; 1) Round the input datetime using the rounding factor.
     ; 2) Add the offset to the rounded datetime.
     ; 3) Added the offset datetime to the relative pattern.
     ; 4) instantiate a schedule from the interval and resulting pattern.
     (let* ([rounded-dur (get-round-amount dt (relative-schedule-rounding-factor self))]
            [offset-dur (relative-schedule-offset self)]
            [pattern-dt (map (lambda (dur)
                               (dt+ dt (dur+ dur (dur+ offset-dur rounded-dur))))
                             (relative-schedule-relative-pattern self))])
       (schedule pattern-dt (relative-schedule-interval self))))])

; Helper function for computing the duration to round up a datetime given a
; rounding factor. The greatest non-zero unit in the rounding factor
; determines the reference at which the rounding starts.
; E.g. (dt+ (datetime 2019 9 18 6 52 0) (get-round-amount (datetime 2019 9 18 6 52 0)
;      (duration 0 7 0 0))) returns (datetime 2019 9 18 7 0 0), but
;      (dt+ (datetime 2019 9 18 6 52 0) (get-round-amount (datetime 2019 9 18 6 52 0)
;      (duration 0 6 24 0))) returns (datetime 2019 9 18 12 48 0).
(define (get-round-amount dt rounding-factor)
  (let* ([round-sec (duration->second rounding-factor)]
         [reference (if (= round-sec 0)
                        null
                        (memf (lambda (f) (not (= 0 (f rounding-factor))))
                              (list duration-day duration-hour
                                    duration-minute duration-second)))]
         [dt-sec (if (= round-sec 0)
                     (void)
                     (duration->second
                      (duration (if (member duration-day reference)
                                    (datetime-day dt)
                                    0)
                                (if (member duration-hour reference)
                                    (datetime-hour dt)
                                    0)
                                (if (member duration-minute reference)
                                    (datetime-minute dt)
                                    0)
                                (datetime-second dt))))]
         [extra-sec (if (= round-sec 0)
                        0
                        (- (* round-sec (ceiling (/ dt-sec round-sec))) dt-sec))])
    (duration 0 0 0 extra-sec)))

; Symbolic constants for verifying generate data.
(define (gen-dt-part) (define-symbolic* dt-part integer?) dt-part)
(define (gen-datetime)
  (let ([hour (gen-dt-part)])
    (assert (and (>= hour 0) (< hour 24)))
    (datetime 7 9 21 hour 0 0)))
(define (gen-dt-range)
  (let* ([start-hour (gen-dt-part)]
         [end-hour (gen-dt-part)])
    (assert (and (>= start-hour 0) (<= start-hour end-hour) (< end-hour 24)))
    (datetime-range (datetime 7 9 21 start-hour 0 0)
                    (datetime 7 9 21 end-hour 0 0))))

(define (gen-proxy) (define-symbolic* proxy boolean?) proxy)

(define headache-grades (list 'none 'low 'medium 'high))
(define (gen-headache-grade)
  (define-symbolic* h-grade integer?)
  (assert (and (>= h-grade 0) (<= h-grade 3)))
  (list-ref headache-grades h-grade))
(struct headache-level abstraction ()
  #:transparent
  #:methods gen:made-d
  [(define (get-type self) headache-level)])

(define (gen-temp) (define-symbolic* temp integer?) temp)
(struct avg-body-temp abstraction ()
  #:transparent
  #:methods gen:made-d
  [(define (get-type self) avg-body-temp)])

(define (gen-headache-level)
  (headache-level (gen-proxy) (gen-dt-range) (gen-headache-grade)))
(define (gen-avg-body-temp)
  (avg-body-temp (gen-proxy) (gen-dt-range) (gen-temp)))

(struct ibuprofen culminating-action ())
(struct treadmill-exercise homogeneous-action ())
(struct analyze-heart-rate analysis-process ())
(struct fever-treatment action-plan ())

(define (gen-round) (define-symbolic* rounding integer?) rounding)
(define r-fact (duration (gen-round) 0 0 0))
(define offset (duration (gen-round) 0 0 0))
(define r-patt (duration (gen-round) 0 0 0))

(define ibuprofen-rel-sched
  (relative-schedule r-fact offset (list r-patt) #f))
(define ibuprofen-template
  (culminating-action-template
   ibuprofen
   ibuprofen-rel-sched
   #t))

(define treadmill-template
  (homogeneous-action-template
   treadmill-exercise
   (relative-schedule
    (duration 1 0 0 0)
    (duration 0 0 0 0)
    (list (duration 0 13 0 0)
          (duration 0 21 0 0))
    (duration 2 0 0 0))
   10
   20))

(define analyze-heart-rate-template
  (control-template
   analyze-heart-rate
   (relative-schedule (duration 0 0 0 0) (duration 0 0 0 0) null #f)
   #f))

(define fever-treatment-template-one
  (plan-template
   fever-treatment
   (list ibuprofen-template treadmill-template)))

(define fever-treatment-template-two
  (plan-template
   fever-treatment
   (list analyze-heart-rate-template)))

(define (abstraction-predicate-one d-state)
  (and (memf (lambda (d) (and (avg-body-temp? d)
                              (> (abstraction-value d) 40)))
             d-state)
       (memf (lambda (d) (and (headache-level? d)
                              (eq? 'high (abstraction-value d))))
             d-state)))

(define (abstraction-predicate-two d-state)
  (and (memf (lambda (d) (and (avg-body-temp? d)
                              (> (abstraction-value d) 37)
                              (< (abstraction-value d) 40)))
             d-state)
       (memf (lambda (d) (and (headache-level? d)
                              (not (eq? 'high (abstraction-value d)))))
             d-state)))

(define d-state
  (let* ([headaches (list (gen-headache-level) (gen-headache-level))]
         [body-temps (list (gen-avg-body-temp) (gen-avg-body-temp))])
    (assert (= 2 (length (remove-duplicates headaches))))
    (assert (= 2 (length (remove-duplicates body-temps))))
    (append headaches body-temps)))

(define sched-dt (gen-datetime))
(define cur-dt (gen-datetime))
(define-symbolic proc-status boolean?)
(define c-state (control-state (schedule (list sched-dt) #f) proc-status))

(define d-proc
  (decision-process 'id d-state c-state (gen-proxy)
                    (list (decision-pair abstraction-predicate-one
                                         fever-treatment-template-one)
                          (decision-pair abstraction-predicate-two
                                         fever-treatment-template-two))))

(define output (generate-data d-proc cur-dt))

; Verify the implementation of the data filter.
(define (verify-filtered-data-length)
  (verify (assert
           (let* ([filtered-data (filter-expired-data d-state cur-dt)])
             (and (<= (count (lambda (d) (headache-level? d)) filtered-data) 1)
                  (<= (count (lambda (d) (avg-body-temp? d)) filtered-data) 1))))))

(define (verify-filtered-data-content)
  (verify #:assume
          (assert (= 4 (length
                        (remove-duplicates
                         (map (lambda (d) (datetime-range-start
                                           (abstraction-valid-datetime-range d)))
                              d-state)))))
          
          #:guarantee
          (assert
           (let* ([filtered-data (filter-expired-data d-state cur-dt)])
             (andmap (lambda (d)
                       (implies (not (member d filtered-data))
                                (or (not (dt-between?
                                          cur-dt
                                          (datetime-range-start
                                           (abstraction-valid-datetime-range d))
                                          (datetime-range-end
                                           (abstraction-valid-datetime-range d))))
                                    (andmap (lambda (f)
                                              (or (not (eq? (get-type d) (get-type f)))
                                                  (dt>? (datetime-range-start
                                                         (abstraction-valid-datetime-range f))
                                                        (datetime-range-start
                                                         (abstraction-valid-datetime-range d)))))
                                            filtered-data))))
                     d-state)))))

; Verify implementation of schedule instantiation.
(define (verify-rounding-factor)
  (verify #:assume
          (assert
           (normalized?
            (list-ref (schedule-pattern
                       (sched-instantiate ibuprofen-rel-sched cur-dt))
                      0)))

          #:guarantee
          (assert
           (let* ([sched-patt (list-ref
                               (schedule-pattern
                                (sched-instantiate ibuprofen-rel-sched cur-dt))
                               0)])
             (implies (and (dur=? offset (duration 0 0 0 0))
                           (dur=? r-patt (duration 0 0 0 0)))
                      (and (implies (ormap (lambda (n) (dur=? r-fact (duration n 0 0 0)))
                                           (list 0 1 2 3 4 5 6 7 8 9 10))
                                    (or (dt>? sched-patt cur-dt)
                                        (dt=? sched-patt cur-dt)))
                           (implies (ormap (lambda (n) (dur=? r-fact (duration (- n) 0 0 0)))
                                           (list 0 1 2 3 4 5 6 7 8 9 10))
                                    (or (dt<? sched-patt cur-dt)
                                        (dt=? sched-patt cur-dt)))))))))

(define (verify-relative-schedule)
  (verify #:assume
          (assert
           (normalized?
            (list-ref (schedule-pattern
                       (sched-instantiate ibuprofen-rel-sched cur-dt))
                      0)))

          #:guarantee
          (assert
           (let* ([sched-patt (list-ref
                               (schedule-pattern
                                (sched-instantiate ibuprofen-rel-sched cur-dt))
                               0)])
             (implies (dur=? r-fact (duration 0 0 0 0))
                      (implies (and (ormap (lambda (n) (dur=? offset (duration n 0 0 0)))
                                           (list 0 1 2 3 4 5 6 7 8 9 10))
                                    (ormap (lambda (n) (dur=? r-patt (duration n 0 0 0)))
                                           (list 0 1 2 3 4 5 6 7 8 9 10)))
                               (dt=? sched-patt (dt+ cur-dt (dur+ offset r-patt)))))))))

; Verify the implementation of the control state.
(define (verify-is-executed)
  (verify (assert (implies (or (not (eq? sched-dt cur-dt))
                               (not proc-status))
                           (void? output)))))

; Verify the implementation of the proxy flags.
(define (verify-data-proxy)
  (verify
   (assert
    (implies (<= 3 (length (filter (lambda (d) (made-data-proxy-flag d)) d-state)))
             (void? output)))))

(define (verify-proc-proxy)
  (verify #:assume
          (assert (action-plan? output))
          #:guarantee
          (assert
           (implies (made-process-proxy-flag d-proc)
                    (made-data-proxy-flag output)))))