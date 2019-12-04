#lang rosette/safe

(require "./MadeProcess.rkt")
(require "./AnalysisProcess.rkt")
(require "../rim/BasicDataTypes.rkt")
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

; This file contains the implementation of Decision processes.

; Decision process inherit from the generic MADE process, extending it with
; a main body that comprises a list of decision criterion and a plan template 
; that can be instantiated into action plans.
(define-generics decision
  [decision-process-plan-template decision]
  [decision-process-decision-criteria decision]
  [decision-process-proxy-flag decision])

(struct decision-process made-process ()
  #:transparent
  #:methods gen:decision []
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) decision-process)
   (define (valid? self)
     (and (valid-spec? self)
          (list? (made-process-data-state self))
          (andmap (lambda (d)
                    (and (made-data? d)
                         (super-valid? d)))
                  (made-process-data-state self))))]
  
  #:methods gen:made-proc
  [(define (proxy? self) (decision-process-proxy-flag self))
   
   (define (generate-data self in-data datetime)
     (gen-proc-generate-data
      (lambda (d-list dt)
        (execute-decision-body d-list
                               dt
                               (decision-process-plan-template self)
                               (decision-process-decision-criteria self)
                               (decision-process-proxy-flag self)))
      self
      in-data
      datetime))

   (define/generic super-valid-spec? valid-spec?)
   (define (valid-spec? self)
     (and (super-valid-spec? (made-process null (made-process-control-state self)))
          (plan-template? (decision-process-plan-template self))
          (valid? (decision-process-plan-template self))
          (list? (decision-process-decision-criteria self))
          (andmap (lambda (c) (procedure? c)) (decision-process-decision-criteria self))
          (boolean? (proxy? self))))])

; Helper function for defining the main behaviour of decision processes.
(define (execute-decision-body d-list dt p-temp d-crit proxy-flag)
  ; To generate output data, a Decision process follows the following steps:
  ; 1) Filter out all data that are not valid at the input date-time.
  ; 2) Check if any decision criterion (if any) returns true given the data.
  ; 3) Instantiate from the plan template the corresponding action plan.
  (let* ([filtered-data (filter-expired-data d-list dt)]
         [selected-crit (findf (lambda (c) (c filtered-data)) d-crit)])
    (if selected-crit
        (list (plan-instantiate p-temp dt proxy-flag))
        null)))
         
; Helper function for filtering out abstractions that are:
; 1) Not valid at the input datetime.
; 2) Overriden by another abstraction.
(define (filter-expired-data d-list dt)
  (let* ([filtered-data
          (filter
           (lambda (d)
             (and (abstraction? d)
                  (dt-between?
                   dt
                   (datetime-range-start (abstraction-valid-datetime-range d))
                   (datetime-range-end (abstraction-valid-datetime-range d)))))
           d-list)]

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
(define-generics plan-temp [plan-instantiate plan-temp dt proxy-flag])
(struct plan-template (plan-type instruction-set)
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) plan-template)
   (define (valid? self)
     (and (procedure? (plan-template-plan-type self))
          (list? (plan-template-instruction-set self))
          (andmap (lambda (i) (and (inst-template? i)
                                   (super-valid? i)))
                  (plan-template-instruction-set self))))]
  
  #:methods gen:plan-temp
  [(define (plan-instantiate self dt proxy-flag)
     ((plan-template-plan-type self)
         proxy-flag
         dt
         (map (lambda (inst) (instantiate inst dt))
              (plan-template-instruction-set self))))])


(define-generics inst-template [instantiate inst-template dt])
(struct control-template (target-process relative-schedule status)
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) control-template)
   (define (valid? self)
     (and (procedure? (control-template-target-process self))
          (or (and (relative-schedule? (control-template-relative-schedule self))
                   (super-valid? (control-template-relative-schedule self)))
              (void? (control-template-relative-schedule self)))
          (or (boolean? (control-template-status self))
              (void? (control-template-status self)))
          (implies (void? (control-template-relative-schedule self))
                   (not (void? (control-template-status self))))
          (implies (void? (control-template-status self))
                   (not (void? (control-template-relative-schedule self))))))]
  
  #:methods gen:inst-template
  [(define (instantiate self dt)
     (scheduled-control
      (control-template-target-process self)
      (if (relative-schedule? (control-template-relative-schedule self))
          (sched-instantiate (control-template-relative-schedule self) dt)
          (control-template-relative-schedule self))
      (control-template-status self)))])

(struct homogeneous-action-template (action-type relative-schedule rate duration)
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) homogeneous-action-template)
   (define (valid? self)
     (and (procedure? (homogeneous-action-template-action-type self))
          (relative-schedule? (homogeneous-action-template-relative-schedule self))
          (super-valid? (homogeneous-action-template-relative-schedule self))
          (dimensioned? (homogeneous-action-template-rate self))
          (super-valid? (homogeneous-action-template-rate self))
          (duration? (homogeneous-action-template-duration self))
          (super-valid? (homogeneous-action-template-duration self))))]
  
  #:methods gen:inst-template
  [(define (instantiate self dt)
     (scheduled-homogeneous-action
      (homogeneous-action-template-action-type self)
      (sched-instantiate (homogeneous-action-template-relative-schedule self) dt)
      (homogeneous-action-template-rate self)
      (homogeneous-action-template-duration self)))])

(struct culminating-action-template (action-type relative-schedule goal-state)
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) culminating-action-template)
   (define (valid? self)
     (and (procedure? (culminating-action-template-action-type self))
          (relative-schedule? (culminating-action-template-relative-schedule self))
          (super-valid? (culminating-action-template-relative-schedule self))
          (basic? (culminating-action-template-goal-state self))
          (super-valid? (culminating-action-template-goal-state self))))]
  
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
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) relative-schedule)
   (define (valid? self)
     (and (duration? (relative-schedule-rounding-factor self))
          (super-valid? (relative-schedule-rounding-factor self))
          (duration? (relative-schedule-offset self))
          (super-valid? (relative-schedule-offset self))
          (list? (relative-schedule-relative-pattern self))
          (andmap (lambda (p) (and (duration? p)
                                   (super-valid? p)))
                  (relative-schedule-relative-pattern self))
          (or (and (duration? (relative-schedule-interval self))
                   (super-valid? (relative-schedule-interval self)))
              (eq? (relative-schedule-interval self) #f))))]
  
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

;; Symbolic constants for verifying generate data.
;(define (gen-dt-part) (define-symbolic* dt-part integer?) dt-part)
;(define (gen-datetime)
;  (let ([hour (gen-dt-part)])
;    (assert (and (>= hour 0) (< hour 24)))
;    (datetime 7 9 21 hour 0 0)))
;(define (gen-dt-range)
;  (let* ([start-hour (gen-dt-part)]
;         [end-hour (gen-dt-part)])
;    (assert (and (>= start-hour 0) (<= start-hour end-hour) (< end-hour 24)))
;    (datetime-range (datetime 7 9 21 start-hour 0 0)
;                    (datetime 7 9 21 end-hour 0 0))))
;
;(define (gen-proxy) (define-symbolic* proxy boolean?) proxy)
;
;(define headache-grades (list 'none 'low 'medium 'high))
;(define (gen-headache-grade)
;  (define-symbolic* h-grade integer?)
;  (assert (and (>= h-grade 0) (<= h-grade 3)))
;  (list-ref headache-grades h-grade))
;(struct headache-level abstraction ()
;  #:transparent
;  #:methods gen:typed
;  [(define (get-type self) headache-level)])
;
;(define (gen-temp) (define-symbolic* temp integer?) temp)
;(struct avg-body-temp abstraction ()
;  #:transparent
;  #:methods gen:typed
;  [(define (get-type self) avg-body-temp)])
;
;(define (gen-headache-level)
;  (headache-level (gen-proxy) (gen-dt-range) (gen-headache-grade)))
;(define (gen-avg-body-temp)
;  (avg-body-temp (gen-proxy) (gen-dt-range) (gen-temp)))
;
;(struct ibuprofen culminating-action () #:transparent)
;(struct treadmill-exercise homogeneous-action () #:transparent)
;(struct analyze-heart-rate analysis-process () #:transparent)
;(struct fever-treatment action-plan () #:transparent)
;
;(define (gen-round) (define-symbolic* rounding integer?) rounding)
;(define r-fact (duration (gen-round) 0 0 0))
;(define offset (duration (gen-round) 0 0 0))
;(define r-patt (duration (gen-round) 0 0 0))
;
;(define ibuprofen-rel-sched
;  (relative-schedule r-fact offset (list r-patt) #f))
;(define ibuprofen-template
;  (culminating-action-template
;   ibuprofen
;   ibuprofen-rel-sched
;   (bool #t)))
;
;(define treadmill-template
;  (homogeneous-action-template
;   treadmill-exercise
;   (relative-schedule
;    (duration 1 0 0 0)
;    (duration 0 0 0 0)
;    (list (duration 0 13 0 0)
;          (duration 0 21 0 0))
;    (duration 2 0 0 0))
;   (dimensioned 10 'units)
;   (duration 0 1 0 0)))
;
;(define analyze-heart-rate-template
;  (control-template
;   analyze-heart-rate
;   (relative-schedule (duration 0 0 0 0) (duration 0 0 0 0) null #f)
;   #f))
;
;(define fever-treatment-template
;  (plan-template
;   fever-treatment
;   (list ibuprofen-template treadmill-template analyze-heart-rate-template)))
;
;(define (decision-criterion-one d-state)
;  (and (memf (lambda (d) (and (avg-body-temp? d)
;                              (> (abstraction-value d) 37)))
;             d-state)
;       (memf (lambda (d) (and (headache-level? d)
;                              (eq? 'high (abstraction-value d))))
;             d-state)))
;
;(define (decision-criterion-two d-state)
;  (memf (lambda (d) (and (avg-body-temp? d)
;                              (> (abstraction-value d) 40))
;             d-state)))
;
;(define d-state
;  (let* ([headaches (list (gen-headache-level) (gen-headache-level))]
;         [body-temps (list (gen-avg-body-temp) (gen-avg-body-temp))])
;    (assert (= 2 (length (remove-duplicates headaches))))
;    (assert (= 2 (length (remove-duplicates body-temps))))
;    (append headaches body-temps)))
;
;(define sched-dt (gen-datetime))
;(define cur-dt (gen-datetime))
;(define-symbolic proc-status boolean?)
;(define c-state (control-state (schedule (list sched-dt) #f) proc-status))
;(define proc-proxy (gen-proxy))
;
;(struct sample-process decision-process ()
;  #:transparent
;  #:methods gen:decision
;  [(define (decision-process-plan-template self) fever-treatment-template)
;   (define (decision-process-decision-criteria self)
;     (list decision-criterion-one decision-criterion-two))
;   (define (decision-process-proxy-flag self) proc-proxy)]
;
;  #:methods gen:typed
;  [(define/generic super-valid? valid?)
;   (define (get-type self) sample-process)
;   (define (valid? self)
;     (and (valid-spec? self)
;          (super-valid? (made-process (made-process-data-state self)
;                                      (made-process-control-state self)))))])
;
;(define d-proc (sample-process d-state c-state))
;
;(define output (generate-data d-proc null cur-dt))
;
;; Verify the implementation of the data filter.
;(define (verify-filtered-data-length)
;  (verify (assert
;           (let* ([filtered-data (filter-expired-data d-state cur-dt)])
;             (and (<= (count (lambda (d) (headache-level? d)) filtered-data) 1)
;                  (<= (count (lambda (d) (avg-body-temp? d)) filtered-data) 1))))))
;
;(define (verify-filtered-data-content)
;  (verify #:assume
;          (assert (= 4 (length
;                        (remove-duplicates
;                         (map (lambda (d) (datetime-range-start
;                                           (abstraction-valid-datetime-range d)))
;                              d-state)))))
;          
;          #:guarantee
;          (assert
;           (let* ([filtered-data (filter-expired-data d-state cur-dt)])
;             (andmap (lambda (d)
;                       (implies (not (member d filtered-data))
;                                (or (not (dt-between?
;                                          cur-dt
;                                          (datetime-range-start
;                                           (abstraction-valid-datetime-range d))
;                                          (datetime-range-end
;                                           (abstraction-valid-datetime-range d))))
;                                    (andmap (lambda (f)
;                                              (or (not (eq? (get-type d) (get-type f)))
;                                                  (dt>? (datetime-range-start
;                                                         (abstraction-valid-datetime-range f))
;                                                        (datetime-range-start
;                                                         (abstraction-valid-datetime-range d)))))
;                                            filtered-data))))
;                     d-state)))))
;
;; Verify implementation of schedule instantiation.
;(define (verify-rounding-factor)
;  (verify #:assume
;          (assert
;           (normalized?
;            (list-ref (schedule-pattern
;                       (sched-instantiate ibuprofen-rel-sched cur-dt))
;                      0)))
;
;          #:guarantee
;          (assert
;           (let* ([sched-patt (list-ref
;                               (schedule-pattern
;                                (sched-instantiate ibuprofen-rel-sched cur-dt))
;                               0)])
;             (implies (and (dur=? offset (duration 0 0 0 0))
;                           (dur=? r-patt (duration 0 0 0 0)))
;                      (and (implies (ormap (lambda (n) (dur=? r-fact (duration n 0 0 0)))
;                                           (list 0 1 2 3 4 5 6 7 8 9 10))
;                                    (or (dt>? sched-patt cur-dt)
;                                        (dt=? sched-patt cur-dt)))
;                           (implies (ormap (lambda (n) (dur=? r-fact (duration (- n) 0 0 0)))
;                                           (list 0 1 2 3 4 5 6 7 8 9 10))
;                                    (or (dt<? sched-patt cur-dt)
;                                        (dt=? sched-patt cur-dt)))))))))
;
;(define (verify-relative-schedule)
;  (verify #:assume
;          (assert
;           (normalized?
;            (list-ref (schedule-pattern
;                       (sched-instantiate ibuprofen-rel-sched cur-dt))
;                      0)))
;
;          #:guarantee
;          (assert
;           (let* ([sched-patt (list-ref
;                               (schedule-pattern
;                                (sched-instantiate ibuprofen-rel-sched cur-dt))
;                               0)])
;             (implies (dur=? r-fact (duration 0 0 0 0))
;                      (implies (and (ormap (lambda (n) (dur=? offset (duration n 0 0 0)))
;                                           (list 0 1 2 3 4 5 6 7 8 9 10))
;                                    (ormap (lambda (n) (dur=? r-patt (duration n 0 0 0)))
;                                           (list 0 1 2 3 4 5 6 7 8 9 10)))
;                               (dt=? sched-patt (dt+ cur-dt (dur+ offset r-patt)))))))))
;
;; Verify the implementation of the control state.
;(define (verify-is-executed)
;  (verify (assert (implies (or (not (eq? sched-dt cur-dt))
;                               (not proc-status))
;                           (null? output)))))
;
;; Verify the implementation of the proxy flags.
;(define (verify-data-proxy)
;  (verify
;   (assert
;    (implies (<= 3 (length (filter (lambda (d) (made-data-proxy-flag d)) d-state)))
;             (null? output)))))
;
;(define (verify-proc-proxy)
;  (verify #:assume
;          (assert (not (null? output)))
;          #:guarantee
;          (assert
;           (implies (proxy? d-proc)
;                    (made-data-proxy-flag (list-ref output 0))))))
;
;; Verify the implementation of the decision criteria.
;(define (verify-decision-criteria)
;  (verify (assert (implies (eq? output
;                                (list (plan-instantiate
;                                       fever-treatment-template cur-dt proc-status)))
;                           (<= 1 (count (lambda (d)
;                                          (and (avg-body-temp? d)
;                                               (> (abstraction-value d) 37)))
;                                        d-state))))))