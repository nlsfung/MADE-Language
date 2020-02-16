#lang rosette/safe

(require (only-in rosette string? symbol?))
(require "./MadeProcess.rkt")
(require "./AnalysisProcess.rkt")
(require "../rim/BasicDataTypes.rkt")
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

(provide gen:decision
         decision-process-plan-template
         decision-process-decision-criteria
         decision-process-proxy-flag)
(provide (struct-out decision-process)
         gen:plan-temp plan-instantiate
         gen:inst-template instantiate
         gen:rel-sched sched-instantiate
         (struct-out relative-schedule)
         (struct-out plan-template)
         (struct-out control-template)
         (struct-out homogeneous-action-template)
         (struct-out culminating-action-template)
         filter-abstractions)
(provide verify-decision
         (struct-out abstraction-generator)
         generate-abstraction-list
         execute-decision-body)

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
  (let* ([filtered-data (filter-abstractions d-list dt)]
         [selected-crit (findf (lambda (c) (c filtered-data)) d-crit)])
    (if selected-crit
        (list (plan-instantiate p-temp dt proxy-flag))
        null)))
         
; Helper function for filtering out abstractions that are:
; 1) Not valid at the input datetime.
; 2) Overriden by another abstraction.
(define (filter-abstractions d-list dt)
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
    
    (filter-map (lambda (t) (findf (lambda (d) (eq? (get-type d) t)) sorted-data))
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
     (and (symbol? (control-template-target-process self))
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
     (and (symbol? (homogeneous-action-template-action-type self))
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
     (and (symbol? (culminating-action-template-action-type self))
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
              (boolean? (relative-schedule-interval self)))))]
  
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

; verify-decision helps verify a Decision process. 
; It accepts as input:
; 1) The struct-constructor for the decision process.
; 2) A list of abstraction generators.
; 3) The execution datetime (which can be symbolic).
; The verifier outputs a model (if any) for each of the following conditions:
; 1) The input observations satisfy one decision criterion.
;    (A seperate model is produced for each criterion).
(define (verify-decision proc-constructor abs-gen-list dt)
  (define (display-solution d-list dt sol d-crit output)
    (displayln (format "Model for decision criterion: ~a" d-crit))
    (if (eq? sol (unsat))
        (displayln (unsat))
        (begin
          (displayln "Input data:")
          (displayln (evaluate d-list sol))
          (displayln "Current date-time:")
          (displayln (evaluate dt sol))
          (displayln "Output data:")
          (displayln (evaluate output sol))))
    (displayln ""))
  
  (let* ([d-list (foldl (lambda (generator result)
                                  (append result
                                          (generate-abstraction-list
                                           (abstraction-generator-getter generator)
                                           (abstraction-generator-start-datetime generator)
                                           (abstraction-generator-end-datetime generator)
                                           (abstraction-generator-frequency generator))))
                                null
                                abs-gen-list)]

         [c-state (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t)]
         [proc (proc-constructor null c-state)]
         [p-temp (decision-process-plan-template proc)]
         [decision-criteria (decision-process-decision-criteria proc)]
         [proxy-flag (decision-process-proxy-flag proc)]

         [output-num (map (lambda (d-crit)
                            (- (length decision-criteria) (length (member d-crit decision-criteria))))
                          decision-criteria)]
         [output-list (map (lambda (n)
                             (let* ([d-crit (list-ref decision-criteria n)])
                               (execute-decision-body d-list dt p-temp (list d-crit) proxy-flag)))
                           output-num)])
    (for-each
     (lambda (n)
       (let* ([output (list-ref output-list n)]
              [sol (solve (assert (and (not (null? output))
                                       (valid? (list-ref output 0)))))])
         (display-solution d-list dt sol n output)))
     output-num)
    (clear-asserts!)))

; Abstraction generator contains the specification for generating a list of
; symbolic abstractions (for verification purposes). It comprises:
; 1) An abstraction getter.
; 2) A starting date-time for the corresponding abstraction.
; 3) An ending date-time for the abstraction.
; 4) A frequency which can either be:
;    a) A duration indicating how often the observations should be repeated.
;    b) A positive integer indicating the number of observations to generate.
;       In this case, the start date-time indicates the earliest date-time for
;       the measurement and the end date-time latest.
(struct abstraction-generator
  (getter start-datetime end-datetime frequency)
  #:transparent)

; generate-abstraction-list generates a list of abstractions.
(define (generate-abstraction-list getter start-datetime end-datetime frequency)
  (define (generate-count total)
    (if (or (<= total 0) (dt>? start-datetime end-datetime))
        null
        (let ([data (getter start-datetime end-datetime)])
          (assert (valid? data))
          (append (list data)
                  (generate-count (- total 1))))))
  
  (define (generate-interval cur-dt)  
    (if (dt>? cur-dt end-datetime)
        null
        (let ([data (getter cur-dt cur-dt)]
              [next-dt (dt+ cur-dt frequency)])
          (assert (valid? data))
          (append (list data)
                  (generate-interval next-dt)))))
  
  (let ([d-list (cond [(integer? frequency) (generate-count frequency)]
                      [(duration? frequency) (generate-interval start-datetime)])])
    
    (assert (and (eq? (length d-list)
                      (length (remove-duplicates
                               (map (lambda (d)
                                      (datetime-range-start
                                       (abstraction-valid-datetime-range d)))
                                    d-list))))
                 (eq? (length d-list)
                      (length (remove-duplicates
                               (map (lambda (d)
                                      (datetime-range-end
                                       (abstraction-valid-datetime-range d)))
                                    d-list))))))
    d-list))
