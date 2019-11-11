#lang rosette/safe

(require "./MadeProcess.rkt")
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

; This file contains the implementation of Monitoring processes.

; Monitoring process inherit from the generic MADE process, extending it with
; the output type of the process as well as a main body that varies depending
; on whether the output is for a property or an event.
(struct monitoring-process made-process (output-specification output-type)
  #:transparent
  #:methods gen:made-proc
  [(define (execute self in-data datetime)
     (gen-proc-execute self in-data datetime))

   (define (update-data-state self in-data)
     (gen-proc-update-data-state self in-data))

   (define (generate-data self datetime)
     (gen-proc-generate-data
      (lambda (d-state dt)
        (execute-monitoring-body
         d-state
         dt
         (monitoring-process-output-specification self)
         (monitoring-process-output-type self)
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
           [o-spec (monitoring-process-output-specification self)]
           [o-type (monitoring-process-output-type self)])
       
       (monitoring-process id d-state c-state p-flag o-spec o-type)))])

; For observed properties, the output specification comprises a time window and
; a function that computes the appropriate property value given the input data
; set (sorted from least to most recent) and datetime.
(struct property-specification (time-window value-function) #:transparent)

; For observed events, the output specification comprise a pair of event
; triggers, each of which in turn comprises a window length for filtering
; out irrelevant data as well as a predicate on the filtered (and sorted) data
; and given datetime. Together, these triggers specify the conditions under
; which an event is onsidered to have started or ended respectively.
(struct event-specification (start-trigger end-trigger) #:transparent)
(struct event-trigger (time-window trigger-predicate) #:transparent)

; Helper function for defining the main behaviour of monitoring processes.
(define (execute-monitoring-body d-state dt o-spec o-type proxy-flag)
  (cond [(property-specification? o-spec)
         (execute-property-body d-state
                                dt
                                (property-specification-time-window o-spec)
                                (property-specification-value-function o-spec)
                                o-type
                                proxy-flag)]
        [(event-specification? o-spec)
         (execute-event-body d-state
                             dt
                             (event-specification-start-trigger o-spec)
                             (event-specification-end-trigger o-spec)
                             o-type
                             proxy-flag)]))
 
; Helper function for specifying the behaviour of monitoring processes that
; output observed properties.
(define (execute-property-body d-state dt t-window p-func o-type proxy-flag)
  ; To generate an observed property, the following steps are followed:
  ; 1) Filter out irrelevant data, and sort the remaining data.
  ; 2) Execute the property function.
  ; 3) Instantiate the property.
  (let* ([filtered-data (sort (filter-expired-data d-state (dt- dt t-window) dt)
                              dt<? #:key measurement-valid-datetime)]
         [property-value (p-func filtered-data dt)])
    (if (void? property-value)
        (void)
        (o-type proxy-flag
                dt
                property-value))))

; Helper function for specifying the behaviour of monitoring processes that
; output observed events.
(define (execute-event-body d-state dt start-trigger end-trigger o-type proxy-flag)
  ; To generate an observed event, the following steps are followed:
  ; 1) Sort the input data from the least to most recent.
  ; 2) Check if the start (or end) trigger is activated.
  ; 3) If activated, find the most recent activation datetime of the opposing trigger.
  ; 4) Instantiate the event (if any) with the appropriate boolean value.
  (let* ([start-win (event-trigger-time-window start-trigger)]
         [start-pred (event-trigger-trigger-predicate start-trigger)]
         [end-win (event-trigger-time-window end-trigger)]
         [end-pred (event-trigger-trigger-predicate end-trigger)]
         
         [sorted-data (sort (filter (lambda (d)
                                      (and (measurement? d)
                                           (not (dt>? (measurement-valid-datetime d) dt))))
                                    d-state)
                            dt<? #:key measurement-valid-datetime)]
         
         [start-event? (start-pred (filter-expired-data sorted-data (dt- dt start-win) dt) dt)]
         [end-event? (end-pred (filter-expired-data sorted-data (dt- dt end-win) dt) dt)]
         [opposing-event (cond [start-event?
                                (findf (lambda (d)
                                         (let* ([ref-dt (measurement-valid-datetime d)])
                                           (end-pred (filter-expired-data sorted-data
                                                                          (dt- ref-dt end-win)
                                                                          ref-dt) dt)))
                                       (reverse sorted-data))]
                               [end-event?
                                (findf (lambda (d)
                                         (let* ([ref-dt (measurement-valid-datetime d)])
                                           (start-pred (filter-expired-data sorted-data
                                                                            (dt- ref-dt start-win)
                                                                            ref-dt) dt)))
                                       (reverse sorted-data))]
                               [else #f])])
    (if opposing-event
        (o-type proxy-flag
                (datetime-range (measurement-valid-datetime opposing-event) dt)
                end-event?)
        (void))))
    
; Helper function for filtering out data that lies outside a given range.
(define (filter-expired-data d-state dt-start dt-end)
  (filter (lambda (d)
            (and (measurement? d)
                 (dt-between? (measurement-valid-datetime d)
                              dt-start dt-end)))
          d-state))

;; Symbolic constants for verifying generate data.
;; Executed with datetime-unwind and schedule-unwind set to 0.
;(define (gen-dt-part) (define-symbolic* dt-part integer?) dt-part)
;(define (gen-datetime)
;  (let ([hour (gen-dt-part)])
;    (assert (and (>= hour 0) (< hour 24)))
;    (datetime 7 10 20 hour 0 0)))
;(define (gen-window)
;  (let ([hour (gen-dt-part)])
;    (assert (and (>= hour 0) (< hour 24)))
;    (duration 0 hour 0 0)))
;
;(define (gen-proxy) (define-symbolic* proxy boolean?) proxy)
;(define (gen-measure-value)
;  (define-symbolic* m integer?)
;  (assert (> m 0))
;  m)
;
;(struct body-acceleration measurement () #:transparent)
;(define (gen-body-acc)
;  (body-acceleration (gen-proxy) (gen-datetime) (gen-measure-value)))
;
;(struct activity-level observed-property () #:transparent)
;(struct exercise-event observed-event () #:transparent)
;
;(define (sum d-list dt)
;  (foldl (lambda (d result) (+ (measurement-value d) result))
;            0
;            (filter (lambda (d) (body-acceleration? d)) d-list)))
;
;(define activity-spec (property-specification (gen-window) sum))
;(define exercise-spec
;  (event-specification
;   (event-trigger (gen-window)
;                  (lambda (d-list dt) (> (sum d-list dt) 25)))
;   (event-trigger (gen-window)
;                  (lambda (d-list dt) (< (sum d-list dt) 5)))))
;
;(define d-state
;  (list (gen-body-acc) (gen-body-acc) (gen-body-acc) (gen-body-acc)))
;  
;(define sched-dt (gen-datetime))
;(define cur-dt (gen-datetime))
;(define-symbolic proc-status boolean?)
;(define c-state (control-state (schedule (list sched-dt) #f) proc-status))
;
;(define m-proc-1
; (monitoring-process 'id d-state c-state (gen-proxy) activity-spec activity-level))
;(define m-proc-2
;  (monitoring-process 'id d-state c-state (gen-proxy) exercise-spec exercise-event)) 
;
;(define output-1 (generate-data m-proc-1 cur-dt))
;(define output-2 (generate-data m-proc-2 cur-dt))
;
;; Verify implementation of execute property body.
;(define (verify-property-body)
;  (verify (assert
;           (implies (is-proc-executed? c-state cur-dt)
;                    (and (activity-level? output-1)
;                         (<= (observed-property-value output-1)
;                             (sum d-state cur-dt)))))))
;
;; Verify implementation of execute event body.
;(define over-acc-meas (filter (lambda (d) (and (> (measurement-value d) 25))) d-state))
;(define start-win (event-trigger-time-window (event-specification-start-trigger exercise-spec)))
;(define end-win (event-trigger-time-window (event-specification-end-trigger exercise-spec)))
;(define (verify-event-body)
;  (verify (assert
;           (implies (and (dur=? start-win (duration 0 0 0 0))
;                         (dur=? end-win (duration 0 0 0 0))
;                         (= 4 (length (remove-duplicates
;                                       (map (lambda (d) (measurement-valid-datetime d))
;                                            d-state)))))
;                    (and (implies (and (exercise-event? output-2)
;                                       (not (observed-event-value output-2)))
;                                  (findf (lambda (d) (dt=? (measurement-valid-datetime d)
;                                                           cur-dt))
;                                         over-acc-meas))
;                         (implies (and (exercise-event? output-2)
;                                       (observed-event-value output-2))
;                                  (findf (lambda (d) (dt<? (measurement-valid-datetime d)
;                                                           cur-dt))
;                                         over-acc-meas)))))))
;
;; Verify the implementation of the proxy flags.
;(define (verify-data-proxy)
;  (verify
;   (assert
;    (implies (and (is-proc-executed? c-state cur-dt)
;                  (= 4 (length (filter (lambda (d) (made-data-proxy-flag d)) d-state))))
;             (and (activity-level? output-1)
;                  (= 0 (observed-property-value output-1))
;                  (void? output-2))))))
;
;(define (verify-proc-proxy-1)
;  (verify #:assume
;          (assert (not (void? output-1)))
;          #:guarantee
;          (assert
;           (implies (made-process-proxy-flag m-proc-1)
;                    (made-data-proxy-flag output-1)))))
;
;(define (verify-proc-proxy-2)
;  (verify #:assume
;          (assert (not (void? output-2)))
;          #:guarantee
;          (assert
;           (implies (made-process-proxy-flag m-proc-2)
;                    (made-data-proxy-flag output-2)))))