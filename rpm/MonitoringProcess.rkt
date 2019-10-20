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
         
         [start-event? (start-pred (filter-expired-data sorted-data (dt- dt start-win) dt))]
         [end-event? (end-pred (filter-expired-data sorted-data (dt- dt end-win) dt))]
         [opposing-event (cond [start-event?
                                (findf (lambda (d)
                                         (let* ([ref-dt (measurement-valid-datetime d)])
                                           (end-pred (filter-expired-data sorted-data
                                                                          (dt- ref-dt end-win)
                                                                          ref-dt))))
                                       (reverse sorted-data))]
                               [end-event?
                                (findf (lambda (d)
                                         (let* ([ref-dt (measurement-valid-datetime d)])
                                           (start-pred (filter-expired-data sorted-data
                                                                            (dt- ref-dt start-win)
                                                                            ref-dt))))
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