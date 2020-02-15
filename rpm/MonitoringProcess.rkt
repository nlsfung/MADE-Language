#lang rosette/safe

(require "./MadeProcess.rkt")
(require "../rim/BasicDataTypes.rkt")
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

(provide gen:monitoring
         monitoring-process-output-specification
         monitoring-process-output-type
         monitoring-process-proxy-flag)
(provide (struct-out monitoring-process)
         (struct-out property-specification)
         (struct-out event-specification)
         (struct-out event-trigger))
(provide verify-monitoring-property
         verify-monitoring-event
         measurement-generator
         generate-measurement-list)

; This file contains the implementation of Monitoring processes.

; Monitoring process inherit from the generic MADE process, extending it with
; the output type of the process as well as a main body that varies depending
; on whether the output is for a property or an event.
(define-generics monitoring
  [monitoring-process-output-specification monitoring]
  [monitoring-process-output-type monitoring]
  [monitoring-process-proxy-flag monitoring])

(struct monitoring-process made-process ()
  #:transparent
  #:methods gen:monitoring []
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) monitoring-process)
   (define (valid? self)
     (and (valid-spec? self)
          (list? (made-process-data-state self))
          (andmap (lambda (d)
                    (and (made-data? d)
                         (super-valid? d)))
                  (made-process-data-state self))))]
  
  #:methods gen:made-proc
  [(define (proxy? self) (monitoring-process-proxy-flag self))

   (define (generate-data self in-data datetime)
     (gen-proc-generate-data
      (lambda (d-list dt)
        (execute-monitoring-body
         d-list
         dt
         (monitoring-process-output-specification self)
         (monitoring-process-output-type self)
         (monitoring-process-proxy-flag self)))
      self
      in-data
      datetime))

   (define/generic super-valid-spec? valid-spec?)
   (define (valid-spec? self)
     (and (super-valid-spec? (made-process null (made-process-control-state self)))
          (or (property-specification? (monitoring-process-output-specification self))
              (event-specification? (monitoring-process-output-specification self)))
          (valid? (monitoring-process-output-specification self))
          (procedure? (monitoring-process-output-type self))
          (boolean? (proxy? self))))])

; For observed properties, the output specification comprises a time window and
; a function that computes the appropriate property value given the input data
; set (sorted from least to most recent) and datetime.
(struct property-specification (time-window value-function)
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) property-specification)
   (define (valid? self)
     (and (duration? (property-specification-time-window self))
          (super-valid? (property-specification-time-window self))
          (procedure? (property-specification-value-function self))))])

; For observed events, the output specification comprise a pair of event
; triggers, each of which in turn comprises a window length for filtering
; out irrelevant data as well as a predicate on the filtered (and sorted) data
; and given datetime. Together, these triggers specify the conditions under
; which an event is onsidered to have started or ended respectively.
(struct event-specification (start-trigger end-trigger)
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) event-specification)
   (define (valid? self)
     (and (event-trigger? (event-specification-start-trigger self))
          (super-valid? (event-specification-start-trigger self))
          (event-trigger? (event-specification-end-trigger self))
          (super-valid? (event-specification-end-trigger self))))])

(struct event-trigger (time-window trigger-predicate)
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) event-trigger)
   (define (valid? self)
     (and (duration? (event-trigger-time-window self))
          (super-valid? (event-trigger-time-window self))
          (procedure? (event-trigger-trigger-predicate self))))])

; Helper function for defining the main behaviour of monitoring processes.
(define (execute-monitoring-body d-list dt o-spec o-type proxy-flag)
  (cond [(property-specification? o-spec)
         (execute-property-body d-list
                                dt
                                (property-specification-time-window o-spec)
                                (property-specification-value-function o-spec)
                                o-type
                                proxy-flag)]
        [(event-specification? o-spec)
         (execute-event-body d-list
                             dt
                             (event-specification-start-trigger o-spec)
                             (event-specification-end-trigger o-spec)
                             o-type
                             proxy-flag)]))
 
; Helper function for specifying the behaviour of monitoring processes that
; output observed properties.
(define (execute-property-body d-list dt t-window p-func o-type proxy-flag)
  ; To generate an observed property, the following steps are followed:
  ; 1) Filter out irrelevant data, and sort the remaining data.
  ; 2) Execute the property function.
  ; 3) Instantiate the property.
  (let* ([filtered-data (sort (filter-expired-data d-list (dt- dt t-window) dt)
                              dt<? #:key measurement-valid-datetime)]
         [property-value (p-func filtered-data)])
    (if (void? property-value)
        null
        (list (o-type proxy-flag dt property-value)))))

; Helper function for specifying the behaviour of monitoring processes that
; output observed events.
(define (execute-event-body d-list dt start-trigger end-trigger o-type proxy-flag)
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
                                    d-list)
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
        (list (o-type proxy-flag
                      (datetime-range (measurement-valid-datetime opposing-event) dt)
                      end-event?))
        null)))
    
; Helper function for filtering out data that lies outside a given range.
(define (filter-expired-data d-state dt-start dt-end)
  (filter (lambda (d)
            (and (measurement? d)
                 (dt-between? (measurement-valid-datetime d)
                              dt-start dt-end)))
          d-state))

; verify-monitoring-property helps verify a Monitoring process for observed properties.
; It accepts as input:
; 1) The struct-constructor for the monitoring process.
; 2) A list of measurement generators.
; 3) The execution datetime (which can be symbolic).
; The verifier outputs (unsat) if the input Monitoring process cannot generate
; a valid output observed property from the specified input measurements. Otherwise,
; it returns a satisfiable model.
(define (verify-monitoring-property proc-constructor measurement-gen-list dt)
  (let* ([c-state (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t)]
         [proc (proc-constructor null c-state)]
         [t-window (property-specification-time-window
                    (monitoring-process-output-specification proc))]
         [p-func (property-specification-value-function
                  (monitoring-process-output-specification proc))]
         [o-type (monitoring-process-output-type proc)]
         [proxy-flag (monitoring-process-proxy-flag proc)]
         
         [d-list (foldl (lambda (generator result)
                          (append result
                                  (generate-measurement-list
                                   (measurement-generator-getter generator)
                                   (measurement-generator-start-datetime generator)
                                   (measurement-generator-end-datetime generator)
                                   (measurement-generator-frequency generator))))
                        null
                        measurement-gen-list)]

         [filtered-data (sort (filter-expired-data d-list (dt- dt t-window) dt)
                              dt<? #:key measurement-valid-datetime)]
         [property-value (p-func filtered-data)]
         [output (o-type proxy-flag dt property-value)]
         [sol (solve (assert (and (not (void? property-value))
                                  (valid? output))))])
    (if (eq? sol (unsat))
        (displayln (unsat))
        (begin
          (displayln "Input data:")
          (displayln (evaluate d-list sol))
          (displayln "Current date-time:")
          (displayln (evaluate dt sol))
          (displayln "Output data:")
          (displayln (evaluate output sol))
          (displayln "")))
    (clear-asserts!)))

; verify-monitoring-event helps verify a Monitoring process for observed events.
; It accepts as input:
; 1) The struct-constructor for the monitoring process.
; 2) A list of measurement generators.
; 3) The execution datetime (which can be symbolic).
; The verifier outputs a model (if any) for each of the following conditions:
; 1) The input measurements satisfy the start trigger.
; 2) The input measurements satisfy the end trigger.
; 3) The input measurements satisfy both the start and end triggers.
(define (verify-monitoring-event proc-constructor measurement-gen-list dt)
  (define (display-solution d-list dt sol)
    (displayln "Input data:")
    (displayln (evaluate d-list sol))
    (displayln "Current date-time:")
    (displayln (evaluate dt sol)))
  
  (let* ([c-state (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t)]
         [proc (proc-constructor null c-state)]
         [start-trigger (event-specification-start-trigger
                         (monitoring-process-output-specification proc))]
         [end-trigger (event-specification-end-trigger
                       (monitoring-process-output-specification proc))]

         [d-list (foldl (lambda (generator result)
                          (append result
                                  (generate-measurement-list
                                   (measurement-generator-getter generator)
                                   (measurement-generator-start-datetime generator)
                                   (measurement-generator-end-datetime generator)
                                   (measurement-generator-frequency generator))))
                        null
                        measurement-gen-list)]

         [start-win (event-trigger-time-window start-trigger)]
         [start-pred (event-trigger-trigger-predicate start-trigger)]
         [start-event? (start-pred (filter-expired-data d-list (dt- dt start-win) dt))]

         [end-win (event-trigger-time-window end-trigger)]
         [end-pred (event-trigger-trigger-predicate end-trigger)]
         [end-event? (end-pred (filter-expired-data d-list (dt- dt end-win) dt))]

         [start-sol (solve (assert start-event?))]
         [end-sol (solve (assert end-event?))]
         [both-sol (solve (assert (and start-event? end-event?)))])
    
    (displayln "Model for start trigger:")
    (if (eq? start-sol (unsat))
        (displayln (unsat))
        (display-solution d-list dt start-sol))
    (displayln "")
    
    (displayln "Model for end trigger:")
    (if (eq? end-sol (unsat))
        (displayln (unsat))
        (display-solution d-list dt end-sol))
    (displayln "")
    
    (displayln "Model for both start and end trigger:")
    (if (eq? both-sol (unsat))
        (displayln (unsat))
        (display-solution d-list dt both-sol))
    (displayln "")))

; Measurement generator contains the specification for generating a list of
; symbolic measurements (for verification purposes). It comprises:
; 1) A measurement getter.
; 2) A starting date-time for the corresponding measurements.
; 3) An ending date-time for the measurements.
; 4) A frequency which can either be:
;    a) A duration indicating how often the measurements should be repeated.
;    b) A positive integer indicating the total number of measurements between
;       the given start date-time and end-datetime.
(struct measurement-generator
  (getter start-datetime end-datetime frequency)
  #:transparent)

; generate-measurement-list generates a list of measurements 
(define (generate-measurement-list getter start-datetime end-datetime frequency)
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
    (assert (eq? (length d-list)
                 (remove-duplicates (map (lambda (d) (measurement-valid-datetime d))
                                         d-list))))
    d-list))
