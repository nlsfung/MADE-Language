#lang rosette/safe

(require "./MadeProcess.rkt")
(require "../rim/BasicDataTypes.rkt")
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

(provide (struct-out analysis-process))

; This file contains the implementation of Analysis processes.

; Analysis process inherit from the generic MADE process, extending it with
; a time window, the output type identifier and a list of abstraction functions.
(struct analysis-process made-process (time-window output-type abstraction-functions)
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) analysis-process)
   (define (valid? self)
     (and (valid-spec? self)
          (list? (made-process-data-state self))
          (andmap (lambda (d)
                    (and (made-data? d)
                         (super-valid? d)))
                  (made-process-data-state self))))]
  
  #:methods gen:made-proc
  [(define/generic super-valid-spec? valid-spec?)
   (define (execute self in-data datetime)
     (gen-proc-execute self in-data datetime))

   (define (update-data-state self in-data)
     (gen-proc-update-data-state self in-data))

   (define (generate-data self datetime)
     (gen-proc-generate-data
      (lambda (d-state dt)
        (execute-analysis-body d-state
                               dt
                               (analysis-process-time-window self)
                               (analysis-process-output-type self)
                               (analysis-process-abstraction-functions self)
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
           [t-window (analysis-process-time-window self)]
           [out-type (analysis-process-output-type self)]
           [ab-funcs (analysis-process-abstraction-functions self)])
       
       (analysis-process id d-state c-state p-flag t-window out-type ab-funcs)))

   (define (valid-spec? self)
     (and (super-valid-spec? (made-process (made-process-id self)
                                           null
                                           (made-process-control-state self)
                                           (made-process-proxy-flag self)))
          (duration? (analysis-process-time-window self))
          (valid? (analysis-process-time-window self))
          (procedure? (analysis-process-output-type self))
          (list? (analysis-process-abstraction-functions self))
          (andmap (lambda (f) (procedure? f)) (analysis-process-abstraction-functions self))))])

; Helper function for defining the main behaviour of analysis processes.
(define (execute-analysis-body d-state dt t-window out-type ab-funcs proxy-flag)
  ; To generate output data, an Analysis process follows the following steps:
  ; 1) Filter out any data that falls outside the time window.
  ; 2) Feed the filtered data into the input list of abstraction functions, one
  ;    of which may produce a non-void output value.
  ; 3) If an output value is produced, determine its valid datetime range.
  ; 4) Generate the output abstraction, if any.
  (let* ([filtered-data (sort (filter-expired-data d-state (dt- dt t-window) dt)
                              observed-after?)]
         [output-func (findf (lambda (f) (not (void? (f filtered-data)))) ab-funcs)]
         [output-value (if output-func (output-func filtered-data) (void))]

         [latest-significant-data
          (if (void? output-value)
              (void)
              (foldl (lambda (d result)
                       (if (eq? output-value
                                (output-func (member d (reverse filtered-data))))
                           (if result result d)
                           #f))
                     #f
                     filtered-data))]

         [latest-start-time
          (if (void? output-value)
              (void)
              (cond [(observed-property? latest-significant-data)
                     (observed-property-valid-datetime
                      latest-significant-data)]
                    [(observed-event? latest-significant-data)
                     (datetime-range-end
                      (observed-event-valid-datetime-range
                       latest-significant-data))]))])

    (if (void? output-value)
        (void)
        (out-type proxy-flag
                  (datetime-range dt (dt+ latest-start-time t-window))
                  output-value))))

; Helper function to determine the temporal order of two observations.
(define (observed-after? ob1 ob2)
  (dt>? (cond [(observed-property? ob1)
               (observed-property-valid-datetime ob1)]
              [(observed-event? ob1)
               (datetime-range-end
                (observed-event-valid-datetime-range ob1))])
        (cond [(observed-property? ob2)
               (observed-property-valid-datetime ob2)]
              [(observed-event? ob2)
               (datetime-range-end
                (observed-event-valid-datetime-range ob2))])))

; Helper function for filtering out data that lies outside a given range.
(define (filter-expired-data d-state dt-start dt-end)
  (filter (lambda (d)
            (or (and (observed-property? d)
                     (dt-between? (observed-property-valid-datetime d)
                                  dt-start dt-end))
                (and (observed-event? d)
                     (dt-range-overlap?
                      (observed-event-valid-datetime-range d)
                      (datetime-range dt-start dt-end)))))
          d-state))

; Helper function to determine if a datetime range intersects with another.
(define (dt-range-overlap? range-1 range-2)
  (and (or (dt<? (datetime-range-start range-1)
                 (datetime-range-end range-2))
           (dt=? (datetime-range-start range-1)
                 (datetime-range-end range-2)))
       (or (dt>? (datetime-range-end range-1)
                 (datetime-range-start range-2))
           (dt=? (datetime-range-end range-1)
                 (datetime-range-start range-2)))))

;; Symbolic constants for verifying generate data.
;(define (gen-dt-part) (define-symbolic* dt-part integer?) dt-part)
;(define (gen-datetime)
;  (let ([hour (gen-dt-part)])
;    (assert (and (>= hour 0) (< hour 24)))
;    (datetime 7 9 12 hour 0 0)))
;
;(struct room-temperature observed-property () #:transparent)
;(define (gen-temp-proxy) (define-symbolic* proxy boolean?) proxy)
;(define (gen-temp-value) (define-symbolic* temp integer?) temp)
;(define (gen-temp)
;  (room-temperature (gen-temp-proxy) (gen-datetime) (gen-temp-value)))
;
;(struct room-temperature-grade abstraction () #:transparent)
;(define (grade-temp-low d-state)
;  (if (>= (length (filter (lambda (d)
;                            (and (room-temperature? d)
;                                 (< (observed-property-value d) 5)))
;                         d-state)) 3)
;      'low
;      (void)))
;(define (grade-temp-high d-state)
;  (if (>= (length (filter (lambda (d)
;                            (and (room-temperature? d)
;                                 (> (observed-property-value d) 10)))
;                         d-state)) 4)
;      'high
;      (void)))
;
;(define d-state (list (gen-temp) (gen-temp) (gen-temp) (gen-temp) (gen-temp)))
;(define sched-dt (gen-datetime))
;(define cur-dt (gen-datetime))
;(define-symbolic proc-status boolean?)
;(define-symbolic win-length integer?)
;(assert (eq? (length d-state)
;             (length
;              (remove-duplicates
;               (map (lambda (d) (observed-property-valid-datetime d)) d-state)))))
;(assert (eq? (length d-state)
;             (length
;              (remove-duplicates
;               (map (lambda (d) (observed-property-value d)) d-state)))))
;(assert (eq? (length d-state)
;             (length (filter
;                      (lambda (d) (<= (datetime-hour
;                                       (observed-property-valid-datetime d))
;                                      (datetime-hour cur-dt)))
;                      d-state))))
;(assert (> win-length 0))
;
;(define c-state (control-state (schedule (list sched-dt) #f) proc-status))
;(define t-window (duration 0 win-length 0 0))
;(define out-type room-temperature-grade)
;(define ab-funcs (list grade-temp-low grade-temp-high))
;(define room-temp-proc
;  (analysis-process 'id d-state c-state (gen-temp-proxy)
;                    t-window room-temperature-grade ab-funcs))
;
;(define output (generate-data room-temp-proc cur-dt))
;
;; Verify the implementation of the control state.
;(define (verify-is-executed)
;  (verify (assert (implies (or (not (eq? sched-dt cur-dt))
;                               (not proc-status))
;                           (void? output)))))
;
;; Verify the implementation of the time window.
;(define (verify-time-window)
;  (verify (assert (implies (< win-length 2) (void? output)))))
;
;; Verify the implementation of the abstraction functions.
;(define (verify-ab-funcs)
;  (verify
;   (assert
;    (implies (eq? (length d-state)
;                  (length (filter
;                           (lambda (d)
;                             (and (< (observed-property-value d) 14)
;                                  (> (observed-property-value d) 2)))
;                           d-state)))
;             (void? output)))))
;
;; Verify the implementation of the proxy flags.
;(define (verify-data-proxy)
;  (verify
;   (assert
;    (implies (<= 3 (length (filter (lambda (d) (made-data-proxy-flag d)) d-state)))
;             (void? output)))))
;
;(define (verify-proc-proxy)
;  (verify #:assume
;          (assert (room-temperature-grade? output))
;          #:guarantee
;          (assert
;           (implies (made-process-proxy-flag room-temp-proc)
;                    (made-data-proxy-flag output)))))
;
;; Verify the implementation of determining abstraction validity range.
;(define new-dt (datetime 7 9 12 (gen-dt-part) 0 0))
;(assert (normalized? new-dt))
;(define new-output (execute-analysis-body d-state new-dt t-window out-type ab-funcs #f))
;(define (verify-valid-range)
;  (verify (assert
;           (implies (and (room-temperature-grade? output)
;                         (dt>? new-dt (datetime-range-start
;                                       (abstraction-valid-datetime-range output)))
;                         (dt<? new-dt (datetime-range-end
;                                       (abstraction-valid-datetime-range output))))
;                    (and (room-temperature-grade? new-output)
;                         (eq? (abstraction-value new-output)
;                              (abstraction-value output)))))))
