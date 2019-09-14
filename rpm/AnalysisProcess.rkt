#lang rosette/safe

(require "./MadeProcess.rkt")
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

; This file contains the implementation of Analysis processes.

; Analysis process inherit from the generic MADE process, extending it with
; a time window, the output type identifier and a main function body.
(struct analysis-process made-process (time-window output-type main-body)
  #:transparent
  #:methods gen:made-proc
  [(define (execute self in-data datetime)
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
                               (analysis-process-main-body self)))
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
           [body (analysis-process-main-body self)])
       
       (analysis-process id d-state c-state p-flag t-window out-type body)))])

; Helper function for defining the main behaviour of analysis processes.
(define (execute-analysis-body d-state dt t-window out-type body)
  ; To generate output data, an Analysis process follows the following steps:
  ; 1) Filter out any data that falls outside the time window.
  ; 2) Feed the filtered data into the main body, which comprises a list of
  ;    functions, one of which may produce a non-void output value.
  ; 3) If an output value is produced, determine its valid datetime range.
  ; 4) Generate the output abstraction, if any.
  (let* ([filtered-data (sort (filter-expired-data d-state (dt- dt t-window) dt)
                              observed-after?)]
         [output-body (findf (lambda (f) (not (void? (f filtered-data)))) body)]
         [output-value (if output-body (output-body filtered-data) (void))]

         [latest-significant-data
          (if (void? output-value)
              (void)
              (first (memf (lambda (d)
                             (eq? output-value
                                  (output-body
                                   (member d (reverse filtered-data)))))
                           filtered-data)))]

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
        (out-type (datetime-range dt (dt+ latest-start-time t-window))
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

; Helper function to determine if a datetime is in between a given range.
(define (dt-between? dt dt-low dt-high)
  (and (or (dt>? dt dt-low) (dt=? dt dt-low))
       (or (dt<? dt dt-high) (dt=? dt dt-high))))

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

;; Symbolic constants for verifying update data state and update control state.
;(define-symbolic p-id integer?)
;(define-symbolic p-data integer?)
;(define-symbolic p-sched p-stat integer?)
;(define a-proc
;  (analysis-process p-id (list p-data) (control-state p-sched p-stat) 1 2 3 4))
;
;(define-symbolic in1 in2 integer?)
;(define in-data (list in1 in2))
;
;(define new-data-proc (update-data-state a-proc in-data))
;
;(define-symbolic t-id integer?)
;(define-symbolic v-dt-y v-dt-mth v-dt-d v-dt-h v-dt-min v-dt-s integer?)
;(define-symbolic c-inst-sched c-inst-stat integer?)
;(define v-dt (datetime v-dt-y v-dt-mth v-dt-d v-dt-h v-dt-min v-dt-s))
;(define c-inst (control-instruction t-id v-dt c-inst-sched c-inst-stat))
;
;(define-symbolic dt-y dt-mth dt-d dt-h dt-min dt-s integer?)
;(define cur-dt (datetime dt-y dt-mth dt-d dt-h dt-min dt-s))
;
;(define new-ctrl-proc (update-control-state a-proc (list c-inst) cur-dt))
;
;; Verify implementation of update data state.
;(define (verify-update-data-state)
;  (verify (assert (implies (< (length (made-process-data-state
;                                       new-data-proc)) 3)
;                           (or (= p-data in1)
;                               (= p-data in2)
;                               (= in1 in2))))))
;
;; Verify implementation of update control state.
;(define (verify-update-control-state)
;  (verify #:assume (assert (and (normalized? v-dt)
;                                (normalized? cur-dt)))
;          #:guarantee (assert (implies (eq? a-proc new-ctrl-proc)
;                                       (or (not (dt=? v-dt cur-dt))
;                                           (not (= p-id t-id))
;                                           (and (= p-sched c-inst-sched)
;                                                (= p-stat c-inst-stat)))))))

;; Symbolic constants for verifying generate data.
;(define (gen-dt-part) (define-symbolic* dt-part integer?) dt-part)
;(define (gen-datetime)
;  (let ([dt (datetime 7 9 12 (gen-dt-part) 0 0)])
;    (assert (normalized? dt))
;    dt))
;
;(struct room-temperature observed-property () #:transparent)
;(define (gen-temp-value) (define-symbolic* temp integer?) temp)
;(define (gen-temp)
;  (room-temperature (gen-datetime) (gen-temp-value)))
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
;
;(define c-state (control-state (schedule (list sched-dt) #f) proc-status))
;(define t-window (duration 0 win-length 0 0))
;(define out-type room-temperature-grade)
;(define body (list grade-temp-low grade-temp-high))
;(define room-temp-proc
;  (analysis-process 'id d-state c-state #f t-window room-temperature-grade body))
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
;; Verify the implementation of the main process body.
;(define (verify-proc-body)
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
;; Verify the implementation of determining abstraction validity range.
;(define (verify-valid-range)
;  (verify #:assume
;          (assert (room-temperature-grade? output))
;          #:guarantee
;          (assert (dt<? (datetime-range-end
;                         (abstraction-valid-datetime-range output))
;                        (dt+ cur-dt t-window)))))