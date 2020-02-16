#lang rosette/safe

(require "../rpm/MonitoringProcess.rkt"
         "../rpm/MadeProcess.rkt"
         "../rim/BasicDataTypes.rkt"
         "../rim/TemporalDataTypes.rkt"
         "../rim/MadeDataStructures.rkt")

; Symbolic constants for verifying generate data.
; Executed with datetime-unwind and schedule-unwind set to 0.
(define (gen-dt-part) (define-symbolic* dt-part integer?) dt-part)
(define (gen-datetime)
  (let ([hour (gen-dt-part)])
    (assert (and (>= hour 0) (< hour 24)))
    (datetime 7 10 20 hour 0 0)))
(define (gen-window)
  (let ([hour (gen-dt-part)])
    (assert (and (>= hour 0) (< hour 24)))
    (duration 0 hour 0 0)))

(define (gen-proxy) (define-symbolic* proxy boolean?) proxy)
(define (gen-measure-value)
  (define-symbolic* m integer?)
  (assert (> m 0))
  m)

(struct body-acceleration measurement () #:transparent)
(define (gen-body-acc)
  (body-acceleration (gen-proxy) (gen-datetime) (gen-measure-value)))

(struct activity-level observed-property () #:transparent)
(struct exercise-event observed-event () #:transparent)

(define (sum d-list)
  (foldl (lambda (d result) (+ (measurement-value d) result))
            0
            (filter (lambda (d) (body-acceleration? d)) d-list)))

(define property-window (gen-window))
(define activity-spec (property-specification property-window sum))

(define event-start-win (gen-window))
(define event-end-win (gen-window))
(define event-start-pred (lambda (d-list) (> (sum d-list) 25)))
(define event-end-pred (lambda (d-list) (< (sum d-list) 5)))
(define exercise-spec
  (event-specification
   (event-trigger event-start-win event-start-pred)
   (event-trigger event-end-win event-end-pred)))

(define d-state
  (list (gen-body-acc) (gen-body-acc) (gen-body-acc) (gen-body-acc)))
  
(define sched-dt (gen-datetime))
(define cur-dt (gen-datetime))
(define-symbolic proc-status boolean?)
(define c-state (control-state (schedule (list sched-dt) #f) #t))

(define sample-process-1-proxy (gen-proxy))
(struct sample-process-1 monitoring-process ()
  #:transparent
  #:methods gen:monitoring
  [(define (monitoring-process-output-specification self) activity-spec)
   (define (monitoring-process-output-type self) activity-level)
   (define (monitoring-process-proxy-flag self) sample-process-1-proxy)]

  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) sample-process-1)
   (define (valid? self)
     (and (valid-spec? self)
          (super-valid? (made-process (made-process-data-state self)
                                      (made-process-control-state self)))))])

(define sample-process-2-proxy (gen-proxy))
(struct sample-process-2 monitoring-process ()
  #:transparent
  #:methods gen:monitoring
  [(define (monitoring-process-output-specification self) exercise-spec)
   (define (monitoring-process-output-type self) exercise-event)
   (define (monitoring-process-proxy-flag self) sample-process-2-proxy)]

  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) sample-process-2)
   (define (valid? self)
     (and (valid-spec? self)
          (super-valid? (made-process (made-process-data-state self)
                                      (made-process-control-state self)))))])

(define m-proc-1 (sample-process-1 d-state c-state))
(define m-proc-2 (sample-process-2 d-state c-state))

(define output-1 (generate-data m-proc-1 null cur-dt))
(define output-2 (generate-data m-proc-2 null cur-dt))

; Verify implementation of generate-data for the Monitoring of observed properties.
(define (filter-ext dSet dt-start dt-end)
  (filter-measurements
   (remove-duplicates
    (filter (lambda (d)
              (not (made-data-proxy-flag d)))
            dSet))
   dt-start
   dt-end))

(define (verify-generate-property)
  (verify
   (assert
    (implies (is-proc-executed? c-state cur-dt)
             (eq? output-1
                  (list (activity-level
                         sample-process-1-proxy
                         cur-dt
                         (sum (filter-ext 
                               d-state
                               (dt- cur-dt property-window)
                               cur-dt)))))))))

; Verify implementation of generate-data for the Monitoring of observed events.
(define (get-event-start-dt d)
  (datetime-range-start (observed-event-valid-datetime-range d)))
(define (get-event-end-dt d)
  (datetime-range-end (observed-event-valid-datetime-range d)))

(define (verify-generate-event-id)
  (verify
   (assert
    (implies (not (null? output-2))
             (and (exercise-event? (list-ref output-2 0))
                  (eq? sample-process-2-proxy
                       (made-data-proxy-flag (list-ref output-2 0))))))))

(define (verify-generate-false-event)
  (verify
   (assert
    (implies (and (not (null? output-2))
                  (not (observed-event-value (list-ref output-2 0))))
             (and (eq? (get-event-end-dt (list-ref output-2 0))
                       cur-dt)
                  (event-start-pred
                   (filter-ext d-state (dt- cur-dt event-start-win) cur-dt))
                  (event-end-pred
                   (filter-ext d-state
                               (dt- (get-event-start-dt (list-ref output-2 0))
                                    event-end-win)
                               (get-event-start-dt (list-ref output-2 0))))
                  (andmap (lambda (dt-mid)
                            (implies (dt>? dt-mid
                                           (get-event-start-dt (list-ref output-2 0)))
                                     (not (event-end-pred
                                           (filter-ext d-state
                                                       (dt- dt-mid event-end-win)
                                                       dt-mid)))))
                          (map (lambda (d) (measurement-valid-datetime d))
                               (filter-ext d-state
                                           (get-event-start-dt (list-ref output-2 0))
                                           (get-event-end-dt (list-ref output-2 0))))))))))

(define (verify-generate-true-event)
  (verify
   (assert
    (implies (and (not (null? output-2))
                  (observed-event-value (list-ref output-2 0)))
             (and (eq? (get-event-end-dt (list-ref output-2 0))
                       cur-dt)
                  (event-end-pred
                   (filter-ext d-state (dt- cur-dt event-end-win) cur-dt))
                  (event-start-pred
                   (filter-ext d-state
                               (dt- (get-event-start-dt (list-ref output-2 0))
                                    event-start-win)
                               (get-event-start-dt (list-ref output-2 0))))
                  (andmap (lambda (dt-mid)
                            (implies (dt>? dt-mid
                                           (get-event-start-dt (list-ref output-2 0)))
                                     (not (event-start-pred
                                           (filter-ext d-state
                                                       (dt- dt-mid event-start-win)
                                                       dt-mid)))))
                          (map (lambda (d) (measurement-valid-datetime d))
                               (filter-ext d-state
                                           (get-event-start-dt (list-ref output-2 0))
                                           (get-event-end-dt (list-ref output-2 0))))))))))
