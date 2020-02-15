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

(define activity-spec (property-specification (gen-window) sum))
(define exercise-spec
  (event-specification
   (event-trigger (gen-window) (lambda (d-list) (> (sum d-list) 25)))
   (event-trigger (gen-window) (lambda (d-list) (< (sum d-list) 5)))))

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

; Verify implementation of execute property body.
(define (verify-property-body)
  (verify (assert
           (implies (is-proc-executed? c-state cur-dt)
                    (and (not (null? output-1))
                         (activity-level? (list-ref output-1 0))
                         (<= (observed-property-value (list-ref output-1 0))
                             (sum d-state)))))))

; Verify implementation of execute event body.
(define over-acc-meas (filter (lambda (d) (and (> (measurement-value d) 25))) d-state))
(define start-win (event-trigger-time-window (event-specification-start-trigger exercise-spec)))
(define end-win (event-trigger-time-window (event-specification-end-trigger exercise-spec)))
(define (verify-event-body)
  (verify (assert
           (implies (and (dur=? start-win (duration 0 0 0 0))
                         (dur=? end-win (duration 0 0 0 0))
                         (= 4 (length (remove-duplicates
                                       (map (lambda (d) (measurement-valid-datetime d))
                                            d-state)))))
                    (and (implies (and (not (null? output-2))
                                       (exercise-event? (list-ref output-2 0))
                                       (not (observed-event-value (list-ref output-2 0))))
                                  (findf (lambda (d) (dt=? (measurement-valid-datetime d)
                                                           cur-dt))
                                         over-acc-meas))
                         (implies (and (not (null? output-2))
                                       (exercise-event? (list-ref output-2 0))
                                       (observed-event-value (list-ref output-2 0)))
                                  (findf (lambda (d) (dt<? (measurement-valid-datetime d)
                                                           cur-dt))
                                         over-acc-meas)))))))

; Verify the implementation of the proxy flags.
(define (verify-data-proxy)
  (verify
   (assert
    (implies (and (is-proc-executed? c-state cur-dt)
                  (= 4 (length (filter (lambda (d) (made-data-proxy-flag d)) d-state))))
             (and (not (null? output-1))
                  (activity-level? (list-ref output-1 0))
                  (= 0 (observed-property-value (list-ref output-1 0)))
                  (null? output-2))))))

(define (verify-proc-proxy-1)
  (verify #:assume
          (assert (not (null? output-1)))
          #:guarantee
          (assert
           (implies (proxy? m-proc-1)
                    (made-data-proxy-flag (list-ref output-1 0))))))

(define (verify-proc-proxy-2)
  (verify #:assume
          (assert (not (null? output-2)))
          #:guarantee
          (assert
           (implies (proxy? m-proc-2)
                    (made-data-proxy-flag (list-ref output-2 0))))))
