#lang rosette/safe

(require (only-in rosette string?))
(require "./MadeProcess.rkt")
(require "../rim/BasicDataTypes.rkt")
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

(provide gen:analysis
         analysis-process-output-type
         analysis-process-output-specification
         analysis-process-proxy-flag)
(provide (struct-out analysis-process)
         (struct-out abstraction-pair))
(provide verify-analysis
         (struct-out observation-generator)
         generate-observation-list
         execute-abstraction-pair)

; This file contains the implementation of Analysis processes.

; Analysis process inherit from the generic MADE process, extending it with
; the output type identifier and the output specification, which in turn
; comprises a set of abstraction pairs.
(define-generics analysis
  [analysis-process-output-type analysis]
  [analysis-process-output-specification analysis]
  [analysis-process-proxy-flag analysis])

(struct analysis-process made-process ()
  #:transparent
  #:methods gen:analysis []
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
  [(define (proxy? self) (analysis-process-proxy-flag self))

   (define (generate-data self in-data datetime)
     (gen-proc-generate-data
      (lambda (d-list dt)
        (execute-analysis-body d-list
                               dt
                               (analysis-process-output-type self)
                               (analysis-process-output-specification self)
                               (analysis-process-proxy-flag self)))
      self
      in-data
      datetime))
   
   (define/generic super-valid-spec? valid-spec?)
   (define (valid-spec? self)
     (and (super-valid-spec? (made-process null (made-process-control-state self)))
          (procedure? (analysis-process-output-type self))
          (list? (analysis-process-output-specification self))
          (andmap (lambda (p) (and (abstraction-pair? p)
                                   (valid? p)))
                  (analysis-process-output-specification self))))])

; An abstraction pair consists of a time window as well as an abstraction function.
(struct abstraction-pair (time-window abstraction-function)
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) abstraction-pair)
   (define (valid? self)
     (and (duration? (abstraction-pair-time-window self))
          (super-valid? (abstraction-pair-time-window self))
          (procedure? (abstraction-pair-abstraction-function self))))])

; Helper function for defining the main behaviour of analysis processes.
(define (execute-analysis-body d-list dt out-type ab-spec proxy-flag)
  ; To generate output data, an Analysis process follows the following steps:
  ; 1) Find an abstraction pair from which an abstraction can be generated.
  ; 2) If none can be found, return an empty list.
  ;    Otherwise, return the generated abstraction.
  (let* ([output-list
          (map (lambda (ab-pair)
                 (execute-abstraction-pair
                  d-list
                  dt
                  (abstraction-pair-time-window ab-pair)
                  out-type
                  (abstraction-pair-abstraction-function ab-pair)
                  proxy-flag))
               ab-spec)]
         [output (findf (lambda (d) (not (void? d))) output-list)])
    (if output
        (list output)
        null)))

; Helper function for executing an individual abstraction pair.
(define (execute-abstraction-pair d-list dt t-window out-type ab-func proxy-flag)
  ; To generate output data from an abstraction pair, the following steps are followed:
  ; 1) Filter out any data that falls outside the time window.
  ; 2) Feed the filtered data into the input abstraction function.
  ; 3) If a (non-void) output value is produced, determine its valid datetime range.
  ; 4) Generate the output abstraction, if any.
  (let* ([filtered-data (sort (filter-expired-data d-list (dt- dt t-window) dt)
                              observed-after?)]
         [output-value (if (null? filtered-data)
                           (void)
                           (ab-func filtered-data))]

         [latest-significant-data
          (if (void? output-value)
              (void)
              (foldl (lambda (d result)
                       (if (eq? output-value
                                (ab-func (member d (reverse filtered-data))))
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
(define (filter-expired-data d-list dt-start dt-end)
  (filter (lambda (d)
            (or (and (observed-property? d)
                     (dt-between? (observed-property-valid-datetime d)
                                  dt-start dt-end))
                (and (observed-event? d)
                     (dt-range-overlap?
                      (observed-event-valid-datetime-range d)
                      (datetime-range dt-start dt-end)))))
          d-list))

; Helper function to determine if a datetime range intersects with another.
(define (dt-range-overlap? range-1 range-2)
  (and (dt<=? (datetime-range-start range-1)
              (datetime-range-end range-2))
       (dt>=? (datetime-range-end range-1)
              (datetime-range-start range-2))))

; verify-analysis helps verify an Analysis process. 
; It accepts as input:
; 1) The struct-constructor for the analysis process.
; 2) A list of observation generators.
; 3) The execution datetime (which can be symbolic).
; The verifier outputs a model (if any) for each of the following conditions:
; 1) The input observations satisfy one abstraction specification.
;    (A seperate model is produced for each specification).
; 2) The input observations satisfy a pair of abstraction specifications.
;    (A separate model is produced for each pair).
; 3) The input observations satisfy only one abstraction specification in each pair.
;    (Only relevant if a model can be found that satisfy both).
(define (verify-analysis proc-constructor obs-gen-list dt)
  (define (display-solution d-list dt sol ab-pair-1 ab-pair-2 output-1 output-2)
    (if (eq? ab-pair-1 ab-pair-2)
        (displayln (format "Model for abstraction pair: ~a" ab-pair-1))
        (displayln (format "Model for abstraction pairs: ~a and ~a" ab-pair-1 ab-pair-2)))
    (if (eq? sol (unsat))
        (displayln (unsat))
        (begin
          (displayln "Input data:")
          (displayln (evaluate d-list sol))
          (displayln "Current date-time:")
          (displayln (evaluate dt sol))
          (displayln "Output data:")
          (cond [(string? ab-pair-1)
                 (displayln (evaluate output-2 sol))]
                [(string? ab-pair-2)
                 (displayln (evaluate output-1 sol))]
                [else 
                 (if (<= ab-pair-1 ab-pair-2)
                     (displayln (evaluate output-1 sol))
                     (displayln (evaluate output-2 sol)))])))
    (displayln ""))
  
  (let* ([d-list (foldl (lambda (generator result)
                                  (append result
                                          (generate-observation-list
                                           (observation-generator-getter generator)
                                           (observation-generator-start-datetime generator)
                                           (observation-generator-end-datetime generator)
                                           (observation-generator-frequency generator))))
                                null
                                obs-gen-list)]

         [c-state (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t)]
         [proc (proc-constructor null c-state)]
         [out-type (analysis-process-output-type proc)]
         [proxy-flag (analysis-process-proxy-flag proc)]
         [proc-spec (analysis-process-output-specification proc)]

         [output-num (map (lambda (ab-pair)
                            (- (length proc-spec) (length (member ab-pair proc-spec))))
                          proc-spec)]
         [output-list (map (lambda (n)
                             (let* ([ab-pair (list-ref proc-spec n)]
                                    [t-window (abstraction-pair-time-window ab-pair)]
                                    [ab-func (abstraction-pair-abstraction-function ab-pair)])
                               (execute-abstraction-pair d-list dt t-window out-type ab-func proxy-flag)))
                           output-num)]
         [output-unsat? (map (lambda (n)
                               (let* ([output (list-ref output-list n)]
                                      [sol (solve (assert (and (not (void? output))
                                                               (valid? output))))])
                                 (display-solution d-list dt sol n n output output)
                                 (eq? sol (unsat))))
                             output-num)])
    (for-each
     (lambda (m)
       (if (list-ref output-unsat? m)
           (void)
           (for-each
            (lambda (n)
              (if (list-ref output-unsat? n)
                  (void)
                  (let* ([output-1 (list-ref output-list m)]
                         [output-2 (list-ref output-list n)]
                         [sol-inter (solve (assert (and (not (void? output-1))
                                                        (not (void? output-2))
                                                        (valid? output-1)
                                                        (valid? output-2))))])
                    (display-solution d-list dt sol-inter m n output-1 output-2)
                    (if (eq? sol-inter (unsat))
                        (void)
                        (let ([sol-1 (solve (assert (and (not (void? output-1))
                                                         (void? output-2)
                                                         (valid? output-1))))]
                              [sol-2 (solve (assert (and (void? output-1)
                                                         (not (void? output-2))
                                                         (valid? output-2))))])
                          (display-solution d-list dt sol-1
                                            m (format "not ~a" n)
                                            output-1 output-2)
                          (display-solution d-list dt sol-2
                                            (format "not ~a" m) n
                                            output-1 output-2))))))
            (list-tail (member m output-num) 1))))
     output-num)
    (clear-asserts!)))

; Observation generator contains the specification for generating a list of
; symbolic observations (for verification purposes). It comprises:
; 1) An observation getter.
; 2) A starting date-time for the corresponding observations.
; 3) An ending date-time for the observations.
; 4) A frequency which can either be:
;    a) A duration indicating how often the observations should be repeated.
;    b) A positive integer indicating the number of observations to generate.
;       In this case, the start date-time indicates the earliest date-time for
;       the measurement and the end date-time latest.
(struct observation-generator
  (getter start-datetime end-datetime frequency)
  #:transparent)

; generate-observation-list generates a list of observations.
(define (generate-observation-list getter start-datetime end-datetime frequency)
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
  
  (let ([sample-data (getter)]
        [d-list (cond [(integer? frequency) (generate-count frequency)]
                      [(duration? frequency) (generate-interval start-datetime)])])
    (cond [(observed-property? sample-data)
           (assert (eq? (length d-list)
                        (length (remove-duplicates
                                 (map (lambda (d) (observed-property-valid-datetime d))
                                      d-list)))))]
          [(observed-event? sample-data)
           (assert (and (eq? (length d-list)
                             (length (remove-duplicates
                                      (map (lambda (d)
                                             (datetime-range-start
                                              (observed-event-valid-datetime-range d)))
                                           d-list))))
                        (eq? (length d-list)
                             (length (remove-duplicates
                                      (map (lambda (d)
                                             (datetime-range-end
                                              (observed-event-valid-datetime-range d)))
                                           d-list))))))])
    d-list))

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
;(define (grade-temp-low d-list)
;  (if (>= (length (filter (lambda (d)
;                            (and (room-temperature? d)
;                                 (< (observed-property-value d) 5)))
;                         d-list)) 3)
;      'low
;      (void)))
;(define (grade-temp-high d-list)
;  (if (>= (length (filter (lambda (d)
;                            (and (room-temperature? d)
;                                 (> (observed-property-value d) 10)))
;                         d-list)) 4)
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
;(define ab-spec
;  (list (abstraction-pair t-window grade-temp-low)
;           (abstraction-pair t-window grade-temp-high)))
;(define proc-proxy (gen-temp-proxy))
;  
;(struct sample-process analysis-process ()
;  #:methods gen:analysis
;  [(define (analysis-process-output-type self) room-temperature-grade)
;   (define (analysis-process-output-specification self) ab-spec)
;   (define (analysis-process-proxy-flag self) proc-proxy)]
;
;  #:methods gen:typed
;  [(define/generic super-valid? valid?)
;   (define (get-type self) sample-process)
;   (define (valid? self)
;     (and (valid-spec? self)
;          (super-valid? (made-process (made-process-data-state self)
;                                      (made-process-control-state self)))))])
;
;(define room-temp-proc (sample-process d-state c-state))
;
;(define output (generate-data room-temp-proc null cur-dt))
;
;; Verify the implementation of the control state.
;(define (verify-is-executed)
;  (verify (assert (implies (or (not (eq? sched-dt cur-dt))
;                               (not proc-status))
;                           (null? output)))))
;
;; Verify the implementation of the time window.
;(define (verify-time-window)
;  (verify (assert (implies (< win-length 2) (null? output)))))
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
;             (null? output)))))
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
;          (assert (and (not (null? output))
;                       (room-temperature-grade? (list-ref output 0))))
;          #:guarantee
;          (assert
;           (implies (proxy? room-temp-proc)
;                    (made-data-proxy-flag (list-ref output 0))))))
;
;; Verify the implementation of determining abstraction validity range.
;(define new-dt (datetime 7 9 12 (gen-dt-part) 0 0))
;(assert (normalized? new-dt))
;(define new-output (execute-analysis-body d-state new-dt out-type ab-spec #f))
;(define (verify-valid-range)
;  (verify (assert
;           (implies (and (not (null? output))
;                         (room-temperature-grade? (list-ref output 0))
;                         (dt>? new-dt (datetime-range-start
;                                       (abstraction-valid-datetime-range (list-ref output 0))))
;                         (dt<? new-dt (datetime-range-end
;                                       (abstraction-valid-datetime-range (list-ref output 0)))))
;                    (and (not (null? new-output))
;                         (room-temperature-grade? (list-ref new-output 0))
;                         (eq? (abstraction-value (list-ref new-output 0))
;                              (abstraction-value (list-ref output 0))))))))
