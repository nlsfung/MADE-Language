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
         (struct-out abstraction-triplet)
         filter-observations)
(provide verify-analysis
         execute-abstraction-pair
         execute-analysis-body)

; This file contains the implementation of Analysis processes.

; Analysis process inherit from the generic MADE process, extending it with
; the output type identifier and the output specification, which in turn
; comprises a set of abstraction triplets.
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
          (andmap (lambda (p) (and (abstraction-triplet? p)
                                   (valid? p)))
                  (analysis-process-output-specification self))))])

; An abstraction triplet consists of a time window, an abstraction predicate as
; well as an abstraction function.
(struct abstraction-triplet (time-window abstraction-predicate abstraction-function)
  #:transparent
  #:methods gen:typed
  [(define/generic super-valid? valid?)
   (define (get-type self) abstraction-triplet)
   (define (valid? self)
     (and (duration? (abstraction-triplet-time-window self))
          (super-valid? (abstraction-triplet-time-window self))
          (procedure? (abstraction-triplet-abstraction-predicate self))
          (procedure? (abstraction-triplet-abstraction-function self))))])

; Helper function for combining an abstraction predicate and an abstraction function.
; Used to convert abstraction triplets into abstraction pairs.
(define (combine-predicate-function pred func)
  (lambda (dSet)
    (if (pred dSet)
        (func dSet)
        (void))))

; Helper function for defining the main behaviour of analysis processes.
(define (execute-analysis-body d-list dt out-type ab-spec proxy-flag)
  ; To generate output data, an Analysis process follows the following steps:
  ; 1) Find an abstraction triplet from which an abstraction can be generated.
  ; 2) If none can be found, return an empty list.
  ;    Otherwise, return the generated abstraction.
  (let* ([output-list
          (map (lambda (ab-triplet)
                 (execute-abstraction-pair
                  d-list
                  dt
                  (abstraction-triplet-time-window ab-triplet)
                  out-type
                  (combine-predicate-function
                   (abstraction-triplet-abstraction-predicate ab-triplet)
                   (abstraction-triplet-abstraction-function ab-triplet))
                  proxy-flag))
               ab-spec)]
         [output (findf (lambda (d) (not (void? d))) output-list)])
    (if output
        (list output)
        null)))

; Helper function for executing an individual abstraction pair.
; An abstraction pair is equivalent to an abstraction triplet except that the
; abstraction predicate and abstraction function are combined.
(define (execute-abstraction-pair d-list dt t-window out-type ab-func proxy-flag)
  ; To generate output data from an abstraction pair, the following steps are followed:
  ; 1) Filter out any data that falls outside the time window.
  ; 2) Feed the filtered data into the input abstraction function.
  ; 3) If a (non-void) output value is produced, determine its valid datetime range.
  ; 4) Generate the output abstraction, if any.
  (let* ([filtered-data (sort (filter-observations d-list (dt- dt t-window) dt)
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
(define (filter-observations d-list dt-start dt-end)
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
  (define (display-solution d-list dt sol ab-triplet-1 ab-triplet-2 output-1 output-2)
    (if (eq? ab-triplet-1 ab-triplet-2)
        (displayln (format "Model for abstraction triplet: ~a" ab-triplet-1))
        (displayln (format "Model for abstraction triplets: ~a and ~a" ab-triplet-1 ab-triplet-2)))
    (if (eq? sol (unsat))
        (displayln (unsat))
        (begin
          (displayln "Input data:")
          (displayln (evaluate d-list sol))
          (displayln "Current date-time:")
          (displayln (evaluate dt sol))
          (displayln "Output data:")
          (cond [(string? ab-triplet-1)
                 (displayln (evaluate output-2 sol))]
                [(string? ab-triplet-2)
                 (displayln (evaluate output-1 sol))]
                [else 
                 (if (<= ab-triplet-1 ab-triplet-2)
                     (displayln (evaluate output-1 sol))
                     (displayln (evaluate output-2 sol)))])))
    (displayln ""))
  
  (let* ([d-list (foldl (lambda (d-list result) (append result d-list))
                                null
                                obs-gen-list)]

         [c-state (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t)]
         [proc (proc-constructor null c-state)]
         [out-type (analysis-process-output-type proc)]
         [proxy-flag (analysis-process-proxy-flag proc)]
         [proc-spec (analysis-process-output-specification proc)]

         [output-num (map (lambda (ab-triplet)
                            (- (length proc-spec) (length (member ab-triplet proc-spec))))
                          proc-spec)]
         [output-list (map (lambda (n)
                             (let* ([ab-triplet (list-ref proc-spec n)]
                                    [t-window (abstraction-triplet-time-window ab-triplet)]
                                    [ab-func (combine-predicate-function
                                              (abstraction-triplet-abstraction-predicate ab-triplet)
                                              (abstraction-triplet-abstraction-function ab-triplet))])
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
