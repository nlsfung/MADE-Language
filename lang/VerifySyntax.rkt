#lang rosette/safe

(require (only-in rosette for/list))
(require (for-syntax "./SyntaxUtil.rkt"))
(require "../rim/BasicDataTypes.rkt")
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt"
         "../rpm/MonitoringProcess.rkt"
         "../rpm/AnalysisProcess.rkt"
         "../rpm/DecisionProcess.rkt"
         "../rpm/EffectuationProcess.rkt")

(provide get-duration
         get-schedule
         get-status
         get-proxy
         get-dimensioned
         get-bool
         get-count
         get-proportion
         get-datetime
         verify-getter
         verify-archetype
         verify-process
         generate-list)

; This file contains the syntax of functions for verifying concrete MADE models.

; get-datetime creates a symbolic datetime values. It accepts as input two
; optional concrete datetime values to indicate the range of possible datetime
; values.
(define get-datetime
  (case-lambda
    [()
     (get-datetime (datetime 2019 12 15 0 0 0)
                   (datetime 2019 12 15 23 0 0))]
    [(start-dt end-dt)
     (datetime (get-datetime-part (datetime-year start-dt)
                                  (datetime-year end-dt))
               (get-datetime-part (datetime-month start-dt)
                                  (datetime-month end-dt))
               (get-datetime-part (datetime-day start-dt)
                                  (datetime-day end-dt))
               (get-datetime-part (datetime-hour start-dt)
                                  (datetime-hour end-dt))
               (get-datetime-part (datetime-minute start-dt)
                                  (datetime-minute end-dt))
               (get-datetime-part (datetime-second start-dt)
                                  (datetime-second end-dt)))]))

(define (get-datetime-part lo hi)
  (if (= lo hi)
      lo
      (begin
  (define-symbolic* dt-part integer?)
  (assert (and (>= dt-part lo) (<= dt-part hi)))
  dt-part)))

; get-duration creates a symbolic duration value.
(define (get-duration)
  (define (get-dur-part)
    (define-symbolic* dur-part integer?)
    dur-part)
  (duration (get-dur-part) (get-dur-part) (get-dur-part) (get-dur-part)))

; get-schedule creates a symbolic schedule value.
; Accepts as argument the number of datetime values in the starting pattern.
(define-syntax (get-schedule stx)
  (syntax-case stx ()
    [(get-schedule start-dt end-dt max)
     #'(schedule
        (for/list ([n (- max 1)]) (get-datetime start-dt end-dt))
        (get-interval))]
    [(get-schedule max)
     #'(schedule
        (for/list ([n (- max 1)]) (get-datetime))
        (get-interval))]))

; get-interval creates a symbolic value for the repeat interval of a schedule.
(define (get-interval)
  (define-symbolic* bool-int? boolean?)
  (define-symbolic* bool-val boolean?)
  (if bool-int? bool-val (get-duration)))

; get-proxy creates a symbolic boolean value for the proxy.
(define (get-proxy)
  (define-symbolic* proxy boolean?)
  proxy)

; get-status creates a symbolic boolean value for the process status.
(define (get-status)
  (define-symbolic* status boolean?)
  status)

; get-dimensioned creates a symbolic dimensioned value.
; It requires the appropriate units to be provided.
(define (get-dimensioned units)
  (define-symbolic* dim real?)
  (dimensioned dim units))

; get-bool creates a symbolic bool value.
(define (get-bool)
  (define-symbolic* bool-val boolean?)
  (bool bool-val))

; get-count creates a symbolic count value.
(define (get-count)
  (define-symbolic* count-val integer?)
  (count count-val))

; get-proportion creates a symbolic proportion value.
(define (get-proportion)
  (define-symbolic* prop-val real?)
  (proportion prop-val))

; verify-getter checks whether the input getter can return a valid instance. If
; not a warning is displayed. If yes, then an example is displayed.
(define (verify-getter get-id id)
  (let* ([example-1 (get-id)]
         [example-2 (get-id)]
         [solution (solve (assert (and (valid? example-1)
                                       (valid? example-2)
                                       (not (eq? example-1 example-2)))))])
    (if (eq? solution (unsat))
        (displayln (format "Valid ~a cannot be generated from specification." id))
        (begin
          (displayln (format "Example ~a: " id))
          (displayln (evaluate example-1 solution))
          (displayln "")))
    (clear-asserts!)))

; verify-archetype checks whether the specification of the input archetype is
; satisfiable or not.
(define-syntax (verify-archetype stx)
  (syntax-case stx ()
    [(verify-archetype id)
     (begin
       (raise-if-not-identifier #'id stx)
       (with-syntax ([get-id (build-getter-name #'id)])
         #'(let* ([example (get-id)]
                  [solution (solve (assert (valid? example)))])
             (displayln (format "Example ~a: " id))
             (if (eq? solution (unsat))
                 (displayln (unsat))
                 (displayln (evaluate example solution)))
             (displayln "")
             (clear-asserts!))))]))

; verify-process performs various checks on the input process depending on its type.
; It accepts as input a process constructor, a list of list of MADE archetype instances
; (which can be symbolic) and an execution datetime.
(define (verify-process proc gen-list dt)
  (let* ([example (proc null null)])
    (cond [(monitoring-process? example)
           (verify-monitoring proc gen-list dt)]
          [(analysis-process? example)
           (verify-analysis proc gen-list dt)]
          [(decision-process? example)
           (verify-decision proc gen-list dt)]
          [(effectuation-process? example)
           (verify-effectuation proc gen-list dt)])))

; generate-list generates a list of MADE data items. It accepts as input the
; target archetype ID, the starting datetime and the ending datetime for the
; archetype instances, a repeat frequency which can be either an integer or
; duration, and an optional list of target instructions for action plans.
(define-syntax (generate-list stx)
  (syntax-case stx ()
    [(generate-list id start-dt end-dt freq)
     #'(generate-list id start-dt end-dt freq #f)]
    
    [(generate-list id start-dt end-dt freq target-id ...)
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-datetime #'start-dt stx)
       (raise-if-not-datetime #'end-dt stx)
       (with-syntax ([get-id (build-getter-name #'id)])          
         #'(let* ([example (get-id)])
             (cond [(measurement? example)
                    (generate-measurement-list get-id start-dt end-dt freq)]
                   [(observation? example)
                    (generate-observation-list get-id start-dt end-dt freq)]
                   [(abstraction? example)
                    (generate-abstraction-list get-id start-dt end-dt freq)]
                   [(action-plan? example)
                    (generate-action-plan-list get-id start-dt end-dt freq target-id ...)]
                   [(or (control-instruction? example) (action-instruction? example))
                    (generate-instruction-list get-id start-dt end-dt freq)]))))]))

; generate-measurement-list generates a list of measurements. It accepts as arguments:
; 1) A measurement getter.
; 2) A starting date-time for the corresponding measurements.
; 3) An ending date-time for the measurements.
; 4) A frequency which can either be:
;    a) A duration indicating how often the measurements should be repeated.
;    b) A positive integer indicating the total number of measurements between
;       the given start date-time and end-datetime.
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
                 (length (remove-duplicates (map (lambda (d) (measurement-valid-datetime d))
                                                 d-list)))))
    d-list))

; generate-observation-list generates a list of observations. It accepts:
; 1) An observation getter.
; 2) A starting date-time for the corresponding observations.
; 3) An ending date-time for the observations.
; 4) A frequency which can either be:
;    a) A duration indicating how often the observations should be repeated.
;    b) A positive integer indicating the number of observations to generate.
;       In this case, the start date-time indicates the earliest date-time for
;       the measurement and the end date-time latest.
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

; generate-abstraction-list generates a list of abstractions. It accepts as arguments:
; 1) An abstraction getter.
; 2) A starting date-time for the corresponding abstraction.
; 3) An ending date-time for the abstraction.
; 4) A frequency which can either be:
;    a) A duration indicating how often the observations should be repeated.
;    b) A positive integer indicating the number of observations to generate.
;       In this case, the start date-time indicates the earliest date-time for
;       the measurement and the end date-time latest.
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

; generate-action-plan generates a list of action plans. It accepts:
; 1) An action plan getter.
; 2) The earliest date-time for the action plans.
; 3) The latest date-time for the action plans.
; 4) A frequency which can either be:
;    a) A duration indicating how often the action plans should be repeated.
;    b) A positive integer indicating the number of action plans to generate.
; 5) An optional list of symbols indicating specific instruction types to include in the plan.
;    If set to #f, all instruction types will be included.
(define (generate-action-plan-list getter start-datetime end-datetime frequency target-list)
  (define (generate-count total)
    (if (or (<= total 0) (dt>? start-datetime end-datetime))
        null
        (let ([data (if (not target-list)
                        (getter start-datetime end-datetime)
                        (getter start-datetime end-datetime target-list))])
          (assert (valid? data))
          (append (list data)
                  (generate-count (- total 1))))))
  
  (define (generate-interval cur-dt)  
    (if (dt>? cur-dt end-datetime)
        null
        (let ([data (if (not target-list)
                        (getter cur-dt cur-dt)
                        (getter cur-dt cur-dt target-list))]
              [next-dt (dt+ cur-dt frequency)])
          (assert (valid? data))
          (append (list data)
                  (generate-interval next-dt)))))
  
  (let ([d-list (cond [(integer? frequency) (generate-count frequency)]
                      [(duration? frequency) (generate-interval start-datetime)])])
    (assert (eq? (length d-list)
                      (length (remove-duplicates
                               (map (lambda (d)
                                      (action-plan-valid-datetime d))
                                    d-list)))))
    d-list))

; generate-instruction-list generates a list of control or action instructions. It accepts:
; 1) An instruction getter.
; 2) A starting date-time for the corresponding instructions.
; 3) An ending date-time for the instructions.
; 4) A frequency which can either be:
;    a) A duration indicating how often the observations should be repeated.
;    b) A positive integer indicating the number of observations to generate.
;       In this case, the start date-time indicates the earliest date-time for
;       the measurement and the end date-time latest.
(define (generate-instruction-list getter start-datetime end-datetime frequency)
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
  
  (let ([sample (getter)]
        [d-list (cond [(integer? frequency) (generate-count frequency)]
                      [(duration? frequency) (generate-interval start-datetime)])])
    (cond [(control-instruction? sample)
           (assert (eq? (length d-list)
                      (length (remove-duplicates
                               (map (lambda (d) (control-instruction-valid-datetime d))
                                    d-list)))))]
          [(homogeneous-action? sample)
           (assert (eq? (length d-list)
                      (length (remove-duplicates
                               (map (lambda (d) (homogeneous-action-start-datetime d))
                                    d-list)))))]
          [(culminating-action? sample)
           (assert (eq? (length d-list)
                      (length (remove-duplicates
                               (map (lambda (d) (culminating-action-start-datetime d))
                                    d-list)))))])
    d-list))

