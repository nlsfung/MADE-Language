#lang rosette/safe

(require "../rpm/MadeProcess.rkt")
(require "../rpm/MonitoringProcess.rkt")
(require "../rpm/AnalysisProcess.rkt")
(require "../rpm/DecisionProcess.rkt")
(require "../rim/BasicDataTypes.rkt")
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

; This file contains the syntax for specifying new MADE processes.

; define-monitoring creates a new Monitoring process.
; It requires the following inputs:
; 1) An indicator whether the process is for a property or an event.
; 2) An identifier for the new process.
; 3) A boolean indicating whether the process is a proxy or not.
; 4) The output type of the process.
; 5) For processes that generate observed properties:
;    a) The time window for the process.
;    b) The value function for the process.
; 6) For processes that generate observed events:
;    a) The trigger (i.e. time window and predicate) for the start of the event.
;    b) The trigger for the end of the event.
(define-syntax (define-monitoring stx)
  (syntax-case stx ()
    [(define-monitoring #:property id proxy output-type (... duration) (... lambda))
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-boolean #'proxy stx)
       (raise-if-not-duration #'duration stx)
       (raise-if-not-lambda #'lambda 1 stx)
       (raise-if-not-identifier #'output-type stx)
       #'(struct id monitoring-process ()
           #:transparent
           #:methods gen:monitoring
           [(define (monitoring-process-output-specification self)
              (property-specification duration lambda))
            (define (monitoring-process-output-type self) output-type)
            (define (monitoring-process-proxy-flag self) proxy)]

           #:methods gen:typed
           [(define/generic super-valid? valid?)
            (define (get-type self) id)
            (define (valid? self)
              (and (valid-spec? self)
                   (super-valid? (made-process (made-process-data-state self)
                                               (made-process-control-state self)))))]))]

    [(define-monitoring #:event id proxy output-type (... start) (... end))
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-boolean #'proxy stx)
       (raise-if-not-identifier #'output-type stx)
       #'(struct id monitoring-process ()
           #:transparent
           #:methods gen:monitoring
           [(define (monitoring-process-output-specification self)
              (event-specification (define-event-trigger start)
                                   (define-event-trigger end)))
            (define (monitoring-process-output-type self) output-type)
            (define (monitoring-process-proxy-flag self) proxy)]

           #:methods gen:typed
           [(define/generic super-valid? valid?)
            (define (get-type self) id)
            (define (valid? self)
              (and (valid-spec? self)
                   (super-valid? (made-process (made-process-data-state self)
                                               (made-process-control-state self)))))]))]))

; define-analysis creates a new Analysis process.
; It requires the following inputs:
; 1) An identifier for the new process.
; 2) A boolean indicating whether the process is a proxy or not.
; 3) The output type of the process.
; 4) The time window for the process.
; 5) A list of abstraction functions for the process.
(define-syntax (define-analysis stx)
  (syntax-case stx ()
    [(define-analysis id proxy output-type (... duration) (... lambda) ...)
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-boolean #'proxy stx)
       (raise-if-not-identifier #'output-type stx)
       (raise-if-not-duration #'duration stx)
       (raise-if-not-lambda #'(lambda ...) 1 stx)
       #'(struct id analysis-process ()
           #:methods gen:analysis
           [(define (analysis-process-time-window self) duration)
            (define (analysis-process-output-type self) output-type)
            (define (analysis-process-abstraction-functions self) (list lambda ...))
            (define (analysis-process-proxy-flag self) proxy)]

           #:methods gen:typed
           [(define/generic super-valid? valid?)
            (define (get-type self) sample-process)
            (define (valid? self)
              (and (valid-spec? self)
                   (super-valid? (made-process (made-process-data-state self)
                                               (made-process-control-state self)))))]))]))

; define-decision creates a new Decision process.
; It requires the following inputs:
; 1) An identifier for the new process.
; 2) A boolean indicating whether the process is a proxy or not.
; 3) The output type of the process.
; 4) A list of instruction templates that constitute the plan.
; 5) A list of predicates which constitute the decision criteria of the process.
(define-syntax (define-decision stx)
  (syntax-case stx ()
    [(define-decision id proxy output-type (#:instructions (... inst) ...) (#:criteria (... lambda) ...))
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-boolean #'proxy stx)
       (raise-if-not-identifier #'output-type stx)
       (raise-if-not-lambda #'(lambda ...) 1 stx)
       #'(struct id decision-process ()
           #:transparent
           #:methods gen:decision
           [(define (decision-process-plan-template self)
              (plan-template output-type (define-instruction-template-list inst ...)))
            (define (decision-process-decision-criteria self) (list lambda ...))
            (define (decision-process-proxy-flag self) proxy)]
           
           #:methods gen:typed
           [(define/generic super-valid? valid?)
            (define (get-type self) sample-process)
            (define (valid? self)
              (and (valid-spec? self)
                   (super-valid? (made-process (made-process-data-state self)
                                               (made-process-control-state self)))))]))]))

; Helper function for raising syntax error if input is not an identifier or a
; list of identifiers. Returns the syntax object for void if the check succeeds.
(define-for-syntax (raise-if-not-identifier stx src-stx)
  (syntax-case stx ()
    [(id)
     (if (identifier? #'id)
         #'(void)
         (raise-syntax-error #f "identifier expected." src-stx #'id))]
    [(id-1 id-2 ...)
     (if (identifier? #'id-1)
         (raise-if-not-identifier #'(id-2 ...) src-stx)
         (raise-syntax-error #f "identifier expected." src-stx #'id-1))]
    [id
     (if (identifier? #'id)
         #'(void)
         (raise-syntax-error #f "identifier expected." src-stx #'id))]))

; Helper function for raising syntax error if input is not an integer or a
; list of integers. Returns the syntax object for void if the check succeeds.
(define-for-syntax (raise-if-not-integer stx src-stx)
  (syntax-case stx ()
    [(num)
     (if (integer? (syntax->datum #'num))
         #'(void)
         (raise-syntax-error #f "integer expected." src-stx #'num))]
    [(num-1 num-2 ...)
     (if (integer? (syntax->datum #'num-1))
         (raise-if-not-integer #'(num-2 ...) src-stx)
         (raise-syntax-error #f "integer expected." src-stx #'num-1))]
    [num
     (if (integer? (syntax->datum #'num))
         #'(void)
         (raise-syntax-error #f "integer expected." src-stx #'num))]))

; Helper function for raising syntax error if input is not a real or a list of
; real numbers. Returns the syntax object for void if the check succeeds.
(define-for-syntax (raise-if-not-real stx src-stx)
  (syntax-case stx ()
    [(num)
     (if (real? (syntax->datum #'num))
         #'(void)
         (raise-syntax-error #f "real expected." src-stx #'num))]
    [(num-1 num-2 ...)
     (if (real? (syntax->datum #'num-1))
         (raise-if-not-integer #'(num-2 ...) src-stx)
         (raise-syntax-error #f "real expected." src-stx #'num-1))]
    [num
     (if (real? (syntax->datum #'num))
         #'(void)
         (raise-syntax-error #f "real expected." src-stx #'num))]))

; Helper function for raising syntax error if input is not a boolean or a
; list of booleans. Returns the syntax object for void if the check succeeds.
(define-for-syntax (raise-if-not-boolean stx src-stx)
  (syntax-case stx ()
    [(bool)
     (if (boolean? (syntax->datum #'bool))
         #'(void)
         (raise-syntax-error #f "boolean expected." src-stx #'bool))]
    [(bool-1 bool-2 ...)
     (if (boolean? (syntax->datum #'bool-1))
         (raise-if-not-boolean #'(bool-2 ...) src-stx)
         (raise-syntax-error #f "boolean expected." src-stx #'bool-1))]
    [bool
     (if (boolean? (syntax->datum #'bool))
         #'(void)
         (raise-syntax-error #f "boolean expected." src-stx #'bool))]))

; Helper function for raising syntax error if input is not a duration or a list
; of durations. Returns the syntax object for void if the check succeeds.
(define-for-syntax (raise-if-not-duration stx src-stx)
  (syntax-case stx ()
    [((duration int-1 int-2 int-3 int-4))
     (raise-if-not-duration #'(duration int-1 int-2 int-3 int-4) src-stx)]
    [((duration int-1 int-2 int-3 int-4) (... duration-2) ...)
     (begin
       (raise-if-not-duration #'(duration int-1 int-2 int-3 int-4) src-stx)
       (raise-if-not-duration #'(duration-2 ...) src-stx))]
    [(duration int-1 int-2 int-3 int-4)
     (cond [(not (eq? (syntax->datum #'duration) 'duration))
            (raise-syntax-error #f "duration expected." src-stx #'duration)]
           [else (begin (raise-if-not-integer #'(int-1 int-2 int-3 int-4) src-stx)
                        #'(void))])]
    [((... rest)) (raise-syntax-error #f "duration expected." src-stx #'rest)]
    [(... rest) (raise-syntax-error #f "duration expected." src-stx #'rest)]))

; Helper function for raising syntax error if input is not a lambda expression
; or a list of lambda expressions of the specified arity. If the arity is not
; an integer, then it is ignored.
; Returns the syntax object for void if the check succeeds.
(define-for-syntax (raise-if-not-lambda stx arity src-stx)
  (syntax-case stx ()
    [((lambda (... arg) rest ...))
     (raise-if-not-lambda #'(lambda (... arg) rest ...) arity src-stx)]
    [((lambda (... arg) rest ...) (... lambda-2) ...)
     (begin
       (raise-if-not-lambda #'(lambda (... arg) rest ...) arity src-stx)
       (raise-if-not-lambda #'(lambda-2 ...) arity src-stx))]
    [(lambda (... arg) rest ...)
     (cond [(not (eq? (syntax->datum #'lambda) 'lambda))
            (raise-syntax-error #f "lambda expected." src-stx #'lambda)]
           [(not (eq? (length (syntax->datum #'arg)) 1))
            (raise-syntax-error #f "arity of 1 expected." src-stx #'arg)]
           [else (begin (raise-if-not-identifier #'arg src-stx)
                        #'(void))])]
    [((... rest))
     (raise-syntax-error #f "lambda expression expected." src-stx #'rest)]
    [(... rest)
     (raise-syntax-error #f "lambda expression expected." src-stx #'rest)]))

; Helper function for raising syntax error if input is not a dimensioned value.
; Returns the syntax object for void if the check succeeds.
(define-for-syntax (raise-if-not-dimensioned stx src-stx)
  (syntax-case stx ()
    [(dimensioned value units)
     (cond [(not (eq? (syntax->datum #'dimensioned) 'dimensioned))
            (raise-syntax-error #f "dimensioned expected." src-stx #'dimensioned)]
           [else (begin (raise-if-not-real #'value src-stx)
                        #'(void))])]
    [(... rest)
     (raise-syntax-error #f "dimensioned value expected." src-stx #'rest)]))

; Helper function for raising syntax error if input is not of a basic data type.
; Returns the syntax object for void if the check succeeds.
; Note: Nominal and enumerated data types are not checked becuase they are
; specified at run-time.
(define-for-syntax (raise-if-not-basic stx src-stx)
  (syntax-case stx ()
    [(type value) (eq? (syntax->datum #'type) 'id)
                  (if (not (symbol? (syntax->datum #'value)))
                      (raise-syntax-error #f "symbol expected." src-stx #'value)
                      #'(void))]
    [(type value) (eq? (syntax->datum #'type) 'bool)
                  (if (not (boolean? (syntax->datum #'value)))
                      (raise-syntax-error #f "boolean expected." src-stx #'value)
                      #'(void))]
    [(type value) (eq? (syntax->datum #'type) 'count)
                  (if (not (integer? (syntax->datum #'value)))
                      (raise-syntax-error #f "integer expected." src-stx #'value)
                      #'(void))]
    [(type value) (eq? (syntax->datum #'type) 'proportion)
                  (if (not (rational? (syntax->datum #'value)))
                      (raise-syntax-error #f "rational expected." src-stx #'value)
                      #'(void))]
    [(type value) #'(void)]

    [(dimensioned value units)
     (raise-if-not-dimensioned #'(dimensioned value units) src-stx)]

    [(... rest)
     (raise-syntax-error #f "basic data value expected." src-stx #'rest)]))

; Helper function for parsing the syntax of event triggers.
(define-syntax (define-event-trigger stx)
  (syntax-case stx ()
    [(define-event-trigger (event-trigger (... window) (... predicate)))
     (begin
       (raise-if-not-duration #'window stx)
       (raise-if-not-lambda #'predicate 1 stx)
       (if (not (eq? (syntax->datum #'event-trigger) 'event-trigger))
           (raise-syntax-error #f "event trigger expected." stx #'event-trigger)
           #'(event-trigger window predicate)))]))

; Helper function for parsing the syntax for lists of instruction templates.
(define-syntax (define-instruction-template-list stx)
  (syntax-case stx ()
    [(_ (template-type target-type (... relative-schedule) rest ...))
     #'(list (define-instruction-template template-type target-type relative-schedule rest ...))]
    [(_ (template-type target-type rest))
     #'(list (define-instruction-template template-type target-type rest))]
    [(_ (template-type target-type (... relative-schedule)))
     #'(list (define-instruction-template template-type target-type relative-schedule))]

    [(_ (template-type target-type (... relative-schedule) rest ...) (... template) ...)
     #'(append (define-instruction-template-list (template-type target-type (... relative-schedule) rest ...))
               (define-instruction-template-list template ...))]
    [(_ (template-type target-type rest) (... template) ...)
     #'(append (define-instruction-template-list (template-type target-type rest))
               (define-instruction-template-list template ...))]
    [(_ (template-type target-type (... relative-schedule)) (... template) ...)
     #'(append (define-instruction-template-list (template-type target-type (... relative-schedule)))
               (define-instruction-template-list template ...))]))

; Helper function for parsing the syntax for instruction templates.
(define-syntax (define-instruction-template stx)
  (syntax-case stx ()
    [(_ control-template target-process (... relative-schedule) status)
     (eq? (syntax->datum #'control-template) 'control-template)
     (begin
       (raise-if-not-identifier #'target-process stx)
       (raise-if-not-boolean #'status stx)
       #'(control-template target-process
                           (define-relative-schedule relative-schedule)
                           status))]
    [(_ control-template target-process status)
     (eq? (syntax->datum #'control-template) 'control-template)
     (begin
       (raise-if-not-identifier #'target-process stx)
       (raise-if-not-boolean #'status stx)
       #'(control-template target-process (void) status))]
    [(_ control-template target-process (... relative-schedule))
     (eq? (syntax->datum #'control-template) 'control-template)
     (begin
       (raise-if-not-identifier #'target-process stx)
       #'(control-template target-process
                           (define-relative-schedule relative-schedule)
                           (void)))]

    [(_ action-template action-type (... relative-schedule) rate (... duration))
     (eq? (syntax->datum #'action-template) 'homogeneous-action-template)
     (begin
       (raise-if-not-identifier #'action-type stx)
       (raise-if-not-dimensioned #'rate stx)
       (raise-if-not-duration #'duration stx)
       #'(homogeneous-action-template action-type
                                      (define-relative-schedule relative-schedule)
                                      rate
                                      duration))]

    [(_ action-template action-type (... relative-schedule) goal-state)
     (eq? (syntax->datum #'action-template) 'culminating-action-template)
     (begin
       (raise-if-not-identifier #'action-type stx)
       (raise-if-not-basic #'goal-state stx)
       #'(culminating-action-template action-type
                                      (define-relative-schedule relative-schedule)
                                      goal-state))]))

; Helper function for parsing the syntax for relative schedules.
(define-syntax (define-relative-schedule stx)
  (syntax-case stx ()
    [(_ (rel-sched #:rounding (... rounding) #:offset (... offset)
                   #:pattern (... pattern) ... #:interval #f))
     (begin
       (raise-if-not-duration #'rounding stx)
       (raise-if-not-duration #'offset stx)
       (raise-if-not-duration #'(pattern ...) stx)
       (if (not (eq? (syntax->datum #'rel-sched) 'relative-schedule))
           (raise-syntax-error #f "relative scheduled expected." stx #'rel-sched)
           #'(relative-schedule rounding offset (list pattern ...) #f)))]
    
    [(_ (rel-sched #:rounding (... rounding) #:offset (... offset)
                   #:pattern (... pattern) ... #:interval (... interval)))
     (begin
       (raise-if-not-duration #'rounding stx)
       (raise-if-not-duration #'offset stx)
       (raise-if-not-duration #'(pattern ...) stx)
       (raise-if-not-duration #'interval stx)
       (if (not (eq? (syntax->datum #'rel-sched) 'relative-schedule))
           (raise-syntax-error #f "relative scheduled expected." stx #'rel-sched)
           #'(relative-schedule rounding offset (list pattern ...) interval)))]))
