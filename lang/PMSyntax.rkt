#lang rosette/safe

(require "../rpm/MadeProcess.rkt")
(require "../rpm/MonitoringProcess.rkt")
(require "../rpm/AnalysisProcess.rkt")
(require "../rpm/DecisionProcess.rkt")
(require "../rpm/EffectuationProcess.rkt")
(require "../rim/BasicDataTypes.rkt")
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")
(require (for-syntax "./SyntaxUtil.rkt"))

(provide define-monitoring
         define-analysis
         define-decision
         define-effectuation)

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

; define-analysis creates a new Analysis process.
; It requires the following inputs:
; 1) An identifier for the new process.
; 2) A boolean indicating whether the process is a proxy or not.
; 3) The output type of the process.
; 4) A list of abstraction pairs.
(define-syntax (define-analysis stx)
  (syntax-case stx ()
    [(define-analysis id proxy output-type ab-pair ...)
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-boolean #'proxy stx)
       (raise-if-not-identifier #'output-type stx)
       #'(struct id analysis-process ()
           #:transparent
           #:methods gen:analysis
           [(define (analysis-process-time-window self) duration)
            (define (analysis-process-output-type self) output-type)
            (define (analysis-process-output-specification self)
              (define-analysis-specification ab-pair ...))
            (define (analysis-process-proxy-flag self) proxy)]

           #:methods gen:typed
           [(define/generic super-valid? valid?)
            (define (get-type self) id)
            (define (valid? self)
              (and (valid-spec? self)
                   (super-valid? (made-process (made-process-data-state self)
                                               (made-process-control-state self)))))]))]))

; define-analysis-specification creates a new list of abstraction pairs.
; It requires a list of pairs as inputs, each containing:
; 1) A time window for the abstraction.
; 2) A function for outputting the appropriate abstraction value.
(define-syntax (define-analysis-specification stx)
  (syntax-case stx ()
    [(define-analysis-specification ((... duration) (... lambda)))
     (begin
       (raise-if-not-duration #'duration stx)
       (raise-if-not-lambda #'lambda 1 stx)
       #'(list (abstraction-pair duration lambda)))]
    [(define-analysis-specification ((... duration) (... lambda)) ab-pair-2 ...)
     (begin
       #'(append (define-analysis-specification (duration lambda))
                 (define-analysis-specification ab-pair-2 ...)))]))

; define-decision creates a new Decision process.
; It requires the following inputs:
; 1) An identifier for the new process.
; 2) A boolean indicating whether the process is a proxy or not.
; 3) The output type of the process.
; 4) A list of instruction templates that constitute the plan.
; 5) A list of predicates which constitute the decision criteria of the process.
; Note: The relative schedules in the instruction templates must contain a
; non-empty list of datetime patterns.
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
       (raise-if-not-symbol #'target-process stx)
       (raise-if-not-boolean #'status stx)
       #'(control-template target-process
                           (define-relative-schedule relative-schedule)
                           status))]
    [(_ control-template target-process status)
     (eq? (syntax->datum #'control-template) 'control-template)
     (begin
       (raise-if-not-symbol #'target-process stx)
       (raise-if-not-boolean #'status stx)
       #'(control-template target-process (void) status))]
    [(_ control-template target-process (... relative-schedule))
     (eq? (syntax->datum #'control-template) 'control-template)
     (begin
       (raise-if-not-symbol #'target-process stx)
       #'(control-template target-process
                           (define-relative-schedule relative-schedule)
                           (void)))]

    [(_ action-template action-type (... relative-schedule) rate (... duration))
     (eq? (syntax->datum #'action-template) 'homogeneous-action-template)
     (begin
       (raise-if-not-symbol #'action-type stx)
       (raise-if-not-dimensioned #'rate stx)
       (raise-if-not-duration #'duration stx)
       #'(homogeneous-action-template action-type
                                      (define-relative-schedule relative-schedule)
                                      rate
                                      duration))]

    [(_ action-template action-type (... relative-schedule) goal-state)
     (eq? (syntax->datum #'action-template) 'culminating-action-template)
     (begin
       (raise-if-not-symbol #'action-type stx)
       (raise-if-not-basic #'goal-state stx)
       #'(culminating-action-template action-type
                                      (define-relative-schedule relative-schedule)
                                      goal-state))]))

; Helper function for parsing the syntax for relative schedules.
(define-syntax (define-relative-schedule stx)
  (syntax-case stx ()
    [(_ (rel-sched #:rounding (... rounding) #:offset (... offset)
                   #:pattern (... pattern) ... #:interval bool-val))
     (boolean? (syntax->datum #'bool-val))
     (begin
       (raise-if-not-duration #'rounding stx)
       (raise-if-not-duration #'offset stx)
       (raise-if-not-duration #'(pattern ...) stx)
       (raise-if-not-boolean #'bool-val stx)
       (if (not (eq? (syntax->datum #'rel-sched) 'relative-schedule))
           (raise-syntax-error #f "relative scheduled expected." stx #'rel-sched)
           #'(relative-schedule rounding offset (list pattern ...) bool-val)))]
    
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

; define-effectuation creates a new Effectuation process.
; It requires the following inputs:
; It requires the following inputs:
; 1) An identifier for the new process.
; 2) A boolean indicating whether the process is a proxy or not.
; 3) The output type of the process.
; 4) A list of target schedules, each containing:
;    a) The target plan type.
;    b) The target instruction type.
;    c) A predicte on the target scheduled instruction.
(define-syntax (define-effectuation stx)
  (syntax-case stx ()
    [(define-effectuation id proxy output-type (... target) ...)
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-boolean #'proxy stx)
       (raise-if-not-identifier #'output-type stx)
       #'(struct id effectuation-process ()
           #:transparent
           #:methods gen:effectuation
           [(define (effectuation-process-target-schedules self)
              (define-target-schedule-list target ...))
            (define (effectuation-process-output-type self) output-type)
            (define (effectuation-process-proxy-flag self) proxy)]

           #:methods gen:typed
           [(define/generic super-valid? valid?)
            (define (get-type self) sample-process)
            (define (valid? self)
              (and (valid-spec? self)
                   (super-valid? (made-process (made-process-data-state self)
                                               (made-process-control-state self)))))]))]))

; Helper function for parsing the syntax for lists of target schedules.
(define-syntax (define-target-schedule-list stx)
  (syntax-case stx ()
    [(_ (target-schedule #:plan plan-type #:instruction inst-type #:predicate pred))
     #'(list (define-target-schedule target-schedule plan-type inst-type pred))]

    [(_ (target-schedule #:plan plan-type #:instruction inst-type #:predicate pred) (... target) ...)
     #'(append (list (define-target-schedule target-schedule plan-type inst-type pred))
               (define-target-schedule-list target ...))]))

; Helper function for parsing the syntax for target schedules.
(define-syntax (define-target-schedule stx)
  (syntax-case stx ()
    [(_ target-schedule plan-type inst-type pred)
     (if (not (eq? (syntax->datum #'target-schedule) 'target-schedule))
         (raise-syntax-error #f "target schedule expected." stx #'target-schedule)
         (begin
           (raise-if-not-identifier #'plan-type stx)
           (raise-if-not-identifier #'inst-type stx)
           (raise-if-not-lambda #'pred 1 stx)
           #'(target-schedule plan-type inst-type pred)))]))
