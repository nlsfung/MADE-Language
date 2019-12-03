#lang rosette/safe

(require "../rpm/MadeProcess.rkt")
(require "../rpm/MonitoringProcess.rkt")
(require "../rim/BasicDataTypes.rkt")
(require "../rim/TemporalDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

; This file contains the syntax for specifying new MADE processes.

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

; Helper function for raising syntax error if input is not a duration.
; Returns the syntax object for void if the check succeeds.
(define-for-syntax (raise-if-not-duration stx src-stx)
  (syntax-case stx ()
    [(duration int-1 int-2 int-3 int-4)
     (cond [(not (eq? (syntax->datum #'duration) 'duration))
            (raise-syntax-error #f "duration expected." src-stx #'duration)]
           [else (begin (raise-if-not-integer #'(int-1 int-2 int-3 int-4) src-stx)
                        #'(void))])]
    [(... _) (raise-syntax-error #f "duration expected." src-stx stx)]))

; Helper function for raising syntax error if input is not a lambda expression
; of the specified arity. If the arity is not an integer, then it is ignored.
; Returns the syntax object for void if the check succeeds.
(define-for-syntax (raise-if-not-lambda stx arity src-stx)
  (syntax-case stx ()
    [(lambda (... arg) rest)
     (cond [(not (eq? (syntax->datum #'lambda) 'lambda))
            (raise-syntax-error #f "lambda expected." src-stx #'lambda)]
           [(not (eq? (length (syntax->datum #'arg)) 1))
            (raise-syntax-error #f "arity of 1 expected." src-stx #'arg)]
           [else (begin (raise-if-not-identifier #'arg src-stx)
                        #'(void))])]
    [(... _) (raise-syntax-error #f "lambda expression expected." src-stx stx)]))

; define-monitoring creates a new Monitoring process.
; It requires the following inputs:
; 1) An identifier for the new process.
; 2) A boolean indicating whether the process is a proxy or not.
; 3) For processes that generate observed properties:
;    a) The time window for the process.
;    b) The value function for the process.
; 4) For processes that generate observed events:
;    a) The trigger (i.e. time window and predicate) for the start of the event.
;    b) The trigger for the end of the event.
; 5) The output type of the process.
(define-syntax (define-monitoring stx)
  (syntax-case stx ()
    [(define-monitoring id proxy (... duration) (... lambda) output-type)
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
                                               (made-process-control-state self)))))]))]))