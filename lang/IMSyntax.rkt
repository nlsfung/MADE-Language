#lang rosette/safe

(require (only-in rosette symbol? syntax->datum eval-syntax check-duplicates raise-argument-error))

(require (for-syntax "./SyntaxUtil.rkt"))
(require "./VerifySyntax.rkt")

(require "../rim/BasicDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")
(require "../rim/TemporalDataTypes.rkt")

; This file contains the syntax for specifying new MADE information models.

; define-measurement creates a new type of measurement.
; It requires three inputs:
; 1) An identifier for the new measurement datatype.
; 2) A symbol specifying the units of measurement.
; 3) An optional invariant on the measurement value.
(define-syntax (define-measurement stx)
  (syntax-case stx ()
    [(define-measurement id units)
     #'(define-measurement id units (lambda (d) #t))]

    [(define-measurement id units (... invariant))
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-symbol #'units stx)
       (raise-if-not-lambda #'invariant 1 stx)
       (with-syntax ([get-id (build-getter-name #'id)])
         #'(begin             
             (struct id measurement ()
               #:transparent
               #:methods gen:typed
               [(define/generic super-valid? valid?)
                (define (get-type self) id)
                (define (valid? self)
                  (and (super-valid? (measurement (made-data-proxy-flag self)
                                                  (measurement-valid-datetime self)
                                                  (measurement-value self)))
                       (eq? (dimensioned-units (measurement-value self)) units)
                       (invariant (measurement-value self))))])
             (define get-id
               (lambda () (id (get-proxy) (get-datetime) (get-dimensioned units)))))))]))

; define-observation creates a new type of observed property or observed event.
; It requires the following inputs:
; 1) An identifier for the new observation datatype.
; 2) A struct constructor specifying the type of the observation value.
;    The symbol 'event indicates that the data type is an observed event.
; 3) A symbol indicating the units if the observation contains a dimensioned value.
; 4) An optional invariant on the observation value (for observed properties).
(define-syntax (define-observation stx)
  (syntax-case stx ()
    [(define-observation id 'event)
     (cond [(not (identifier? #'id))
            (raise-syntax-error #f "identifier expected." stx #'id)]
           [else #'(struct id observed-event ()
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define (get-type self) id)
                      (define (valid? self)
                        (super-valid? (observed-event
                                       (made-data-proxy-flag self)
                                       (observed-event-valid-datetime-range self)
                                       (observed-event-value self))))])])]

    [(define-observation id type)
     (cond [(not (identifier? #'id))
            (raise-syntax-error #f "identifier expected." stx #'id)]
           [(and (not (identifier? #'type))
                 (not (procedure? (eval-syntax #'type))))
            (raise-syntax-error #f "datatype constructor expected." stx #'type)]
           [(eq? 'dimensioned (syntax->datum #'type))
            (raise-syntax-error #f "units missing." stx #'type)]
           [else #'(struct id observed-property ()
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define/generic super-get-type get-type)
                      (define (get-type self) id)
                      (define (valid? self)
                        (and (super-valid? (observed-property
                                            (made-data-proxy-flag self)
                                            (observed-property-valid-datetime self)
                                            (observed-property-value self)))
                             (eq? (super-get-type (observed-property-value self)) type)))])])]

    [(define-observation id dimensioned units)
     (eq? 'dimensioned (syntax->datum #'dimensioned))
     (cond [(not (identifier? #'id))
            (raise-syntax-error #f "identifier expected." stx #'id)]
           [(not (symbol? (eval-syntax #'units)))
            (raise-syntax-error #f "symbol expected." stx #'units)]           
           [else #'(struct id observed-property ()
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define (get-type self) id)
                      (define (valid? self)
                        (and (super-valid? (observed-property
                                            (made-data-proxy-flag self)
                                            (observed-property-valid-datetime self)
                                            (observed-property-value self)))
                             (dimensioned? (observed-property-value self))
                             (eq? units (dimensioned-units (observed-property-value self)))))])])]

    [(define-observation id type invariant)
     (cond [(not (identifier? #'id))
            (raise-syntax-error #f "identifier expected." stx #'id)]
           [(and (not (identifier? #'type))
                 (not (procedure? (eval-syntax #'type))))
            (raise-syntax-error #f "datatype constructor expected." stx #'type)]
           [(and (not (identifier? #'invariant))
                 (not (and (procedure? (eval-syntax #'invariant))
                           (eq? (procedure-arity (eval-syntax #'invariant)) 1))))
            (raise-syntax-error #f "invariant on property value expected." stx #'invariant)]
           [else #'(struct id observed-property ()
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define/generic super-get-type get-type)
                      (define (get-type self) id)
                      (define (valid? self)
                        (and (super-valid? (observed-property
                                            (made-data-proxy-flag self)
                                            (observed-property-valid-datetime self)
                                            (observed-property-value self)))
                             (eq? (super-get-type (observed-property-value self)) type)
                             (invariant (observed-property-value self))))])])]

    [(define-observation id dimensioned units invariant)
     (eq? 'dimensioned (syntax->datum #'dimensioned))
     (cond [(not (identifier? #'id))
            (raise-syntax-error #f "identifier expected." stx #'id)]
           [(not (symbol? (eval-syntax #'units)))
            (raise-syntax-error #f "symbol expected." stx #'units)]
           [(and (not (identifier? #'invariant))
                 (not (and (procedure? (eval-syntax #'invariant))
                           (eq? (procedure-arity (eval-syntax #'invariant)) 1))))
            (raise-syntax-error #f "invariant on property value expected." stx #'invariant)]
           [else #'(struct id observed-property ()
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define (get-type self) id)
                      (define (valid? self)
                        (and (super-valid? (observed-property
                                            (made-data-proxy-flag self)
                                            (observed-property-valid-datetime self)
                                            (observed-property-value self)))
                             (dimensioned? (observed-property-value self))
                             (eq? units (dimensioned-units (observed-property-value self)))
                             (invariant (observed-property-value self))))])])]))

; define-abstraction creates a new type of abstraction. 
; It requires the following inputs:
; 1) An identifier for the new abstraction datatype.
; 2) A struct constructor specifying the type of the abstraction value.
; 3) A symbol indicating the units if the abstraction contains a dimensioned value.
; 4) An optional invariant on the abstraction value.
(define-syntax (define-abstraction stx)
  (syntax-case stx ()
    [(define-abstraction id type)
     (cond [(not (identifier? #'id))
            (raise-syntax-error #f "identifier expected." stx #'id)]
           [(and (not (identifier? #'type))
                 (not (procedure? (eval-syntax #'type))))
            (raise-syntax-error #f "datatype constructor expected." stx #'type)]
           [(eq? 'dimensioned (syntax->datum #'type))
            (raise-syntax-error #f "units missing." stx #'type)]
           [else #'(struct id abstraction ()
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define/generic super-get-type get-type)
                      (define (get-type self) id)
                      (define (valid? self)
                        (and (super-valid? (abstraction
                                            (made-data-proxy-flag self)
                                            (abstraction-valid-datetime-range self)
                                            (abstraction-value self)))
                             (eq? (super-get-type (abstraction-value self)) type)))])])]

    [(define-abstraction id dimensioned units)
     (eq? 'dimensioned (syntax->datum #'dimensioned))
     (cond [(not (identifier? #'id))
            (raise-syntax-error #f "identifier expected." stx #'id)]
           [(not (symbol? (eval-syntax #'units)))
            (raise-syntax-error #f "symbol expected." stx #'units)]           
           [else #'(struct id abstraction ()
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define (get-type self) id)
                      (define (valid? self)
                        (and (super-valid? (abstraction
                                            (made-data-proxy-flag self)
                                            (abstraction-valid-datetime-range self)
                                            (abstraction-value self)))
                             (dimensioned? (abstraction-value self))
                             (eq? units (dimensioned-units (abstraction-value self)))))])])]

    [(define-abstraction id type invariant)
     (cond [(not (identifier? #'id))
            (raise-syntax-error #f "identifier expected." stx #'id)]
           [(and (not (identifier? #'type))
                 (not (procedure? (eval-syntax #'type))))
            (raise-syntax-error #f "datatype constructor expected." stx #'type)]
           [(and (not (identifier? #'invariant))
                 (not (and (procedure? (eval-syntax #'invariant))
                           (eq? (procedure-arity (eval-syntax #'invariant)) 1))))
            (raise-syntax-error #f "invariant on property value expected." stx #'invariant)]
           [else #'(struct id abstraction ()
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define/generic super-get-type get-type)
                      (define (get-type self) id)
                      (define (valid? self)
                        (and (super-valid? (abstraction
                                            (made-data-proxy-flag self)
                                            (abstraction-valid-datetime-range self)
                                            (abstraction-value self)))
                             (eq? (super-get-type (abstraction-value self)) type)
                             (invariant (abstraction-value self))))])])]

    [(define-abstraction id dimensioned units invariant)
     (eq? 'dimensioned (syntax->datum #'dimensioned))
     (cond [(not (identifier? #'id))
            (raise-syntax-error #f "identifier expected." stx #'id)]
           [(not (symbol? (eval-syntax #'units)))
            (raise-syntax-error #f "symbol expected." stx #'units)]
           [(and (not (identifier? #'invariant))
                 (not (and (procedure? (eval-syntax #'invariant))
                           (eq? (procedure-arity (eval-syntax #'invariant)) 1))))
            (raise-syntax-error #f "invariant on property value expected." stx #'invariant)]
           [else #'(struct id observed-property ()
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define (get-type self) id)
                      (define (valid? self)
                        (and (super-valid? (abstraction
                                            (made-data-proxy-flag self)
                                            (abstraction-valid-datetime-range self)
                                            (abstraction-value self)))
                             (dimensioned? (abstraction-value self))
                             (eq? units (dimensioned-units (abstraction-value self)))
                             (invariant (observed-property-value self))))])])]))

; define-action-plan creates a new type of action plan. 
; It requires the following inputs:
; 1) An identifier for the new action plan datatype.
; 2) An optional list of struct constructors specifying the accepted targets.
(define-syntax (define-action-plan stx)
  (syntax-case stx ()
    [(define-action-plan id)
     (cond [(not (identifier? #'id))
            (raise-syntax-error #f "identifier expected." stx #'id)]
           [else #'(struct id action-plan ()
                     #:transparent
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define (get-type self) id)
                      (define (valid? self)
                        (super-valid? (action-plan
                                       (made-data-proxy-flag self)
                                       (action-plan-valid-datetime self)
                                       (action-plan-instruction-set self))))])])]

    [(define-action-plan id target-list)
     (cond [(not (identifier? #'id))
            (raise-syntax-error #f "identifier expected." stx #'id)]
           [(not (list? (syntax->datum #'target-list)))
            (raise-syntax-error #f "list of identifiers expected." stx #'target-list)]
           [else (let* ([bad-target (find-invalid-target #'target-list)])
                   (if bad-target
                       (raise-syntax-error #f "identifier expected." stx bad-target)
                       #'(struct id action-plan ()
                           #:transparent
                           #:methods gen:typed
                           [(define/generic super-valid? valid?)
                            (define (get-type self) id)
                            (define (valid? self)
                              (and (super-valid? (action-plan
                                                  (made-data-proxy-flag self)
                                                  (action-plan-valid-datetime self)
                                                  (action-plan-instruction-set self)))
                                   (not (eq? (andmap
                                              (lambda (i)
                                                (member (cond [(scheduled-control? i)
                                                               (scheduled-control-target-process i)]
                                                              [(scheduled-homogeneous-action? i)
                                                               (scheduled-homogeneous-action-action-type i)]
                                                              [(scheduled-culminating-action? i)
                                                               (scheduled-culminating-action-action-type i)])
                                                        target-list))
                                              (action-plan-instruction-set self))
                                             #f))))])))])]))

; Helper function to check that the target list contains identifiers only.
; Returns the first invalid input found or #f if all inputs are identifiers.
(define-for-syntax (find-invalid-target stx)
  (syntax-case stx ()
    [(id)
     (if (not (identifier? #'id)) #'id #f)]
    [(id-1 id-2 ...)
     (if (not (identifier? #'id-1))
         #'id-1
         (find-invalid-target #'(id-2 ...)))]))

; define-action-instruction creates a new type of action instruction. 
; It requires the following inputs:
; 1) An identifier for the new action instruction datatype.
; 2) A symbol indicating whether the action is homogeneous or culminating.
; 3) For homogeneous action instructions:
;    a) A symbol indicating the units for the rate.
;    b) An optional invariant on the action rate and duration.
; 4) For culminating action instructions:
;    a) A struct constructor specifying the type of the goal state.
;    b) A symbol indicating the units if the goal state is dimensioned.
;    c) An optional invariant on the goal state.
(define-syntax (define-action-instruction stx)
  (syntax-case stx ()
    [(define-action-instruction id action-type rest ...)
     (cond [(not (identifier? #'id))
            (raise-syntax-error #f "identifier expected." stx #'id)]
           [(eq? 'homogeneous (eval-syntax #'action-type))
            (define-homogeneous-action #'(id rest ...) stx)]
           [(eq? 'culminating (eval-syntax #'action-type))
            (define-culminating-action #'(id rest ...) stx)]
           [else (raise-syntax-error #f "'homogeneous or 'culminating expected."
                                     stx #'action-type)])]))    

; Helper function for creating new homogeneous action instruction types.
(define-for-syntax (define-homogeneous-action stx src-stx)
  (syntax-case stx ()
    [(id units)
     (cond [(not (symbol? (eval-syntax #'units)))
            (raise-syntax-error #f "symbol expected." src-stx #'units)]
           [else #'(struct id homogeneous-action ()
                     #:transparent
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define (get-type self) id)
                      (define (valid? self)
                        (and (super-valid? (homogeneous-action
                                            (made-data-proxy-flag self)
                                            (homogeneous-action-start-datetime self)
                                            (homogeneous-action-rate self)
                                            (homogeneous-action-duration self)))
                             (eq? (dimensioned-units (homogeneous-action-rate self)) units)))])])]

    [(id units invariant)
     (cond [(not (symbol? (eval-syntax #'units)))
            (raise-syntax-error #f "symbol expected." src-stx #'units)]
           [(and (not (identifier? #'invariant))
                 (not (and (procedure? (eval-syntax #'invariant))
                           (eq? (procedure-arity (eval-syntax #'invariant)) 2))))
            (raise-syntax-error #f "invariant on action rate and duration expected."
                                src-stx #'invariant)]
           [else #'(struct id homogeneous-action ()
                     #:transparent
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define (get-type self) id)
                      (define (valid? self)
                        (and (super-valid? (homogeneous-action
                                            (made-data-proxy-flag self)
                                            (homogeneous-action-start-datetime self)
                                            (homogeneous-action-rate self)
                                            (homogeneous-action-duration self)))
                             (eq? (dimensioned-units (homogeneous-action-rate self)) units)
                             (invariant (homogeneous-action-rate self)
                                        (homogeneous-action-duration self))))])])]))

; Helper function for creating new culminating action instruction types.
(define-for-syntax (define-culminating-action stx src-stx)
  (syntax-case stx ()
    [(id type)
     (cond [(and (not (identifier? #'type))
                 (not (procedure? (eval-syntax #'type))))
            (raise-syntax-error #f "datatype constructor expected." src-stx #'type)]
           [(eq? 'dimensioned (syntax->datum #'type))
            (raise-syntax-error #f "units missing." src-stx #'type)]
           [else #'(struct id culminating-action ()
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define/generic super-get-type get-type)
                      (define (get-type self) id)
                      (define (valid? self)
                        (and (super-valid? (culminating-action
                                            (made-data-proxy-flag self)
                                            (culminating-action-start-datetime self)
                                            (culminating-action-goal-state self)))
                             (eq? (super-get-type (culminating-action-goal-state self)) type)))])])]

    [(id dimensioned units)
     (eq? 'dimensioned (syntax->datum #'dimensioned))
     (cond [(not (symbol? (eval-syntax #'units)))
            (raise-syntax-error #f "symbol expected." src-stx #'units)]           
           [else #'(struct id culminating-action ()
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define (get-type self) id)
                      (define (valid? self)
                        (and (super-valid? (culminating-action
                                            (made-data-proxy-flag self)
                                            (culminating-action-start-datetime self)
                                            (culminating-action-goal-state self)))
                             (dimensioned? (culminating-action-goal-state self))
                             (eq? units (dimensioned-units (culminating-action-goal-state self)))))])])]

    [(id type invariant)
     (cond [(and (not (identifier? #'type))
                 (not (procedure? (eval-syntax #'type))))
            (raise-syntax-error #f "datatype constructor expected." src-stx #'type)]
           [(and (not (identifier? #'invariant))
                 (not (and (procedure? (eval-syntax #'invariant))
                           (eq? (procedure-arity (eval-syntax #'invariant)) 1))))
            (raise-syntax-error #f "invariant on goal state expected." src-stx #'invariant)]
           [else  #'(struct id culminating-action ()
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define/generic super-get-type get-type)
                      (define (get-type self) id)
                      (define (valid? self)
                        (and (super-valid? (culminating-action
                                            (made-data-proxy-flag self)
                                            (culminating-action-start-datetime self)
                                            (culminating-action-goal-state self)))
                             (eq? (super-get-type (culminating-action-goal-state self)) type)
                             (invariant (culminating-action-goal-state self))))])])]

    [(id dimensioned units invariant)
     (eq? 'dimensioned (syntax->datum #'dimensioned))
     (cond [(not (symbol? (eval-syntax #'units)))
            (raise-syntax-error #f "symbol expected." src-stx #'units)]
           [(and (not (identifier? #'invariant))
                 (not (and (procedure? (eval-syntax #'invariant))
                           (eq? (procedure-arity (eval-syntax #'invariant)) 1))))
            (raise-syntax-error #f "invariant on goal state expected." src-stx #'invariant)]
           [else #'(struct id culminating-action ()
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define (get-type self) id)
                      (define (valid? self)
                        (and (super-valid? (culminating-action
                                            (made-data-proxy-flag self)
                                            (culminating-action-start-datetime self)
                                            (culminating-action-goal-state self)))
                             (dimensioned? (culminating-action-goal-state self))
                             (eq? units (dimensioned-units (culminating-action-goal-state self)))
                             (invariant (culminating-action-goal-state self))))])])]))

; define-control-instruction creates a new type of control instruction. 
; It requires the following inputs:
; 1) An identifier for the new control instruction datatype.
; 2) An optional list of struct constructors specifying the accepted target processes.
(define-syntax (define-control-instruction stx)
  (syntax-case stx ()
    [(define-control-instruction id)
     (cond [(not (identifier? #'id))
            (raise-syntax-error #f "identifier expected." stx #'id)]
           [else #'(struct id control-instruction ()
                     #:transparent
                     #:methods gen:typed
                     [(define/generic super-valid? valid?)
                      (define (get-type self) id)
                      (define (valid? self)
                        (super-valid? (control-instruction
                                       (made-data-proxy-flag self)
                                       (control-instruction-target-process self)
                                       (control-instruction-valid-datetime self)
                                       (control-instruction-schedule self)
                                       (control-instruction-status self))))])])]

    [(define-control-instruction id target-list)
     (cond [(not (identifier? #'id))
            (raise-syntax-error #f "identifier expected." stx #'id)]
           [(not (list? (syntax->datum #'target-list)))
            (raise-syntax-error #f "list of identifiers expected." stx #'target-list)]
           [else (let* ([bad-target (find-invalid-target #'target-list)])
                   (if bad-target
                       (raise-syntax-error #f "identifier expected." stx bad-target)
                       #'(struct id control-instruction ()
                           #:transparent
                           #:methods gen:typed
                           [(define/generic super-valid? valid?)
                            (define (get-type self) id)
                            (define (valid? self)
                              (and (super-valid? (control-instruction
                                                  (made-data-proxy-flag self)
                                                  (control-instruction-target-process self)
                                                  (control-instruction-valid-datetime self)
                                                  (control-instruction-schedule self)
                                                  (control-instruction-status self)))
                                   (not (eq? (member (control-instruction-target-process self)
                                                     target-list)
                                             #f))))])))])]))
