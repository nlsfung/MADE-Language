#lang rosette/safe

(require (only-in rosette
                  symbol?
                  syntax->datum
                  eval-syntax
                  check-duplicates
                  raise-argument-error
                  for/list
                  in-list))

(require (for-syntax "./SyntaxUtil.rkt"))
(require "./VerifySyntax.rkt")
(require "./NomEnumSyntax.rkt")

(require "../rim/BasicDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")
(require "../rim/TemporalDataTypes.rkt")

(provide define-measurement
         define-observation
         define-abstraction
         define-action-instruction
         define-control-instruction
         define-action-plan)

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
               (case-lambda
                 [()
                  (id (get-proxy) (get-datetime) (get-dimensioned units))]
                 [(start-dt end-dt)
                  (id (get-proxy) (get-datetime start-dt end-dt) (get-dimensioned units))]))
             (verify-getter get-id id))))]))

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
     (begin
       (raise-if-not-identifier #'id stx)
       (with-syntax ([get-id (build-getter-name #'id)])
         #'(begin
             (struct id observed-event ()
               #:transparent
               #:methods gen:typed
               [(define/generic super-valid? valid?)
                (define (get-type self) id)
                (define (valid? self)
                  (super-valid? (observed-event
                                 (made-data-proxy-flag self)
                                 (observed-event-valid-datetime-range self)
                                 (observed-event-value self))))])
             (define get-id               
               (lambda () (id (get-proxy)
                              (datetime-range (get-datetime) (get-datetime))
                              (get-bool))))
             (verify-getter get-id id))))]

    [(define-observation id type)
     (cond [(eq? (syntax->datum #'type) 'dimensioned)
            (raise-syntax-error #f "units missing." stx #'type)]
           [else #'(define-observation id type (lambda (d) #t))])]

    [(define-observation id dimensioned units)
     (eq? 'dimensioned (syntax->datum #'dimensioned))
     #'(define-observation id dimensioned units (lambda (d) #t))]
     
    [(define-observation id type (... invariant))
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-identifier #'type stx)
       (raise-if-not-lambda #'invariant 1 stx)
       (with-syntax ([get-id (build-getter-name #'id)]
                     [get-val (build-getter-name #'type)])
         #'(begin
             (struct id observed-property ()
               #:transparent
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
                       (invariant (observed-property-value self))))])
             (define get-id
               (lambda () (id (get-proxy) (get-datetime) (get-val))))
             (verify-getter get-id id))))]

    [(define-observation id dimensioned units (... invariant))
     (eq? 'dimensioned (syntax->datum #'dimensioned))
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-symbol #'units stx)
       (raise-if-not-lambda #'invariant 1 stx)
       (with-syntax ([get-id (build-getter-name #'id)])
         #'(begin
             (struct id observed-property ()
               #:transparent
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
                       (invariant (observed-property-value self))))])
             (define get-id
               (lambda () (id (get-proxy) (get-datetime) (get-dimensioned units))))
             (verify-getter get-id id))))]))

; define-abstraction creates a new type of abstraction. 
; It requires the following inputs:
; 1) An identifier for the new abstraction datatype.
; 2) A struct constructor specifying the type of the abstraction value.
; 3) A symbol indicating the units if the abstraction contains a dimensioned value.
; 4) An optional invariant on the abstraction value.
(define-syntax (define-abstraction stx)
  (syntax-case stx ()
    [(define-abstraction id type)
     (cond [(eq? (syntax->datum #'type) 'dimensioned)
            (raise-syntax-error #f "units missing." stx #'type)]
           [else #'(define-abstraction id type (lambda (d) #t))])]

    [(define-abstraction id dimensioned units)
     (eq? 'dimensioned (syntax->datum #'dimensioned))
     #'(define-abstraction id dimensioned units (lambda (d) #t))]     

    [(define-abstraction id type (... invariant))
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-identifier #'type stx)
       (raise-if-not-lambda #'invariant 1 stx)
       (with-syntax ([get-id (build-getter-name #'id)]
                     [get-val (build-getter-name #'type)])
       #'(begin
           (struct id abstraction ()
             #:transparent
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
                     (invariant (abstraction-value self))))])
           (define get-id
             (lambda () (id (get-proxy)
                            (datetime-range (get-datetime) (get-datetime))
                            (get-val))))
           (verify-getter get-id id))))]

    [(define-abstraction id dimensioned units (... invariant))
     (eq? 'dimensioned (syntax->datum #'dimensioned))
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-symbol #'units stx)
       (raise-if-not-lambda #'invariant 1 stx)
       (with-syntax ([get-id (build-getter-name #'id)])
         #'(begin
             (struct id abstraction ()
               #:transparent
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
                       (invariant (abstraction-value self))))])
             (define get-id
               (lambda () (id (get-proxy)
                              (datetime-range (get-datetime) (get-datetime))
                              (get-dimensioned units))))
             (verify-getter get-id id))))]))

; define-action-plan creates a new type of action plan. 
; It requires the following inputs:
; 1) An identifier for the new action plan datatype.
; 2) A list of struct constructors specifying the accepted targets.
;    The list is divided into three categories:
;    a) control for scheduled control instructions.
;    b) homogeneous-action for scheduled homogeneous action instructions.
;    c) culminating-action for scheduled culminating action instructions.
(define-syntax (define-action-plan stx)
  ; Helper function for extracting the IDs for the input type of scheduled instructions.
  (define (extract-instructions type stx)
    (syntax-case stx ()
      [(type-id id ...)
       (eq? (syntax->datum #'type-id) type)
       (begin
         (raise-if-not-symbol #'(id ...) stx)
         #'(id ...))]
      [((type-id id ...) rest ...)
       (cond [(eq? (syntax->datum #'type-id) type)
              (extract-instructions type #'(type-id id ...))]
             [else (extract-instructions type #'(rest ...))])]
      [() #'()]))
  
  ; Helper function for creating a syntax object for a list of input IDs.
  (define (add-list-prefix stx)
    (syntax-case stx ()
      [(id ...) #'(list id ...)]))

  ; Helper function for creating a list of "get-x" syntax objects.
  (define (build-getter-name-list stx)
    (syntax-case stx ()
      [(id) (list (syntax->datum (build-getter-name #'id)))]
      [(id-1 id-2 ...) (append (build-getter-name-list #'(id-1))
                               (build-getter-name-list #'(id-2 ...)))]
      [() null]))
 
  (syntax-case stx ()
    [(define-action-plan id target ...)
     (let* ([control-ids (extract-instructions 'control #'(target ...))]
            [homogeneous-ids (extract-instructions 'homogeneous-action #'(target ...))]
            [culminating-ids (extract-instructions 'culminating-action #'(target ...))])
       (raise-if-not-identifier #'id stx)
       (with-syntax ([get-id (build-getter-name #'id)]
                     [control-list (add-list-prefix control-ids)]
                     [homogeneous-list (add-list-prefix homogeneous-ids)]
                     [culminating-list (add-list-prefix culminating-ids)]
                     [get-homogeneous-list (add-list-prefix
                                            (datum->syntax
                                             stx
                                             (build-getter-name-list
                                              (symbol->identifier homogeneous-ids))
                                             stx))]
                     [get-culminating-list (add-list-prefix
                                            (datum->syntax
                                             stx
                                             (build-getter-name-list
                                              (symbol->identifier culminating-ids))
                                             stx))])
         #'(begin
             (struct id action-plan ()
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
                                    (cond [(scheduled-control? i)
                                           (member (scheduled-control-target-process i)
                                                   control-list)]
                                          [(scheduled-homogeneous-action? i)
                                           (member (scheduled-homogeneous-action-action-type i)
                                                   homogeneous-list)]
                                          [(scheduled-culminating-action? i)
                                           (member (scheduled-culminating-action-action-type i)
                                                   culminating-list)]))
                                  (action-plan-instruction-set self))
                                 #f))))])
             (define get-id
               (lambda ([pat-length 2]
                        [units-list (list 'units)])
                 (id (get-proxy)
                     (get-datetime)
                     (append (map (lambda (i)
                                    (scheduled-control
                                     i (get-schedule pat-length) (get-status)))
                                  control-list)
                             (for/list ([inst-type (in-list homogeneous-list)]
                                        [get-inst (in-list get-homogeneous-list)])
                               (let ([inst (get-inst)])
                                 (scheduled-homogeneous-action
                                  inst-type
                                  (get-schedule pat-length)
                                  (homogeneous-action-rate inst)
                                  (homogeneous-action-duration inst))))
                             (for/list ([inst-type (in-list culminating-list)]
                                        [get-inst (in-list get-culminating-list)])
                               (let ([inst (get-inst)])
                                 (scheduled-culminating-action
                                  inst-type
                                  (get-schedule pat-length)
                                  (culminating-action-goal-state inst))))))))
             (verify-getter get-id id))))]))

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
     (cond [(eq? (syntax->datum #'action-type) 'homogeneous)
            #'(define-homogeneous-action id rest ...)]
           [(eq? (syntax->datum #'action-type) 'culminating)
            #'(define-culminating-action id rest ...)]
           [else (raise-syntax-error #f "'homogeneous or 'culminating expected."
                                     stx #'action-type)])]))

; Helper function for creating new homogeneous action instruction types.
(define-syntax (define-homogeneous-action stx)
  (syntax-case stx ()
    [(_ id units)
     #'(define-homogeneous-action id units (lambda (d1 d2) #t))]

    [(_ id units (... invariant))
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-symbol #'units stx)
       (raise-if-not-lambda #'invariant 2 stx)
       (with-syntax ([get-id (build-getter-name #'id)])
         #'(begin
             (struct id homogeneous-action ()
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
                                        (homogeneous-action-duration self))))])
             (define get-id
               (lambda () (id (get-proxy)
                              (get-datetime)
                              (get-dimensioned units)
                              (get-duration))))
             (verify-getter get-id id))))]))

; Helper function for creating new culminating action instruction types.
(define-syntax (define-culminating-action stx)
  (syntax-case stx ()
    [(_ id type)
     (cond [(eq? (syntax->datum #'type) 'dimensioned)
            (raise-syntax-error #f "units missing." stx #'type)]
           [else #'(define-culminating-action id type (lambda (d) #t))])]

    [(_ id dimensioned units)
     (eq? (syntax->datum #'dimensioned) 'dimensioned)
     #'(define-culminating-action id dimensioned units (lambda (d) #t))]

    [(_ id type (... invariant))
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-identifier #'type stx)
       (raise-if-not-lambda #'invariant 1 stx)
       (with-syntax ([get-id (build-getter-name #'id)]
                     [get-val (build-getter-name #'type)])
         #'(begin
             (struct id culminating-action ()
               #:transparent
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
                       (invariant (culminating-action-goal-state self))))])
             (define get-id
               (lambda () (id (get-proxy) (get-datetime) (get-val))))
             (verify-getter get-id id))))]

    [(_ id dimensioned units (... invariant))
     (eq? 'dimensioned (syntax->datum #'dimensioned))
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-symbol #'units stx)
       (raise-if-not-lambda #'invariant 1 stx)
       (with-syntax ([get-id (build-getter-name #'id)])
         #'(begin
             (struct id culminating-action ()
               #:transparent
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
                       (invariant (culminating-action-goal-state self))))])
             (define get-id
               (lambda () (id (get-proxy) (get-datetime) (get-dimensioned units))))
             (verify-getter get-id id))))]))

; define-control-instruction creates a new type of control instruction. 
; It requires the following inputs:
; 1) An identifier for the new control instruction datatype.
; 2) An list of struct constructors specifying the accepted target processes.
(define (get-target-val) (define-symbolic* target-val integer?) target-val)
(define (get-void-sched) (define-symbolic* void-sched boolean?) void-sched)
(define (get-void-stat) (define-symbolic* void-stat boolean?) void-stat)
(define-syntax (define-control-instruction stx)
  (syntax-case stx ()
    [(define-control-instruction id target ...)
     (begin
       (raise-if-not-identifier #'id stx)
       (raise-if-not-identifier #'(target ...) stx)
       (with-syntax ([get-id (build-getter-name #'id)])
         #'(begin
             (struct id control-instruction ()
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
                                                     (list target ...))
                                             #f))))])
             (define get-id
               (lambda ([pat-length 2])
                 (id (get-proxy)
                     (list-ref (list target ...) (get-target-val))
                     (get-datetime)
                     (if (get-void-sched) (void) (get-schedule pat-length))
                     (if (get-void-stat) (void) (get-status)))))
             (verify-getter get-id id))))]))
