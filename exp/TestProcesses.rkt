#lang rosette/safe

(require "../lang/IMSyntax.rkt"
         "../lang/PMSyntax.rkt"
         "../lang/VerifySyntax.rkt"
         "../rim/BasicDataTypes.rkt"
         "../rim/TemporalDataTypes.rkt"
         "../rim/MadeDataStructures.rkt"
         "../rpm/MadeProcess.rkt"
         "../rpm/MonitoringProcess.rkt"
         "../rpm/AnalysisProcess.rkt"
         "../rpm/DecisionProcess.rkt"
         "../rpm/EffectuationProcess.rkt")

(provide (all-defined-out))

; This file contains the tests performed to verify the
; implementation of generate-list and verify-process. 

; Specification of the relevant MADE archetypes.
(define-measurement body-speed 'ms-1)

(define-observation sprint-event #:event)

(define-abstraction exercise-abstraction nominal
  'insufficient 'excessive)

(define-action-plan exercise-plan
  (homogeneous-action 'sprint-action)
  (culminating-action 'endurance-running-action))

(define-action-instruction sprint-action
  #:homogeneous 'ms-1)

(define-action-instruction endurance-running-action
  #:culminating dimensioned 'm)

(define-action-instruction treadmill-output
  #:culminating dimensioned 'm)

; Specification of the processes (and associated procedures).
(define-syntax-rule (average dSet)
  (dimensioned
   (/ (foldl (lambda (d result)
               (+ (get-value (measurement-value d)) result))
                 0
                 dSet)
      5)
   'ms-1))

(define-monitoring #:event monitor-sprint #f sprint-event 
  (event-trigger 
    (duration 0 0 0 5) 
    (lambda (dSet) 
      (dim>=? (average (filter (lambda (d) (body-speed? d)) 
                               dSet)) 
              (dimensioned 3 'ms-1))))
  (event-trigger 
    (duration 0 0 0 5) 
    (lambda (dSet) 
      (dim<? (average (filter (lambda (d) (body-speed? d)) 
                              dSet)) 
             (dimensioned 2 'ms-1)))))

(define-syntax-rule (count-sprints dSet)
  (length (filter (lambda (d)
                    (and (sprint-event? d) 
                         (get-value (observed-event-value d))))
                  dSet)))

(define-analysis analyse-exercise #f exercise-abstraction
  ((duration 30 0 0 0)
   (lambda (dSet) (< (count-sprints dSet) 5))
   (lambda (dSet) 
     (exercise-abstraction-value-space 'insufficient)))
  ((duration 7 0 0 0)
   (lambda (dSet) (> (count-sprints dSet) 10))
   (lambda (dSet) 
     (exercise-abstraction-value-space 'excessive))))

(define-decision
  decide-exercise #f exercise-plan
  (#:instructions
   (homogeneous-action-template 
     'sprint-action 
     (relative-schedule #:rounding (duration 1 0 0 0)
                        #:pattern (duration 0 7 0 0)
                        #:interval (duration 14 0 0 0))
     (dimensioned 3 'ms-1)
     (duration 0 0 0 30))
   (culminating-action-template 
     'endurance-running-action
     (relative-schedule #:rounding (duration 1 0 0 0)
                        #:pattern (duration 7 17 0 0)
                        #:interval (duration 14 0 0 0))
     (dimensioned 5000 'm)))
  (#:criteria
    (lambda (dSet) 
      (findf 
        (lambda (d) 
          (and (exercise-abstraction? d) 
               (eq? (abstraction-value d) 
                    (exercise-abstraction-value-space 
                      'insufficient))))
        dSet))))

(define-effectuation effectuate-running #f treadmill-output 
  (target-schedule #:plan exercise-plan 
                   #:instruction 'endurance-running-action 
                   #:predicate (lambda (inst-set) #t)))

; Execution of verify-process and generate-list.
(verify-process  monitor-sprint
                 (generate-list body-speed
                                (datetime 2019 3 10 0 0 0)
                                (datetime 2019 3 10 0 0 59)
                                3)
                 (datetime 2019 3 10 0 1 0))

(verify-process  analyse-exercise
                 (generate-list sprint-event
                                (datetime 2020 3 17 0 0 0)
                                (datetime 2020 3 17 23 0 0)
                                (duration 0 2 0 0))
                 (datetime 2020 3 18 0 0 0))

(verify-process decide-exercise
                (generate-list exercise-abstraction
                               (datetime 2020 3 1 0 0 0)
                               (datetime 2020 3 15 23 0 0)
                               1)
                (datetime 2020 3 15 0 0 0))

(verify-process effectuate-running
                (generate-list exercise-plan
                               (datetime 2020 3 1 0 0 0)
                               (datetime 2020 3 15 23 0 0)
                               1)
                (datetime 2020 3 15 0 0 0))
