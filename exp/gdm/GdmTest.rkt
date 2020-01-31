#lang rosette/safe

(require "./GdmIM.rkt"
         "./GdmPM.rkt"
         "../../lang/VerifySyntax.rkt"
         "../../rim/BasicDataTypes.rkt"
         "../../rim/TemporalDataTypes.rkt"
         "../../rim/MadeDataStructures.rkt"
         "../../rpm/MadeProcess.rkt"
         "../../rpm/MadeNetwork.rkt"
         "../../rpm/MonitoringProcess.rkt"
         "../../rpm/AnalysisProcess.rkt"
         "../../rpm/DecisionProcess.rkt"
         "../../rpm/EffectuationProcess.rkt")

; This file contains the MADE networks associated with the GDM guideline.

; The bg-network comprises all the MADE processes involved in the bg workflow.
(define bg-network
  (made-network
   null
   (list (analyse-blood-glucose null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (analyse-urinary-ketone null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t)))
   (list (decide-bg-twice-weekly null (control-state (schedule (list (datetime 2020 1 1 8 0 0)) #t) #t))
         (decide-bg-daily null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #f) #f))
         (decide-bg-insulin null (control-state (schedule (list (datetime 2019 12 8 8 0 0)) #t) #t))
         (decide-bg-twice-weekly-post-insulin null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #f) #f))
         (decide-bg-daily-post-insulin null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #f) #f))
         (decide-bg-insulin-adjust null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #f) #f))
         (decide-bg-nutrition-change null (control-state (schedule (list (datetime 2019 12 8 8 0 0)) #t) #t))
         (decide-bg-twice-weekly-post-nutrition null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #f) #f))
         (decide-bg-daily-post-nutrition null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #f) #f))
         (decide-bg-insulin-post-nutrition null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #f) #f)))
   (list (effectuate-monitor-bg null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bg-nutrition-change null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bg-insulin-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bg-insulin-post-nutrition-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bg-insulin-adjust-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bg-twice-weekly-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bg-twice-weekly-post-nutrition-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bg-twice-weekly-post-insulin-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bg-daily-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bg-daily-post-nutrition-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bg-daily-post-insulin-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t)))))

; Test data stream for the blood glucose (bg) workflow.
(define bg-data-stream
  (list
   (stream-item
    (datetime 2019 12 1 7 0 0)
    (list
     (meal-event #f (datetime-range (datetime 2019 12 1 7 0 0) (datetime 2019 12 1 7 0 0)) (bool #t))
     (carbohydrate-intake #f (datetime 2019 12 1 7 0 0) (carbohydrate-intake-value-space '-/+))))
   (stream-item
    (datetime 2019 12 1 8 0 0)
    (list
     (blood-glucose #f (datetime 2019 12 1 8 0 0) (dimensioned 145 'mg/dL))))
   (stream-item
    (datetime 2019 12 2 7 0 0)
    (list
     (meal-event #f (datetime-range (datetime 2019 12 2 7 0 0) (datetime 2019 12 2 7 0 0)) (bool #t))
     (carbohydrate-intake #f (datetime 2019 12 2 7 0 0) (carbohydrate-intake-value-space '-/+))))
   (stream-item
    (datetime 2019 12 2 8 0 0)
    (list
     (blood-glucose #f (datetime 2019 12 2 8 0 0) (dimensioned 145 'mg/dL))))
   (stream-item (datetime 2019 12 3 8 0 0) '())
   (stream-item (datetime 2019 12 4 8 0 0) '())
   (stream-item (datetime 2019 12 5 8 0 0) '())
   (stream-item (datetime 2019 12 6 8 0 0) '())
   (stream-item (datetime 2019 12 7 8 0 0) '())
   (stream-item (datetime 2019 12 8 8 0 0) '())
   (stream-item (datetime 2019 12 9 8 0 0) '())
   (stream-item (datetime 2019 12 10 8 0 0) '())
   (stream-item
    (datetime 2019 12 11 7 0 0)
    (list
     (meal-event #f (datetime-range (datetime 2019 12 11 7 0 0) (datetime 2019 12 11 7 0 0)) (bool #t))
     (carbohydrate-intake #f (datetime 2019 12 11 7 0 0) (carbohydrate-intake-value-space '-/+))))
   (stream-item
    (datetime 2019 12 11 8 0 0)
    (list
     (blood-glucose #f (datetime 2019 12 11 8 0 0) (dimensioned 145 'mg/dL))))
   (stream-item
    (datetime 2019 12 12 7 0 0)
    (list
     (meal-event #f (datetime-range (datetime 2019 12 12 7 0 0) (datetime 2019 12 12 7 0 0)) (bool #t))
     (carbohydrate-intake #f (datetime 2019 12 12 7 0 0) (carbohydrate-intake-value-space '-/+))))
   (stream-item
    (datetime 2019 12 12 8 0 0)
    (list
     (blood-glucose #f (datetime 2019 12 12 8 0 0) (dimensioned 145 'mg/dL))))
   (stream-item (datetime 2019 12 13 8 0 0) '())
   (stream-item (datetime 2019 12 14 8 0 0) '())
   (stream-item (datetime 2019 12 15 8 0 0) '())
   (stream-item (datetime 2019 12 16 8 0 0) '())
   (stream-item (datetime 2019 12 17 8 0 0) '())
   (stream-item (datetime 2019 12 18 8 0 0) '())))
