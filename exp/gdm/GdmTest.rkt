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

; The bg-network comprises all the MADE processes involved in the blood glucose (bg) workflow.
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
         (effectuate-administer-insulin null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-change-diet null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
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

;; Test data stream for the blood glucose (bg) workflow.
;(define bg-data-stream
;  (list
;   (stream-item
;    (datetime 2019 12 1 7 0 0)
;    (list
;     (meal-event #f (datetime-range (datetime 2019 12 1 7 0 0) (datetime 2019 12 1 7 0 0)) (bool #t))
;     (carbohydrate-intake #f (datetime 2019 12 1 7 0 0) (carbohydrate-intake-value-space '-/+))))
;   (stream-item
;    (datetime 2019 12 1 8 0 0)
;    (list
;     (blood-glucose #f (datetime 2019 12 1 8 0 0) (dimensioned 145 'mg/dL))))
;   (stream-item
;    (datetime 2019 12 2 7 0 0)
;    (list
;     (meal-event #f (datetime-range (datetime 2019 12 2 7 0 0) (datetime 2019 12 2 7 0 0)) (bool #t))
;     (carbohydrate-intake #f (datetime 2019 12 2 7 0 0) (carbohydrate-intake-value-space '-/+))))
;   (stream-item
;    (datetime 2019 12 2 8 0 0)
;    (list
;     (blood-glucose #f (datetime 2019 12 2 8 0 0) (dimensioned 145 'mg/dL))))
;   (stream-item (datetime 2019 12 3 8 0 0) '())
;   (stream-item (datetime 2019 12 4 8 0 0) '())
;   (stream-item (datetime 2019 12 5 8 0 0) '())
;   (stream-item (datetime 2019 12 6 8 0 0) '())
;   (stream-item (datetime 2019 12 7 8 0 0) '())
;   (stream-item (datetime 2019 12 8 8 0 0) '())
;   (stream-item (datetime 2019 12 9 8 0 0) '())
;   (stream-item (datetime 2019 12 10 8 0 0) '())
;   (stream-item
;    (datetime 2019 12 11 7 0 0)
;    (list
;     (meal-event #f (datetime-range (datetime 2019 12 11 7 0 0) (datetime 2019 12 11 7 0 0)) (bool #t))
;     (carbohydrate-intake #f (datetime 2019 12 11 7 0 0) (carbohydrate-intake-value-space '-/+))))
;   (stream-item
;    (datetime 2019 12 11 8 0 0)
;    (list
;     (blood-glucose #f (datetime 2019 12 11 8 0 0) (dimensioned 145 'mg/dL))))
;   (stream-item
;    (datetime 2019 12 12 7 0 0)
;    (list
;     (meal-event #f (datetime-range (datetime 2019 12 12 7 0 0) (datetime 2019 12 12 7 0 0)) (bool #t))
;     (carbohydrate-intake #f (datetime 2019 12 12 7 0 0) (carbohydrate-intake-value-space '-/+))))
;   (stream-item
;    (datetime 2019 12 12 8 0 0)
;    (list
;     (blood-glucose #f (datetime 2019 12 12 8 0 0) (dimensioned 145 'mg/dL))))
;   (stream-item (datetime 2019 12 13 8 0 0) '())
;   (stream-item (datetime 2019 12 14 8 0 0) '())
;   (stream-item (datetime 2019 12 15 8 0 0) '())
;   (stream-item (datetime 2019 12 16 8 0 0) '())
;   (stream-item (datetime 2019 12 17 8 0 0) '())
;   (stream-item (datetime 2019 12 18 8 0 0) '())))

; The ktn-network comprises all the MADE processes involved in the ketonuria (ktn) workflow.
(define ktn-network
  (made-network
   null
   (list (analyse-urinary-ketone null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (analyse-carbohydrates-intake null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t)))
   (list (decide-uk-twice-weekly null (control-state (schedule (list (datetime 2019 12 14 0 0 0)) #t) #t))
         (decide-uk-daily null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #f) #f))
         (decide-uk-dinner-increase null (control-state (schedule (list (datetime 2019 12 7 0 0 0)) #t) #t))
         (decide-uk-twice-weekly-post-dinner null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #f) #f))
         (decide-uk-daily-post-dinner null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #f) #f)))
   (list (effectuate-change-dinner null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-monitor-uk-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-uk-dinner-increase null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-uk-twice-weekly-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-uk-twice-weekly-post-dinner-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-uk-daily-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-uk-daily-post-dinner-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t)))))

;; Test data for the ketonuria workflow.
;(define ktn-data-stream
;  (list
;   (stream-item
;    (datetime 2019 12 1 8 0 0)
;    (list
;     (urinary-ketone #f (datetime 2019 12 1 8 0 0) (urinary-ketone-value-space '+))
;     (carbohydrate-intake #f (datetime 2019 12 1 8 0 0) (carbohydrate-intake-value-space '-/+))))
;   (stream-item
;    (datetime 2019 12 2 8 0 0)
;    (list
;     (urinary-ketone #f (datetime 2019 12 2 8 0 0) (urinary-ketone-value-space '+))
;     (carbohydrate-intake #f (datetime 2019 12 2 8 0 0) (carbohydrate-intake-value-space '-/+))))
;   (stream-item
;    (datetime 2019 12 3 8 0 0)
;    (list
;     (urinary-ketone #f (datetime 2019 12 3 8 0 0) (urinary-ketone-value-space '+))
;     (carbohydrate-intake #f (datetime 2019 12 2 3 0 0) (carbohydrate-intake-value-space '-/+))))
;   (stream-item (datetime 2019 12 7 8 0 0) '())
;   (stream-item (datetime 2019 12 9 19 0 0) '())
;   (stream-item (datetime 2019 12 10 8 0 0) '())
;   (stream-item
;    (datetime 2019 12 20 8 0 0)
;    (list
;     (urinary-ketone #f (datetime 2019 12 20 8 0 0) (urinary-ketone-value-space '--))))
;   (stream-item (datetime 2019 12 21 8 0 0) '())
;   (stream-item (datetime 2019 12 22 8 0 0) '())
;   (stream-item (datetime 2019 12 23 8 0 0) '())))

; The bp-network comprises all the MADE processes involved in the blood pressure (bp) workflow.
(define bp-network
  (made-network
   null
   (list (analyse-blood-pressure null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t)))
   (list (decide-bp-once-weekly null (control-state (schedule (list (datetime 2019 12 14 0 0 0)) #t) #t))
         (decide-bp-twice-weekly null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #f) #f))
         (decide-bp-chronic null (control-state (schedule (list (datetime 2019 12 1 6 0 0)) #t) #t))
         (decide-bp-gestational null (control-state (schedule (list (datetime 2019 12 1 6 0 0)) #t) #t))
         (decide-bp-once-weekly-gestational null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #f) #f))
         (decide-bp-two-days-gestational null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #f) #f))
         (decide-bp-hourly-gestational null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #f) #f)))
   (list (effectuate-monitor-systolic-bp-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-monitor-diastolic-bp-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bp-once-weekly-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bp-twice-weekly-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bp-chronic-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bp-gestational-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bp-two-days-gestational-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bp-once-weekly-gestational-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t))
         (effectuate-bp-hourly-gestational-control null (control-state (schedule (list (datetime 1 1 1 0 0 0)) #t) #t)))))

;; Test data stream for the blood pressure (bp) workflow.
;(define bp-data-stream
;  (list
;   (stream-item
;    (datetime 2019 12 1 8 0 0)
;    (list
;     (systolic-blood-pressure #f (datetime 2019 12 1 8 0 0) (dimensioned 130 'mmHg))))
;   (stream-item
;    (datetime 2019 12 2 8 0 0)
;    (list
;     (diastolic-blood-pressure #f (datetime 2019 12 2 8 0 0) (dimensioned 85 'mmHg))))
;   (stream-item (datetime 2019 12 14 8 0 0) '())
;   (stream-item
;    (datetime 2019 12 15 8 0 0)
;    (list
;     (systolic-blood-pressure #f (datetime 2019 12 15 8 0 0) (dimensioned 145 'mmHg))))
;   (stream-item
;    (datetime 2019 12 15 13 0 0)
;    (list
;     (diastolic-blood-pressure #f (datetime 2019 12 15 13 0 0) (dimensioned 96 'mmHg))))
;   (stream-item (datetime 2019 12 15 14 0 0) '())))

