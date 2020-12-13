#lang rosette/safe

(require "../../lang/IMSyntax.rkt"
         "../../lang/PMSyntax.rkt"
         "../../lang/VerifySyntax.rkt"
         "../../rim/BasicDataTypes.rkt"
         "../../rim/TemporalDataTypes.rkt"
         "../../rim/MadeDataStructures.rkt"
         "../../rpm/MadeProcess.rkt"
         "../../rpm/AnalysisProcess.rkt"
         "../../rpm/DecisionProcess.rkt"
         "../../rpm/EffectuationProcess.rkt")

(provide (all-defined-out))

; This file contains the specification of example data and processes for
; the AIME2020 conference.

(define-measurement raw-heart-rate 'bpm)

(define-observation heart-rate dimensioned 'bpm)

(define-monitoring
  #:property
  monitor-heart-rate
  #f
  heart-rate
  (duration 0 0 0 5)
  (lambda (d-list)
    (let* ([hr-list (filter (lambda (d) (raw-heart-rate? d)) d-list)])
      (dim/ (foldl (lambda (datum result)
                     (dim+ (measurement-value datum) result 'bpm))
                   (dimensioned 0 'bpm)
                   hr-list)
            (* 1.0 (length hr-list))
            'bpm))))

(define process
  (monitor-heart-rate
   null
   (control-state (schedule (list (datetime 2020 8 27 17 20 0)) #t) #t)))

;(execute process hr-data (datetime 2020 8 27 17 20 20))

(define hr-data
  (list (raw-heart-rate #f (datetime 2020 8 27 17 20 0) (dimensioned 86 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 1) (dimensioned 71 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 2) (dimensioned 69 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 3) (dimensioned 77 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 4) (dimensioned 61 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 5) (dimensioned 88 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 6) (dimensioned 61 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 7) (dimensioned 88 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 8) (dimensioned 72 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 9) (dimensioned 85 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 10) (dimensioned 67 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 11) (dimensioned 69 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 12) (dimensioned 61 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 13) (dimensioned 75 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 14) (dimensioned 64 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 15) (dimensioned 68 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 16) (dimensioned 88 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 17) (dimensioned 87 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 18) (dimensioned 64 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 19) (dimensioned 68 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 20) (dimensioned 78 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 21) (dimensioned 77 'bpm))
        (raw-heart-rate #f (datetime 2020 8 27 17 20 22) (dimensioned 85 'bpm))))
