#lang rosette/safe

(require (rename-in rosette/safe [count rosette-count])
         "./GdmIM.rkt"
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

; This file contains the specification of the process model for the clinical
; guideline for gestational diabetes mellitus (GDM).

; No monitoring processes are specified by the guideline.

; analyse-blood-glucose (ABG) analyses blood glucose (BG) measurements to 
; determine the patient's degree of glycemic control, which can be:
; a) Good (BG levels are normal for a month)
; b) Poor (A single abnormal BG value)
; c) Meal-compliance poor (>= 2 abnormal BG values in a week but diet compliant).
; d) Meal-incompliance poor (>= 2 abnormal BG values due to diet incompliance).
; e) Non-related poor (>= 2 abnormal BG values at different intervals)
; f) Very poor (>= 2 abnormal BG values exeeding a given threshold).
(define-analysis analyse-blood-glucose #f glycemic-control
  ((duration 31 0 0 0)
   (lambda (d-list)
     (let* ([bg-list (filter (lambda (d) (blood-glucose? d)) d-list)]
            [meal-list (filter (lambda (d) (meal-event? d)) d-list)])
       (andmap
        (lambda (bg)
          (not (abnormal-bg? (post-prandial-bg? bg meal-list) bg)))
        bg-list)))
   (lambda (d-list) (glycemic-control-value-space 'good)))
  ((duration 7 0 0 0)
   (lambda (d-list)
     (let* ([meal-list (filter (lambda (d) (meal-event? d)) d-list)]
            [carb-list (filter (lambda (d) (carbohydrate-intake? d)) d-list)]
            [very-abnormal-bg-list
             (filter (lambda (d) (and (blood-glucose? d)
                                      (very-abnormal-bg? (post-prandial-bg? d meal-list) d)))
                     d-list)])
       (>= (length very-abnormal-bg-list) 2)))
   (lambda (d-list) (glycemic-control-value-space 'very-poor)))
  ((duration 7 0 0 0)
   (lambda (d-list)
     (let* ([meal-list (filter (lambda (d) (meal-event? d)) d-list)]
            [carb-list (filter (lambda (d) (carbohydrate-intake? d)) d-list)]
            [abnormal-bg-list
             (filter (lambda (d) (and (blood-glucose? d)
                                      (abnormal-bg? (post-prandial-bg? d meal-list) d)))
                     d-list)]
            [abnormal-bg-dt
             (remove-duplicates
              (map (lambda (d) (datetime-hour (observed-property-valid-datetime d)))
                   abnormal-bg-list))])
       (>= (length abnormal-bg-dt) 2)))
   (lambda (d-list) (glycemic-control-value-space 'non-related-poor)))
  ((duration 7 0 0 0)
   (lambda (d-list)
     (let* ([meal-list (filter (lambda (d) (meal-event? d)) d-list)]
            [carb-list (filter (lambda (d) (carbohydrate-intake? d)) d-list)]
            [abnormal-bg-list
             (filter (lambda (d) (and (blood-glucose? d)
                                      (abnormal-bg? (post-prandial-bg? d meal-list) d)))
                     d-list)])
       (and (>= (length abnormal-bg-list) 2)
            (andmap (lambda (bg) (compliant-bg? bg carb-list))
                    abnormal-bg-list))))
   (lambda (d-list) (glycemic-control-value-space 'meal-compliant-poor)))
  ((duration 7 0 0 0)
   (lambda (d-list)
     (let* ([meal-list (filter (lambda (d) (meal-event? d)) d-list)]
            [carb-list (filter (lambda (d) (carbohydrate-intake? d)) d-list)]
            [abnormal-bg-list
             (filter (lambda (d) (and (blood-glucose? d)
                                      (abnormal-bg? (post-prandial-bg? d meal-list) d)))
                     d-list)])
       (and (>= (length abnormal-bg-list) 2)
            (andmap (lambda (bg) (not (compliant-bg? bg carb-list)))
                    abnormal-bg-list))))
   (lambda (d-list) (glycemic-control-value-space 'meal-incompliant-poor)))
  ((duration 0 1 0 0)
   (lambda (d-list)
     (let* ([bg-list (filter (lambda (d) (blood-glucose? d)) d-list)]
            [meal-list (filter (lambda (d) (meal-event? d)) d-list)])
       (findf
        (lambda (bg)
          (abnormal-bg? (post-prandial-bg? bg meal-list) bg))
        bg-list)))
   (lambda (d-list) (glycemic-control-value-space 'poor))))

;(verify-process
;   analyse-blood-glucose
;   (list (generate-list
;          blood-glucose
;          (datetime 2019 12 2 7 0 0)
;          (datetime 2019 12 3 24 0 0)
;          (duration 0 12 0 0))
;         (generate-list
;          meal-event
;          (datetime 2019 12 2 6 0 0)
;          (datetime 2019 12 3 24 0 0)
;          (duration 0 12 0 0))
;         (generate-list
;          carbohydrate-intake
;          (datetime 2019 12 2 6 0 0)
;          (datetime 2019 12 3 24 0 0)
;          (duration 0 12 0 0)))
;   (get-datetime (datetime 2019 12 3 19 0 0) (datetime 2019 12 3 19 0 0)))

; Helper function for determining if a BG measurement is post- or pre-prandial.
; Returns the associated meal event if measurement is post-prandial.
(define-syntax-rule (post-prandial-bg? bg-data meal-data-list)
  (findf (lambda (m)
           (and (eq? (observed-event-value m)
                     (bool #t))
                (dt>=? (datetime-range-end
                        (observed-event-valid-datetime-range m))
                       (dt- (observed-property-valid-datetime bg-data)
                            (duration 0 1 0 0)))))
         meal-data-list))

; Helper function for determining if a BG measurement is abnormal or not.
(define-syntax-rule (abnormal-bg? post-prandial? bg-data)
  (if post-prandial?
      (dim>=? (observed-property-value bg-data)
              (dimensioned 140 'mg/dL))
      (dim>=? (observed-property-value bg-data)
              (dimensioned 95 'mg/dL))))

; Helper function for determining if a BG measurement is very abnormal or not.
(define-syntax-rule (very-abnormal-bg? post-prandial? bg-data)
  (if post-prandial?
      (dim>=? (observed-property-value bg-data)
              (dimensioned 150 'mg/dL))
      (dim>=? (observed-property-value bg-data)
              (dimensioned 110 'mg/dL))))

; Helper function for determining if a blood glucose measurement follows
; a compliant meal or not. Defaults to #t.
(define-syntax-rule (compliant-bg? bg-data carb-data-list)
  (let ([carb-data (findf (lambda (c)
                            (dt>=? (observed-property-valid-datetime c)
                                   (dt- (observed-property-valid-datetime bg-data)
                                        (duration 0 1 0 0))))
                          carb-data-list)])
    (if carb-data
        (enum<? (observed-property-value carb-data)
                (carbohydrate-intake-value-space '+))
        #t)))

; analyse-urinary-ketone (AUK) analyses urinary ketone (UK) measurements to
; detect the patient's degree of ketonuria, which can be:
; a) Negative (UK levels are negative in 1 week)
; b) Positive (> 2 positive results in 1 week)
(define-analysis analyse-urinary-ketone #f ketonuria
  ((duration 7 0 0 0)
   (lambda (d-list)
     (let* ([uk-list (filter (lambda (d) (urinary-ketone? d)) d-list)])
       (andmap (lambda (d) (enum<? (observed-property-value d)
                                   (urinary-ketone-value-space '-/+)))
               uk-list)))
   (lambda (d-list) (ketonuria-value-space 'negative)))
  ((duration 7 0 0 0)
   (lambda (d-list)
     (let* ([uk-list (filter (lambda (d) (urinary-ketone? d)) d-list)])
       (> (rosette-count
           (lambda (d) (enum>? (observed-property-value d)
                               (urinary-ketone-value-space '-/+)))
           uk-list)
          2)))
   (lambda (d-list) (ketonuria-value-space 'positive))))

;(verify-process
;   analyse-urinary-ketone
;   (list (generate-list
;          urinary-ketone
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 31 24 0 0)
;          5))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 31 24 0 0)))

; analyse-carbohydrates-intake (ACI) analyses carbohydrates intake (CI) of
; patient to determine his or her degree of compliance to the pre-determined
; diet, which can be:
; a) Insufficient (At least one single negative CI level in a week)
; b) Non-compliant (At least two non-compliance in a week)
(define-analysis analyse-carbohydrates-intake #f carbohydrates-compliance
  ((duration 7 0 0 0)
   (lambda (d-list)
     (let* ([carb-list (filter (lambda (d) (carbohydrate-intake? d)) d-list)])
       (>= (rosette-count
            (lambda (d) (enum<? (observed-property-value d)
                                (carbohydrate-intake-value-space '-/+)))
            carb-list)
           1)))
   (lambda (d-list) (carbohydrates-compliance-value-space 'insufficient)))
  ((duration 7 0 0 0)
   (lambda (d-list)
     (let* ([carb-list (filter (lambda (d) (carbohydrate-intake? d)) d-list)])
       (>= (rosette-count
            (lambda (d) (not (eq? (observed-property-value d)
                                  (carbohydrate-intake-value-space '-/+))))
            carb-list)
           2)))
   (lambda (d-list) (carbohydrates-compliance-value-space 'non-compliant))))

;(verify-process
;   analyse-carbohydrates-intake
;   (list (generate-list
;          carbohydrate-intake
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 31 24 0 0)
;          5))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 31 24 0 0)))

; analyse-blood-pressure (ABP) analyses blood pressure (BP) measurements,
; both systolic and diastolic, to determine the patient's degree of high
; blood glucose, which can be:
; a) High (sBP >= 140 and/or dBP >= 90)
; b) Very high (sBP >= 150 and/or dBP >= 100)
; c) Normal (sBP < 140 and dBP < 90 for more than 2 weeks)
; d) Sustained high (2 high measurements in 6 hours)
; e) Extremely high (sBP >= 160 and dBP >= 110)
(define-analysis analyse-blood-pressure #f hypertension
  ((duration 14 0 0 0)
   (lambda (d-list)
     (andmap
      (lambda (d) (or (and (systolic-blood-pressure? d)
                           (dim<? (observed-property-value d)
                                  (dimensioned 140 'mmHg)))
                      (and (diastolic-blood-pressure? d)
                           (dim<? (observed-property-value d)
                                  (dimensioned 90 'mmHg)))))
      d-list))
   (lambda (d-list) (hypertension-value-space 'normal)))
  ((duration 0 6 0 0)
   (lambda (d-list)
     (>= (rosette-count
          (lambda (d) (or (and (systolic-blood-pressure? d)
                               (dim>=? (observed-property-value d)
                                       (dimensioned 140 'mmHg)))
                          (and (diastolic-blood-pressure? d)
                               (dim>=? (observed-property-value d)
                                       (dimensioned 90 'mmHg)))))
          d-list)
         2))
   (lambda (d-list) (hypertension-value-space 'sustained-high)))
  ((duration 0 0 0 0)
   (lambda (d-list)
     (findf (lambda (d) (or (and (systolic-blood-pressure? d)
                                 (dim>=? (observed-property-value d)
                                         (dimensioned 160 'mmHg)))
                            (and (diastolic-blood-pressure? d)
                                 (dim>=? (observed-property-value d)
                                         (dimensioned 110 'mmHg)))))
            d-list))
   (lambda (d-list) (hypertension-value-space 'extremely-high)))
  ((duration 0 0 0 0)
   (lambda (d-list)
     (findf (lambda (d) (or (and (systolic-blood-pressure? d)
                                 (dim>=? (observed-property-value d)
                                         (dimensioned 150 'mmHg)))
                            (and (diastolic-blood-pressure? d)
                                 (dim>=? (observed-property-value d)
                                         (dimensioned 100 'mmHg)))))
            d-list))
   (lambda (d-list) (hypertension-value-space 'very-high)))
  ((duration 0 0 0 0)
   (lambda (d-list)
     (findf (lambda (d) (or (and (systolic-blood-pressure? d)
                                 (dim>=? (observed-property-value d)
                                         (dimensioned 140 'mmHg)))
                            (and (diastolic-blood-pressure? d)
                                 (dim>=? (observed-property-value d)
                                         (dimensioned 90 'mmHg)))))
            d-list))
   (lambda (d-list) (hypertension-value-space 'high))))

;(verify-process
;   analyse-blood-pressure
;   (list (generate-list
;          systolic-blood-pressure
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2)
;         (generate-list
;          diastolic-blood-pressure
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bg-twice-weekly (DBg2Wk) relates to the decision to adjust blood
; glucose monitoring to two days each week instead of daily. The decision
; criteria involves the following abstraction(s):
; 1) glycemic-control ('good)
; It affects the following actions and processes:
; 1) monitor-blood-glucose (two days every week)
; 2) decide-bg-nutrition-change (disabled)
; 3) decide-bg-insulin (disabled)
; 4) decide-bg-twice-weekly (disabled)
; 5) decide-bg-daily (enabled after 7 days)
(define-decision
  decide-bg-twice-weekly
  #f
  bg-twice-weekly-plan
  (#:instructions
   (control-template 'monitor-blood-glucose
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern
                      (duration 0 7 0 0) (duration 0 9 0 0)
                      (duration 0 14 0 0) (duration 0 20 0 0)
                      (duration 3 7 0 0) (duration 3 9 0 0)
                      (duration 3 14 0 0) (duration 3 20 0 0)
                      #:interval (duration 7 0 0 0))
                     #t)
   (control-template 'decide-bg-nutrition-change #f)
   (control-template 'decide-bg-insulin #f)
   (control-template 'decide-bg-twice-weekly #f)
   (control-template 'decide-bg-daily
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d) (and (glycemic-control? d)
                       (eq? (abstraction-value d)
                            (glycemic-control-value-space 'good))))
      d-list))))

;(verify-process
;   decide-bg-twice-weekly
;   (list (generate-list
;          glycemic-control
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bg-daily (DBgDaily) relates to the decision to adjust blood glucose
; monitoring to daily (instead of two days each week). The decision criteria
; involves the following abstraction(s):
; 1) glycemic-control (2 abnormal values in a week)
; It affects the following actions and processes:
; 1) monitor-blood-glucose (daily)
; 2) decide-bg-nutrition-change (enabled after 7 days)
; 3) decide-bg-insulin (enabled after 7 days)
; 4) decide-bg-twice-weekly (enabled after 7 days)
; 5) decide-bg-daily (disabled)
(define-decision
  decide-bg-daily
  #f
  bg-daily-plan
  (#:instructions
   (control-template 'monitor-blood-glucose
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern
                      (duration 0 7 0 0) (duration 0 9 0 0)
                      (duration 0 14 0 0) (duration 0 20 0 0)
                      #:interval (duration 1 0 0 0))
                     #t)
   (control-template 'decide-bg-nutrition-change
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-insulin
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-twice-weekly
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-daily #f))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d)
        (and (glycemic-control? d)
             (or (eq? (abstraction-value d)
                      (glycemic-control-value-space 'meal-compliant-poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'meal-incompliant-poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'non-related-poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'very-poor)))))
      d-list))))

;(verify-process
;   decide-bg-daily
;   (list (generate-list
;          glycemic-control
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bg-insulin (DBgInsulin) is a proxy process for deciding to start
; insulin therapy. The decision criteria involves the following abstraction(s):
; 1) glycemic-control (not 'good or 'poor)
; 2) ketonuria ('positive)
; It affects the following actions and processes:
; 1) decide-bg-nutrition-change (disabled)
; 2) decide-bg-twice-weekly (disabled)
; 3) decide-bg-insulin (disabled)
; 4) decide-bg-twice-weekly-post-insulin (enabled after 7 days)
; 5) decide-bg-insulin-adjust (enabled after 7 days)
; 6) administer-insulin-action (4 times each day)
; (Note: Since the guideline does not specify the amount of insulin to 
; prescribe, its set to an arbitrary value of -1.
(define-decision
  decide-bg-insulin
  #t
  start-insulin-plan
  (#:instructions
   (control-template 'decide-bg-nutrition-change #f)
   (control-template 'decide-bg-twice-weekly #f)
   (control-template 'decide-bg-insulin #f)
   (control-template 'decide-bg-twice-weekly-post-insulin
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-insulin-adjust
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (culminating-action-template 'administer-insulin-action
                                (relative-schedule
                                 #:rounding (duration 1 0 0 0)
                                 #:pattern
                                 (duration 0 7 0 0) (duration 0 9 0 0)
                                 (duration 0 14 0 0) (duration 0 20 0 0)
                                 #:interval (duration 1 0 0 0))
                                (dimensioned -1 'IU)))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d)
        (and (glycemic-control? d)
             (or (eq? (abstraction-value d)
                      (glycemic-control-value-space 'non-related-poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'very-poor)))))
      d-list))
   (lambda (d-list)
     (findf
      (lambda (d)
        (and (glycemic-control? d)
             (eq? (abstraction-value d)
                  (glycemic-control-value-space 'meal-incompliant-poor))))
      d-list))
   (lambda (d-list)
     (and (findf (lambda (d)
                   (and (glycemic-control? d)
                        (eq? (abstraction-value d)
                             (glycemic-control-value-space 'meal-compliant-poor))))
                 d-list)
          (findf (lambda (d)
                   (and (ketonuria? d)
                        (eq? (abstraction-value d)
                             (ketonuria-value-space 'positive))))
                 d-list)))))

;(verify-process
;   decide-bg-insulin
;   (list (generate-list
;          glycemic-control
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2)
;         (generate-list
;          ketonuria
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bg-twice-weekly-post-insulin (DBg2WkPostInsulin) relates to the
; decision to adjust blood glucose monitoring to two days each week instead of
; daily (after the prescription of insulin). The decision criteria involves the
; following abstraction(s):
; 1) glycemic-control ('good)
; It affects the following actions and processes:
; 1) monitor-blood-glucose (two days every week)
; 2) decide-bg-insulin-adjust (disabled)
; 3) decide-bg-twice-weekly-post-insulin (disabled)
; 4) decide-bg-daily-post-insulin (enabled after 7 days)
(define-decision
  decide-bg-twice-weekly-post-insulin
  #f
  bg-twice-weekly-plan
  (#:instructions
   (control-template 'monitor-blood-glucose
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern
                      (duration 0 7 0 0) (duration 0 9 0 0)
                      (duration 0 14 0 0) (duration 0 20 0 0)
                      (duration 3 7 0 0) (duration 3 9 0 0)
                      (duration 3 14 0 0) (duration 3 20 0 0)
                      #:interval (duration 7 0 0 0))
                     #t)
   (control-template 'decide-bg-insulin-adjust #f)
   (control-template 'decide-bg-twice-weekly-post-insulin #f)
   (control-template 'decide-bg-daily-post-insulin
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d) (and (glycemic-control? d)
                       (eq? (abstraction-value d)
                            (glycemic-control-value-space 'good))))
      d-list))))

;(verify-process
;   decide-bg-twice-weekly-post-insulin
;   (list (generate-list
;          glycemic-control
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bg-daily-post-insulin (DBgDailyPostInsulin) relates to the decision
; to adjust blood glucose monitoring to daily (instead of two days each week)
; after the prescription of insulin. The decision criteria involves the
; following abstraction(s):
; 1) glycemic-control (2 abnormal values in a week)
; It affects the following actions and processes:
; 1) monitor-blood-glucose (daily)
; 2) decide-bg-insulin-adjust (enabled after 7 days)
; 3) decide-bg-twice-weekly-post-insulin (enabled after 7 days)
; 4) decide-bg-daily-post-insulin (disabled)
(define-decision
  decide-bg-daily-post-insulin
  #f
  bg-daily-plan
  (#:instructions
   (control-template 'monitor-blood-glucose
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern
                      (duration 0 7 0 0) (duration 0 9 0 0)
                      (duration 0 14 0 0) (duration 0 20 0 0)
                      #:interval (duration 1 0 0 0))
                     #t)
   (control-template 'decide-bg-insulin-adjust
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-twice-weekly-post-insulin
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-daily-post-insulin #f))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d)
        (and (glycemic-control? d)
             (or (eq? (abstraction-value d)
                      (glycemic-control-value-space 'meal-compliant-poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'meal-incompliant-poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'non-related-poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'very-poor)))))
      d-list))))

;(verify-process
;   decide-bg-daily-post-insulin
;   (list (generate-list
;          glycemic-control
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bg-insulin-adjust (DBgInsAdjust) is a proxy process for adjusting the
; insulin therapy for the patient. The decision criteria involves the following
; abstraction(s):
; 1) glycemic-control (1 abnormal value detected).
; It affects the following actions and processes:
; 1) administer-insulin-action.
(define-decision
  decide-bg-insulin-adjust
  #t
  adjust-insulin-plan
  (#:instructions
   (culminating-action-template 'administer-insulin-action
                                (relative-schedule
                                 #:rounding (duration 1 0 0 0)
                                 #:pattern
                                 (duration 0 7 0 0) (duration 0 9 0 0)
                                 (duration 0 14 0 0) (duration 0 20 0 0)
                                 #:interval (duration 1 0 0 0))
                                (dimensioned -1 'IU)))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d)
        (and (glycemic-control? d)
             (or (eq? (abstraction-value d)
                      (glycemic-control-value-space 'poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'meal-compliant-poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'meal-incompliant-poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'non-related-poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'very-poor)))))
      d-list))))

;(verify-process
;   decide-bg-insulin-adjust
;   (list (generate-list
;          glycemic-control
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bg-nutrition-change (DBgCarb) is a proxy process for changing the
; nutritional prescription of the patient due to poor glycemic control.
; Specifically, the decision criteria involves the following abstraction(s):
; 1) glyemic-control ('meal-compliant-poor)
; 2) ketonuria ('negative)
; It affects the following actions and processes:
; 1) decide-bg-twice-weekly (disabled)
; 2) decide-bg-nutrition-change (disabled)
; 3) decide-bg-insulin (disabled)
; 4) decide-bg-twice-weekly-post-nutrition (enabled after 7 days)
; 5) decide-bg-insulin-post-nutrition (enabled after 7 days)
; 6) change-diet-action (4 times each day)
; Note: Since the guideline does not specify a concrete nutrition change,
; its set to an arbitrary value of -1.
(define-decision
  decide-bg-nutrition-change
  #t
  change-nutrition-plan
  (#:instructions
   (control-template 'decide-bg-twice-weekly #f)
   (control-template 'decide-bg-nutrition-change #f)
   (control-template 'decide-bg-insulin #f)
   (control-template 'decide-bg-twice-weekly-post-nutrition
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-insulin-post-nutrition
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (culminating-action-template 'change-diet-action
                                (relative-schedule
                                 #:rounding (duration 1 0 0 0)
                                 #:pattern
                                 (duration 0 7 0 0) (duration 0 9 0 0)
                                 (duration 0 14 0 0) (duration 0 20 0 0)
                                 #:interval (duration 1 0 0 0))
                                (dimensioned -1 'g)))
  (#:criteria
   (lambda (d-list)
     (and (findf (lambda (d)
                   (and (glycemic-control? d)
                        (eq? (abstraction-value d)
                             (glycemic-control-value-space 'meal-compliant-poor))))
                 d-list)
          (findf (lambda (d)
                   (and (ketonuria? d)
                        (eq? (abstraction-value d)
                             (ketonuria-value-space 'negative))))
                 d-list)))))

;(verify-process
;   decide-bg-nutrition-change
;   (list (generate-list
;          glycemic-control
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2)
;         (generate-list
;          ketonuria
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bg-twice-weekly-post-nutrition (DBg2WkPostNutrition) relates to the
; decision to adjust blood glucose monitoring to two days each week instead of
; daily (after the changing nutrition prescription). The decision criteria
; involves the following abstraction(s):
; 1) glycemic-control ('good)
; It affects the following actions and processes:
; 1) monitor-blood-glucose (two days every week)
; 2) decide-bg-twice-weekly-post-nutrition (disabled)
; 3) decide-bg-insulin-post-nutrition (disabled)
; 4) decide-bg-daily-post-nutrition (enabled after 7 days)
(define-decision
  decide-bg-twice-weekly-post-nutrition
  #f
  bg-twice-weekly-plan
  (#:instructions
   (control-template 'monitor-blood-glucose
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern
                      (duration 0 7 0 0) (duration 0 9 0 0)
                      (duration 0 14 0 0) (duration 0 20 0 0)
                      (duration 3 7 0 0) (duration 3 9 0 0)
                      (duration 3 14 0 0) (duration 3 20 0 0)
                      #:interval (duration 7 0 0 0))
                     #t)
   (control-template 'decide-bg-twice-weekly-post-nutrition #f)
   (control-template 'decide-bg-insulin-post-nutrition #f)
   (control-template 'decide-bg-daily-post-nutrition
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d) (and (glycemic-control? d)
                       (eq? (abstraction-value d)
                            (glycemic-control-value-space 'good))))
      d-list))))

;(verify-process
;   decide-bg-twice-weekly-post-nutrition
;   (list (generate-list
;          glycemic-control
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bg-daily-post-nutrition (DBgDailyPostNutrition) relates to the decision
; to adjust blood glucose monitoring to daily (instead of two days each week)
; after the changing nutrition prescription. The decision criteria involves the
; following abstraction(s):
; 1) glycemic-control (2 abnormal values in a week)
; It affects the following actions and processes:
; 1) monitor-blood-glucose (daily)
; 2) decide-bg-insulin-post-nutrition (enabled after 7 days)
; 3) decide-bg-twice-weekly-post-nutrition (enabled after 7 days)
; 4) decide-bg-daily-post-nutrition (disabled)
(define-decision
  decide-bg-daily-post-nutrition
  #f
  bg-daily-plan
  (#:instructions
   (control-template 'monitor-blood-glucose
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern
                      (duration 0 7 0 0) (duration 0 9 0 0)
                      (duration 0 14 0 0) (duration 0 20 0 0)
                      #:interval (duration 1 0 0 0))
                     #t)
   (control-template 'decide-bg-insulin-post-nutrition
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-twice-weekly-post-nutrition
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-daily-post-nutrition #f))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d)
        (and (glycemic-control? d)
             (or (eq? (abstraction-value d)
                      (glycemic-control-value-space 'meal-compliant-poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'meal-incompliant-poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'non-related-poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'very-poor)))))
      d-list))))

;(verify-process
;   decide-bg-daily-post-nutrition
;   (list (generate-list
;          glycemic-control
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bg-insulin-post-nutrition (DBgInsulinPostNutrition) is a proxy process
; for deciding to start insulin therapy (after changing nutrition prescription).
; The decision criteria involves the following abstraction(s):
; 1) glycemic-control (not 'good or 'poor)
; It affects the following actions and processes:
; 1) decide-bg-twice-weekly-post-nutrition (disabled)
; 2) decide-bg-insulin-post-nutrition (disabled)
; 3) decide-bg-twice-weekly-post-insulin (enabled after 7 days)
; 4) decide-bg-insulin-adjust (enabled after 7 days)
; 5) administer-insulin-action (4 times each day)
; (Note: Since the guideline does not specify the amount of insulin to 
; prescribe, its set to an arbitrary value of -1.
(define-decision
  decide-bg-insulin-post-nutrition
  #t
  start-insulin-plan
  (#:instructions
   (control-template 'decide-bg-twice-weekly-post-nutrition #f)
   (control-template 'decide-bg-insulin-post-nutrition #f)
   (control-template 'decide-bg-twice-weekly-post-insulin
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-insulin-adjust
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (culminating-action-template 'administer-insulin-action
                                (relative-schedule
                                 #:rounding (duration 1 0 0 0)
                                 #:pattern
                                 (duration 0 7 0 0) (duration 0 9 0 0)
                                 (duration 0 14 0 0) (duration 0 20 0 0)
                                 #:interval (duration 1 0 0 0))
                                (dimensioned -1 'IU)))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d)
        (and (glycemic-control? d)
             (or (eq? (abstraction-value d)
                      (glycemic-control-value-space 'meal-compliant-poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'meal-incompliant-poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'non-related-poor))
                 (eq? (abstraction-value d)
                      (glycemic-control-value-space 'very-poor)))))
      d-list))))

;(verify-process
;   decide-bg-insulin-post-nutrition
;   (list (generate-list
;          glycemic-control
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-uk-twice-weekly (DUk2Wk) relates to the decision to adjust urinary
; ketone monitoring to two days each week instead of daily. The decision
; criteria involves the following abstraction(s):
; 1) ketonuria ('negative)
; It affects the following actions and processes:
; 1) monitor-urinary-ketones (two days every week)
; 2) decide-uk-dinner-increase (disabled)
; 3) decide-uk-twice-weekly (disabled)
; 4) decide-uk-daily (enabled after 7 days)
(define-decision
  decide-uk-twice-weekly
  #f
  uk-twice-weekly-plan
  (#:instructions
   (control-template 'monitor-urinary-ketones
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern (duration 0 7 0 0) (duration 3 7 0 0)
                      #:interval (duration 7 0 0 0))
                     #t)
   (control-template 'decide-uk-dinner-increase #f)
   (control-template 'decide-uk-twice-weekly #f)
   (control-template 'decide-uk-daily
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d) (and (ketonuria? d)
                       (eq? (abstraction-value d)
                            (ketonuria-value-space 'negative))))
      d-list))))

;(verify-process
;   decide-uk-twice-weekly
;   (list (generate-list
;          ketonuria
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-uk-daily (DUkDaily) relates to the decision to adjust urinary ketone
; monitoring to daily (instead of two days each week). The decision criteria
; involves the following abstraction(s):
; 1) ketonuria ('positive)
; It affects the following actions and processes:
; 1) monitor-urinary-ketones (daily)
; 2) decide-uk-dinner-increase (enabled after 7 days)
; 3) decide-uk-twice-weekly (enabled after 7 days)
; 4) decide-uk-daily (disabled)
(define-decision
  decide-uk-daily
  #f
  uk-daily-plan
  (#:instructions
   (control-template 'monitor-urinary-ketones
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern (duration 0 7 0 0)
                      #:interval (duration 1 0 0 0))
                     #t)
   (control-template 'decide-uk-dinner-increase
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-uk-twice-weekly
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-uk-daily #f))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d) (and (ketonuria? d)
                       (eq? (abstraction-value d)
                            (ketonuria-value-space 'positive))))
      d-list))))

;(verify-process
;   decide-uk-daily
;   (list (generate-list
;          ketonuria
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-uk-dinner-increase (DUkCarb) relates to the decision to increase
; carbohydrates intake at dinner. The decision criteria involves the following
; abstraction(s):
; 1) ketonuria ('positive)
; 2) carbohydrates-compliance (not 'insufficient)
; It affects the following actions and processes:
; 1) decide-uk-dinner-increase (disabled)
; 2) decide-uk-twice-weekly (disabled)
; 3) decide-uk-daily (disabled)
; 4) decide-uk-twice-weekly-post-dinner (enabled after 7 days)
; 5) change-dinner-action (daily)
(define-decision
  decide-uk-dinner-increase
  #f
  increase-dinner-intake-plan
  (#:instructions
   (control-template 'decide-uk-dinner-increase #f)
   (control-template 'decide-uk-twice-weekly #f)
   (control-template 'decide-uk-daily #f)
   (control-template 'decide-uk-twice-weekly-post-dinner
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 14 0 0 0)
                      #:interval #t)
                     #t)
   (culminating-action-template 'change-dinner-action
                                (relative-schedule
                                 #:rounding (duration 1 0 0 0)
                                 #:pattern (duration 0 19 0 0)
                                 #:interval (duration 1 0 0 0))
                                (dimensioned 10 'g)))
  (#:criteria
   (lambda (d-list)
     (and 
      (findf
       (lambda (d) (and (ketonuria? d)
                        (eq? (abstraction-value d)
                             (ketonuria-value-space 'positive))))
       d-list)
      (not (findf
            (lambda (d) (and (carbohydrates-compliance? d)
                             (eq? (abstraction-value d)
                                  (carbohydrates-compliance-value-space 'insufficient))))
            d-list))))))

;(verify-process
;   decide-uk-dinner-increase
;   (list (generate-list
;          ketonuria
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2)
;         (generate-list
;          carbohydrates-compliance
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-uk-twice-weekly-post-dinner (DUk2WkPostDinner) relates to the decision
; to adjust urinary ketone monitoring to two days each week instead of daily
; (after increasing carbohydrates intake at dinner). The decision criteria
; involves the following abstraction(s):
; 1) ketonuria ('negative)
; It affects the following actions and processes:
; 1) monitor-urinary-ketones (two days every week)
; 3) decide-uk-twice-weekly-post-dinner (disabled)
; 4) decide-uk-daily-post-dinner (enabled after 7 days)
(define-decision
  decide-uk-twice-weekly-post-dinner
  #f
  uk-twice-weekly-plan
  (#:instructions
   (control-template 'monitor-urinary-ketones
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern (duration 0 7 0 0) (duration 3 7 0 0)
                      #:interval (duration 7 0 0 0))
                     #t)
   (control-template 'decide-uk-twice-weekly-post-dinner #f)
   (control-template 'decide-uk-daily-post-dinner
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d) (and (ketonuria? d)
                       (eq? (abstraction-value d)
                            (ketonuria-value-space 'negative))))
      d-list))))

;(verify-process
;   decide-uk-twice-weekly-post-dinner
;   (list (generate-list
;          ketonuria
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-uk-daily-post-dinner (DUkDailyPostDinner) relates to the decision to
; adjust urinary ketone monitoring to daily (instead of two days each week)
; after increasing carbohydrates intake. The decision criteria involves the
; following abstraction(s):
; 1) ketonuria ('positive)
; It affects the following actions and processes:
; 1) monitor-urinary-ketones (daily)
; 2) decide-uk-twice-weekly-post-dinner (enabled after 7 days)
; 3) decide-uk-daily-post-dinner (disabled)
(define-decision
  decide-uk-daily-post-dinner
  #f
  uk-daily-plan
  (#:instructions
   (control-template 'monitor-urinary-ketones
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern (duration 0 7 0 0)
                      #:interval (duration 1 0 0 0))
                     #t)
   (control-template 'decide-uk-twice-weekly-post-dinner
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-uk-daily-post-dinner #f))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d) (and (ketonuria? d)
                       (eq? (abstraction-value d)
                            (ketonuria-value-space 'positive))))
      d-list))))

;(verify-process
;   decide-uk-daily-post-dinner
;   (list (generate-list
;          ketonuria
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bp-once-weekly (DBp1Wk) relates to the decision to adjust blood
; pressure monitoring to once a week instead of twice a week. The decision
; criteria involves the following abstraction(s):
; 1) hypertension ('normal)
; It affects the following actions and processes:
; 1) monitor-blood-pressure (once every week)
; 2) decide-bp-once-weekly (disabled)
; 3) decide-bp-twice-weekly (enabled after 7 days)
(define-decision
  decide-bp-once-weekly
  #f
  bp-once-weekly-plan
  (#:instructions
   (control-template 'monitor-systolic-blood-pressure
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern (duration 0 7 0 0) (duration 0 11 0 0)
                      #:interval (duration 7 0 0 0))
                     #t)
   (control-template 'monitor-diastolic-blood-pressure
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern (duration 0 7 0 0) (duration 0 11 0 0)
                      #:interval (duration 7 0 0 0))
                     #t)
   (control-template 'decide-bp-once-weekly #f)
   (control-template 'decide-bp-twice-weekly
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d) (and (hypertension? d)
                       (eq? (abstraction-value d)
                            (hypertension-value-space 'normal))))
      d-list))))

;(verify-process
;   decide-bp-once-weekly
;   (list (generate-list
;          hypertension
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bp-twice-weekly (DBp2Wk) relates to the decision to adjust blood
; pressure monitoring to twice a week instead of once a week. The decision
; criteria involves the following abstraction(s):
; 1) hypertension ('high)
; It affects the following actions and processes:
; 1) monitor-blood-pressure (twice every week)
; 2) decide-bp-once-weekly (enabled after 7 days)
; 3) decide-bp-twice-weekly (disabled)
(define-decision
  decide-bp-twice-weekly
  #f
  bp-twice-weekly-plan
  (#:instructions
   (control-template 'monitor-systolic-blood-pressure
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern
                      (duration 0 7 0 0) (duration 0 11 0 0)
                      (duration 3 7 0 0) (duration 3 11 0 0)
                      #:interval (duration 7 0 0 0))
                     #t)
   (control-template 'monitor-diastolic-blood-pressure
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern
                      (duration 0 7 0 0) (duration 0 11 0 0)
                      (duration 3 7 0 0) (duration 3 11 0 0)
                      #:interval (duration 7 0 0 0))
                     #t)
   (control-template 'decide-bp-once-weekly
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bp-twice-weekly #f))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d) (and (hypertension? d)
                       (eq? (abstraction-value d)
                            (hypertension-value-space 'high))))
      d-list))))

;(verify-process
;   decide-bp-twice-weekly
;   (list (generate-list
;          hypertension
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bp-chronic (DBpChronic) is a proxy process for starting the chronic
; blood pressure monitoring plan. The decision criteria involves the following
; abstraction(s):
; 1) hypertension ('sustained-high)
; It affects the following actions and processes:
; 1) monitor-blood-pressure (every two days)
; 2) decide-bp-once-weekly (disabled)
; 3) decide-bp-twice-weekly (disabled)
; 4) decide-bp-chronic (disabled)
(define-decision
  decide-bp-chronic
  #t
  chronic-hypertension-plan
  (#:instructions
   (control-template 'monitor-systolic-blood-pressure
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern (duration 0 7 0 0)
                      #:interval (duration 2 0 0 0))
                     #t)
   (control-template 'monitor-diastolic-blood-pressure
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern (duration 0 7 0 0)
                      #:interval (duration 2 0 0 0))
                     #t)
   (control-template 'decide-bp-once-weekly #f)
   (control-template 'decide-bp-twice-weekly #f)
   (control-template 'decide-bp-chronic #f))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d) (and (hypertension? d)
                       (eq? (abstraction-value d)
                            (hypertension-value-space 'sustained-high))))
      d-list))))

;(verify-process
;   decide-bp-chronic
;   (list (generate-list
;          hypertension
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bp-gestational (DBpGestational) is a proxy process for starting the
; gestational blood pressure monitoring plan. The decision criteria involves
; the following abstraction(s):
; 1) hypertension ('sustained-high)
; It affects the following actions and processes:
; 1) monitor-blood-pressure (every two days)
; 2) decide-bp-once-weekly (disabled)
; 3) decide-bp-twice-weekly (disabled)
; 4) decide-bp-gestational (disabled)
; 5) decide-bp-once-weekly-gestational (enabled after 7 days)
; 6) decide-bp-hourly-gestational (enabled after 7 days)
(define-decision
  decide-bp-gestational
  #t
  gestational-hypertension-plan
  (#:instructions
   (control-template 'monitor-systolic-blood-pressure
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern (duration 0 7 0 0)
                      #:interval (duration 2 0 0 0))
                     #t)
   (control-template 'monitor-diastolic-blood-pressure
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern (duration 0 7 0 0)
                      #:interval (duration 2 0 0 0))
                     #t)
   (control-template 'decide-bp-once-weekly #f)
   (control-template 'decide-bp-twice-weekly #f)
   (control-template 'decide-bp-gestational #f)
   (control-template 'decide-bp-once-weekly-gestational
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bp-hourly-gestational
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d) (and (hypertension? d)
                       (eq? (abstraction-value d)
                            (hypertension-value-space 'sustained-high))))
      d-list))))

;(verify-process
;   decide-bp-gestational
;   (list (generate-list
;          hypertension
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bp-once-weekly-gestational (DBp1WkGestational) relates to the decision
; to adjust blood pressure monitoring to once a week instead of every two days
; (in the gestational hypertension plan). The decision criteria involves the
; following abstraction(s):
; 1) hypertension (not 'very-high or 'extremely-high)
; 2) proteinuria (false)
; It affects the following actions and processes:
; 1) monitor-blood-pressure (once a week)
; 2) decide-bp-once-weekly-gestational (disabled)
; 3) decide-bp-hourly-gestational (disabled)
; 4) decide-bp-two-days-gestational (enabled after 7 days)
(define-decision
  decide-bp-once-weekly-gestational
  #f
  gestational-weekly-plan
  (#:instructions
   (control-template 'monitor-systolic-blood-pressure
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern (duration 0 7 0 0)
                      #:interval (duration 7 0 0 0))
                     #t)
   (control-template 'monitor-diastolic-blood-pressure
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern (duration 0 7 0 0)
                      #:interval (duration 7 0 0 0))
                     #t)
   (control-template 'decide-bp-once-weekly-gestational #f)
   (control-template 'decide-bp-hourly-gestational #f)
   (control-template 'decide-bp-two-days-gestational
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t))
  (#:criteria
   (lambda (d-list)
     (and (not (findf
                (lambda (d) (and (hypertension? d)
                                 (or (eq? (abstraction-value d)
                                          (hypertension-value-space 'very-high))
                                     (eq? (abstraction-value d)
                                          (hypertension-value-space 'extremely-high)))))
                d-list))
          (findf
           (lambda (d) (and (proteinuria? d)
                            (eq? (abstraction-value d) (bool #f))))
           d-list)))))

;(verify-process
;   decide-bp-once-weekly-gestational
;   (list (generate-list
;          hypertension
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2)
;         (generate-list
;          proteinuria
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bp-two-days-gestational (DBp2DaysGest) relates to decision to adjust
; blood pressure monitoring to every two days instead of once a week (in the
; gestational hypertension workflow). It is equivalent to the process decide-
; bp-gestational except that the decision criteria involves:
; 1) hypertension ('very-high or 'extremely-high)
; 2) proteinuria (true)
; It affects the following actions and processes:
; 1) monitor-blood-pressure (every two days)
; 2) decide-bp-once-weekly-gestational (enabled after 7 days)
; 3) decide-bp-hourly-gestational (enabled after 7 days)
; 4) decide-bp-two-days-gestational (disabled)
(define-decision
  decide-bp-two-days-gestational
  #f
  gestational-hypertension-plan
  (#:instructions
   (control-template 'monitor-systolic-blood-pressure
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern (duration 0 7 0 0)
                      #:interval (duration 2 0 0 0))
                     #t)
   (control-template 'monitor-diastolic-blood-pressure
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern (duration 0 7 0 0)
                      #:interval (duration 2 0 0 0))
                     #t)
   (control-template 'decide-bp-once-weekly-gestational
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bp-hourly-gestational
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:pattern (duration 7 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bp-two-days-gestational #f))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d) (and (hypertension? d)
                       (or (eq? (abstraction-value d)
                            (hypertension-value-space 'very-high))
                           (eq? (abstraction-value d)
                            (hypertension-value-space 'extremely-high)))))
      d-list))
   (lambda (d-list)
     (findf
      (lambda (d) (and (proteinuria? d)
                       (eq? (abstraction-value d) (bool #t))))
      d-list))))

;(verify-process
;   decide-bp-two-days-gestational
;   (list (generate-list
;          hypertension
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2)
;         (generate-list
;          proteinuria
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; decide-bp-hourly-gestational (DBpHoursGest) is a proxy process for deciding
; to adjust blood pressure monitoring to every few hours (e.g. 4) instead of
; once a week (in the gestational hypertension workflow). The decision criteria
; involves:
; 1) hypertension (not 'very-high or 'extremely-high)
; 2) proteinuria (true)
; It affects the following actions and processes:
; 1) monitor-blood-pressure (every 4 hours)
; 2) decide-bp-once-weekly-gestational (disabled)
; 3) decide-bp-hourly-gestational (disabled)
(define-decision
  decide-bp-hourly-gestational
  #t
  gestational-hours-plan
  (#:instructions
   (control-template 'monitor-systolic-blood-pressure
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern (duration 0 7 0 0)
                      #:interval (duration 0 4 0 0))
                     #t)
   (control-template 'monitor-diastolic-blood-pressure
                     (relative-schedule
                      #:rounding (duration 1 0 0 0)
                      #:pattern (duration 0 7 0 0)
                      #:interval (duration 0 4 0 0))
                     #t)
   (control-template 'decide-bp-once-weekly-gestational #f)
   (control-template 'decide-bp-hourly-gestational #f))
  (#:criteria
   (lambda (d-list)
     (and (not (findf
                (lambda (d) (and (hypertension? d)
                                 (or (eq? (abstraction-value d)
                                          (hypertension-value-space 'very-high))
                                     (eq? (abstraction-value d)
                                          (hypertension-value-space 'extremely-high)))))
                d-list))
          (findf
           (lambda (d) (and (proteinuria? d)
                            (eq? (abstraction-value d) (bool #t))))
           d-list)))))

;(verify-process
;   decide-bp-hourly-gestational
;   (list (generate-list
;          hypertension
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2)
;         (generate-list
;          proteinuria
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))

; effectuate-administer-insulin is a proxy process for administering insulin.
(define-effectuation
  effectuate-administer-insulin
  #t
  administer-insulin-action
  (target-schedule
   #:plan adjust-insulin-plan
   #:instruction 'administer-insulin-action
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan start-insulin-plan
   #:instruction 'administer-insulin-action
   #:predicate (lambda (i-list) #t)))

;(verify-process
; effectuate-administer-insulin
; (list (generate-list
;        adjust-insulin-plan
;        (datetime 2019 12 7 0 0 0)
;        (datetime 2019 12 7 24 0 0)
;        1))
; (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))
;
;(verify-process
; effectuate-administer-insulin
; (list (generate-list
;        start-insulin-plan
;        (datetime 2019 12 7 0 0 0)
;        (datetime 2019 12 7 24 0 0)
;        1))
; (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-change-diet is responsible for effectuating the change
; to the patient's diet (due to poor glycaemic control).
(define-effectuation
  effectuate-change-diet
  #t
  change-diet-action
  (target-schedule
   #:plan change-nutrition-plan
   #:instruction 'change-diet-action
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-change-diet
;   (list (generate-list
;          change-nutrition-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-change-dinner is responsible for effectuating the change to the
; patient's carbohydrates intake at dinner (due to positive ketonuria).
(define-effectuation
  effectuate-change-dinner
  #t
  change-dinner-action
  (target-schedule
   #:plan increase-dinner-intake-plan
   #:instruction 'change-dinner-action
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-change-dinner
;   (list (generate-list
;          increase-dinner-intake-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-monitor-bg is responsible for effectuating any changes to the
; monitoring of blood glucose.
(define-effectuation
  effectuate-monitor-bg
  #f
  monitor-bg-control
  (target-schedule
   #:plan bg-twice-weekly-plan
   #:instruction 'monitor-blood-glucose
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan bg-daily-plan
   #:instruction 'monitor-blood-glucose
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-monitor-bg
;   (list (generate-list
;          bg-twice-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          bg-daily-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-bg-nutrition-change is responsible for effectuating the decide-bg-
; nutrition-change process.
(define-effectuation
  effectuate-bg-nutrition-change
  #f
  bg-nutrition-change-control
  (target-schedule
   #:plan bg-twice-weekly-plan
   #:instruction 'decide-bg-nutrition-change
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan change-nutrition-plan
   #:instruction 'decide-bg-nutrition-change
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan start-insulin-plan
   #:instruction 'decide-bg-nutrition-change
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan bg-daily-plan
   #:instruction 'decide-bg-nutrition-change
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-bg-nutrition-change
;   (list (generate-list
;          bg-twice-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          change-nutrition-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          start-insulin-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          bg-daily-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-bg-insulin-control is responsible for effectuating the 'decide-bg-
; insulin process (including its variants).
(define-effectuation
  effectuate-bg-insulin-control
  #f
  bg-insulin-control
  (target-schedule
   #:plan bg-twice-weekly-plan
   #:instruction 'decide-bg-insulin
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan start-insulin-plan
   #:instruction 'decide-bg-insulin
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan change-nutrition-plan
   #:instruction 'decide-bg-insulin
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan bg-daily-plan
   #:instruction 'decide-bg-insulin
   #:predicate (lambda (i-list) #t)))

(define-effectuation
  effectuate-bg-insulin-post-nutrition-control
  #f
  bg-insulin-control
  (target-schedule
   #:plan bg-twice-weekly-plan
   #:instruction 'decide-bg-insulin-post-nutrition
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan change-nutrition-plan
   #:instruction 'decide-bg-insulin-post-nutrition
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan start-insulin-plan
   #:instruction 'decide-bg-insulin-post-nutrition
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan bg-daily-plan
   #:instruction 'decide-bg-insulin-post-nutrition
   #:predicate (lambda (i-list) #t)))

(define-effectuation
  effectuate-bg-insulin-adjust-control
  #f
  bg-insulin-control  
  (target-schedule
   #:plan bg-twice-weekly-plan
   #:instruction 'decide-bg-insulin-adjust
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan start-insulin-plan
   #:instruction 'decide-bg-insulin-adjust
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan bg-daily-plan
   #:instruction 'decide-bg-insulin-adjust
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-bg-insulin-control
;   (list (generate-list
;          bg-twice-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          change-nutrition-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          start-insulin-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          bg-daily-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-bg-twice-weekly-control is responsible for effectuating the decide-
; bg-twice-weekly process (and its variants).
(define-effectuation
  effectuate-bg-twice-weekly-control
  #f
  bg-twice-weekly-control
  (target-schedule
   #:plan bg-twice-weekly-plan
   #:instruction 'decide-bg-twice-weekly
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan change-nutrition-plan
   #:instruction 'decide-bg-twice-weekly
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan start-insulin-plan
   #:instruction 'decide-bg-twice-weekly
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan bg-daily-plan
   #:instruction 'decide-bg-twice-weekly
   #:predicate (lambda (i-list) #t)))

(define-effectuation
  effectuate-bg-twice-weekly-post-nutrition-control
  #f
  bg-twice-weekly-control
  (target-schedule
   #:plan bg-twice-weekly-plan
   #:instruction 'decide-bg-twice-weekly-post-nutrition
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan change-nutrition-plan
   #:instruction 'decide-bg-twice-weekly-post-nutrition
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan start-insulin-plan
   #:instruction 'decide-bg-twice-weekly-post-nutrition
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan bg-daily-plan
   #:instruction 'decide-bg-twice-weekly-post-nutrition
   #:predicate (lambda (i-list) #t)))

(define-effectuation
  effectuate-bg-twice-weekly-post-insulin-control
  #f
  bg-twice-weekly-control
  (target-schedule
   #:plan bg-twice-weekly-plan
   #:instruction 'decide-bg-twice-weekly-post-insulin
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan start-insulin-plan
   #:instruction 'decide-bg-twice-weekly-post-insulin
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan bg-daily-plan
   #:instruction 'decide-bg-twice-weekly-post-insulin
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-bg-twice-weekly-control
;   (list (generate-list
;          bg-twice-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          change-nutrition-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          start-insulin-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          bg-daily-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-bg-daily-control is responsible for effectuating the decide-bg-
; daily process (and its variants).
(define-effectuation
  effectuate-bg-daily-control
  #f
  bg-daily-control
  (target-schedule
   #:plan bg-twice-weekly-plan
   #:instruction 'decide-bg-daily
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan bg-daily-plan
   #:instruction 'decide-bg-daily
   #:predicate (lambda (i-list) #t)))

(define-effectuation
  effectuate-bg-daily-post-nutrition-control
  #f
  bg-daily-control
  (target-schedule
   #:plan bg-twice-weekly-plan
   #:instruction 'decide-bg-daily-post-nutrition
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan bg-daily-plan
   #:instruction 'decide-bg-daily-post-nutrition
   #:predicate (lambda (i-list) #t)))

(define-effectuation
  effectuate-bg-daily-post-insulin-control
  #f
  bg-daily-control  
  (target-schedule
   #:plan bg-twice-weekly-plan
   #:instruction 'decide-bg-daily-post-insulin
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan bg-daily-plan
   #:instruction 'decide-bg-daily-post-insulin
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-bg-daily-control
;   (list (generate-list
;          bg-twice-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          bg-daily-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-monitor-uk-control is responsible for effectuating changes to the
; monitoring of urinary ketones (uk).
(define-effectuation
  effectuate-monitor-uk-control
  #f
  monitor-uk-control
  (target-schedule
   #:plan uk-twice-weekly-plan
   #:instruction 'monitor-urinary-ketones
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan uk-daily-plan
   #:instruction 'monitor-urinary-ketones
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-monitor-uk-control
;   (list (generate-list
;          uk-twice-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          uk-daily-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-uk-dinner-increase is responsible for effectuating the 'decide-uk-
; dinner-increase process.
(define-effectuation
  effectuate-uk-dinner-increase
  #f
  uk-dinner-increase-control
  (target-schedule
   #:plan uk-twice-weekly-plan
   #:instruction 'decide-uk-dinner-increase
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan uk-daily-plan
   #:instruction 'decide-uk-dinner-increase
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan increase-dinner-intake-plan
   #:instruction 'decide-uk-dinner-increase
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-uk-dinner-increase
;   (list (generate-list
;          uk-twice-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          uk-daily-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          increase-dinner-intake-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-uk-twice-weekly-control is responsible for effectuating the
; 'decide-uk-twice-weekly process and its variants.
(define-effectuation
  effectuate-uk-twice-weekly-control
  #f
  uk-twice-weekly-control
  (target-schedule
   #:plan uk-twice-weekly-plan
   #:instruction 'decide-uk-twice-weekly
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan uk-daily-plan
   #:instruction 'decide-uk-twice-weekly
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan increase-dinner-intake-plan
   #:instruction 'decide-uk-twice-weekly
   #:predicate (lambda (i-list) #t)))

(define-effectuation
  effectuate-uk-twice-weekly-post-dinner-control
  #f
  uk-twice-weekly-control
  (target-schedule
   #:plan uk-twice-weekly-plan
   #:instruction 'decide-uk-twice-weekly-post-dinner
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan uk-daily-plan
   #:instruction 'decide-uk-twice-weekly-post-dinner
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan increase-dinner-intake-plan
   #:instruction 'decide-uk-twice-weekly-post-dinner
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-uk-twice-weekly-control
;   (list (generate-list
;          uk-twice-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          uk-daily-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          increase-dinner-intake-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-uk-daily-control is responsible for effectuating the 'decide-uk
; daily process and its variants.
(define-effectuation
  effectuate-uk-daily-control
  #f
  uk-daily-control
  (target-schedule
   #:plan uk-twice-weekly-plan
   #:instruction 'decide-uk-daily
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan uk-daily-plan
   #:instruction 'decide-uk-daily
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan increase-dinner-intake-plan
   #:instruction 'decide-uk-daily
   #:predicate (lambda (i-list) #t)))

(define-effectuation
  effectuate-uk-daily-post-dinner-control
  #f
  uk-daily-control
  (target-schedule
   #:plan uk-twice-weekly-plan
   #:instruction 'decide-uk-daily-post-dinner
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan uk-daily-plan
   #:instruction 'decide-uk-daily-post-dinner
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-uk-daily-control
;   (list (generate-list
;          uk-twice-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          uk-daily-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          increase-dinner-intake-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-monitor-systolic-bp-control is responsible for effectuating changes
; to the monitoring of systolic blood pressure (bp).
(define-effectuation
  effectuate-monitor-systolic-bp-control
  #f
  monitor-systolic-bp-control
  (target-schedule
   #:plan bp-once-weekly-plan
   #:instruction 'monitor-systolic-blood-pressure
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan bp-twice-weekly-plan
   #:instruction 'monitor-systolic-blood-pressure
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan chronic-hypertension-plan
   #:instruction 'monitor-systolic-blood-pressure
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan gestational-hypertension-plan
   #:instruction 'monitor-systolic-blood-pressure
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan gestational-weekly-plan
   #:instruction 'monitor-systolic-blood-pressure
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan gestational-hours-plan
   #:instruction 'monitor-systolic-blood-pressure
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-monitor-systolic-bp-control
;   (list (generate-list
;          bp-once-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          bp-twice-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          chronic-hypertension-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          gestational-hypertension-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          gestational-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          gestational-hours-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-monitor-diastolic-bp-control is responsible for effectuating changes
; to the monitoring of diastolic blood pressure (bp).
(define-effectuation
  effectuate-monitor-diastolic-bp-control
  #f
  monitor-diastolic-bp-control
  (target-schedule
   #:plan bp-once-weekly-plan
   #:instruction 'monitor-diastolic-blood-pressure
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan bp-twice-weekly-plan
   #:instruction 'monitor-diastolic-blood-pressure
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan chronic-hypertension-plan
   #:instruction 'monitor-diastolic-blood-pressure
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan gestational-hypertension-plan
   #:instruction 'monitor-diastolic-blood-pressure
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan gestational-weekly-plan
   #:instruction 'monitor-diastolic-blood-pressure
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan gestational-hours-plan
   #:instruction 'monitor-diastolic-blood-pressure
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-monitor-diastolic-bp-control
;   (list (generate-list
;          bp-once-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          bp-twice-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          chronic-hypertension-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          gestational-hypertension-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          gestational-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          gestational-hours-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-bp-once-weekly-control is responsible for effectuating the
; decide-bp-once-weekly process.
(define-effectuation
  effectuate-bp-once-weekly-control
  #f
  bp-once-weekly-control
  (target-schedule
   #:plan bp-once-weekly-plan
   #:instruction 'decide-bp-once-weekly
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan bp-twice-weekly-plan
   #:instruction 'decide-bp-once-weekly
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan chronic-hypertension-plan
   #:instruction 'decide-bp-once-weekly
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan gestational-hypertension-plan
   #:instruction 'decide-bp-once-weekly
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-bp-once-weekly-control
;   (list (generate-list
;          bp-once-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          bp-twice-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          chronic-hypertension-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          gestational-hypertension-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-bp-twice-weekly-control is responsible for effectuating the
; decide-bp-twice-weekly process.
(define-effectuation
  effectuate-bp-twice-weekly-control
  #f
  bp-twice-weekly-control
  (target-schedule
   #:plan bp-once-weekly-plan
   #:instruction 'decide-bp-twice-weekly
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan bp-twice-weekly-plan
   #:instruction 'decide-bp-twice-weekly
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan chronic-hypertension-plan
   #:instruction 'decide-bp-twice-weekly
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan gestational-hypertension-plan
   #:instruction 'decide-bp-twice-weekly
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-bp-twice-weekly-control
;   (list (generate-list
;          bp-once-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          bp-twice-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          chronic-hypertension-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          gestational-hypertension-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-bp-chronic-control is responsible for effectuating the decide-bp-
; chronic process.
(define-effectuation
  effectuate-bp-chronic-control
  #f
  bp-chronic-control
  (target-schedule
   #:plan chronic-hypertension-plan
   #:instruction 'decide-bp-chronic
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-bp-chronic-control
;   (list (generate-list
;          chronic-hypertension-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-bp-gestational-control is responsible for effectuating the decide-
; bp-gestational process and its variants.
(define-effectuation
  effectuate-bp-gestational-control
  #f
  bp-gestational-control
  (target-schedule
   #:plan gestational-hypertension-plan
   #:instruction 'decide-bp-gestational
   #:predicate (lambda (i-list) #t)))

(define-effectuation
  effectuate-bp-two-days-gestational-control
  #f
  bp-gestational-control
  (target-schedule
   #:plan gestational-hypertension-plan
   #:instruction 'decide-bp-two-days-gestational
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan gestational-weekly-plan
   #:instruction 'decide-bp-two-days-gestational
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-bp-gestational-control
;   (list (generate-list
;          gestational-hypertension-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          gestational-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-bp-once-weekly-gestational-control is responsible for effectuating
; the 'decide-bp-once-weekly-gestational process.
(define-effectuation
  effectuate-bp-once-weekly-gestational-control
  #f
  bp-once-weekly-gestational-control
  (target-schedule
   #:plan gestational-hypertension-plan
   #:instruction 'decide-bp-once-weekly-gestational
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan gestational-weekly-plan
   #:instruction 'decide-bp-once-weekly-gestational
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan gestational-hours-plan
   #:instruction 'decide-bp-once-weekly-gestational
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-bp-once-weekly-gestational-control
;   (list (generate-list
;          gestational-hypertension-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          gestational-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          gestational-hours-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))

; effectuate-bp-hourly-gestational-control is responsible for effectuating the
; 'decide-bp-hourly-gestational process.
(define-effectuation
  effectuate-bp-hourly-gestational-control
  #f
  bp-hourly-gestational-control
  (target-schedule
   #:plan gestational-hypertension-plan
   #:instruction 'decide-bp-hourly-gestational
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan gestational-weekly-plan
   #:instruction 'decide-bp-hourly-gestational
   #:predicate (lambda (i-list) #t))
  (target-schedule
   #:plan gestational-hours-plan
   #:instruction 'decide-bp-hourly-gestational
   #:predicate (lambda (i-list) #t)))

;(verify-process
;   effectuate-bp-hourly-gestational-control
;   (list (generate-list
;          gestational-hypertension-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          gestational-weekly-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1)
;         (generate-list
;          gestational-hours-plan
;          (datetime 2019 12 7 0 0 0)
;          (datetime 2019 12 7 24 0 0)
;          1))
;   (get-datetime (datetime 2019 12 7 20 0 0) (datetime 2019 12 7 20 0 0)))
