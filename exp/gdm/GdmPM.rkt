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
         "../../rpm/DecisionProcess.rkt")

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
     (if (andmap
          (lambda (bg)
            (not (abnormal-bg? (post-prandial-bg? bg meal-list) bg)))
          bg-list)
         (glycemic-control-value-space 'good)
         (void)))))
  ((duration 7 0 0 0)
   (lambda (d-list)
     (let* ([meal-list (filter (lambda (d) (meal-event? d)) d-list)]
            [carb-list (filter (lambda (d) (carbohydrate-intake? d)) d-list)]
            [very-abnormal-bg-list
             (filter (lambda (d) (and (blood-glucose? d)
                                      (very-abnormal-bg? (post-prandial-bg? d meal-list) d)))
                     d-list)])
       (if (>= (length very-abnormal-bg-list) 2)
           (glycemic-control-value-space 'very-poor)
           (void)))))
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
       (if (>= (length abnormal-bg-dt) 2)
           (glycemic-control-value-space 'non-related-poor)
           (void)))))
  ((duration 7 0 0 0)
   (lambda (d-list)
     (let* ([meal-list (filter (lambda (d) (meal-event? d)) d-list)]
            [carb-list (filter (lambda (d) (carbohydrate-intake? d)) d-list)]
            [abnormal-bg-list
             (filter (lambda (d) (and (blood-glucose? d)
                                      (abnormal-bg? (post-prandial-bg? d meal-list) d)))
                     d-list)])
       (if (and (>= (length abnormal-bg-list) 2)
                (andmap (lambda (bg) (compliant-bg? bg carb-list))
                        abnormal-bg-list))
           (glycemic-control-value-space 'meal-compliant-poor)
           (void)))))
  ((duration 7 0 0 0)
   (lambda (d-list)
     (let* ([meal-list (filter (lambda (d) (meal-event? d)) d-list)]
            [carb-list (filter (lambda (d) (carbohydrate-intake? d)) d-list)]
            [abnormal-bg-list
             (filter (lambda (d) (and (blood-glucose? d)
                                      (abnormal-bg? (post-prandial-bg? d meal-list) d)))
                     d-list)])
       (if (and (>= (length abnormal-bg-list) 2)
                (andmap (lambda (bg) (not (compliant-bg? bg carb-list)))
                        abnormal-bg-list))
           (glycemic-control-value-space 'meal-incompliant-poor)
           (void)))))
  ((duration 0 1 0 0)
   (lambda (d-list)
     (let* ([bg-list (filter (lambda (d) (blood-glucose? d)) d-list)]
            [meal-list (filter (lambda (d) (meal-event? d)) d-list)])
     (if (findf
          (lambda (bg)
            (abnormal-bg? (post-prandial-bg? bg meal-list) bg))
          bg-list)
         (glycemic-control-value-space 'poor)
         (void))))))

;(verify-analysis
;   analyse-blood-glucose
;   (list (observation-generator
;          get-blood-glucose
;          (datetime 2019 12 2 7 0 0)
;          (datetime 2019 12 3 24 0 0)
;          (duration 0 12 0 0))
;         (observation-generator
;          get-meal-event
;          (datetime 2019 12 2 6 0 0)
;          (datetime 2019 12 3 24 0 0)
;          (duration 0 12 0 0))
;         (observation-generator
;          get-carbohydrate-intake
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
     (if (andmap (lambda (d) (enum<? (observed-property-value d)
                                     (urinary-ketone-value-space '-/+)))
                 d-list)
         (ketonuria-value-space 'negative)
         (void))))
  ((duration 7 0 0 0)
   (lambda (d-list)
     (if (> (rosette-count
             (lambda (d) (enum>? (observed-property-value d)
                                 (urinary-ketone-value-space '-/+)))
             d-list)
            2)
         (ketonuria-value-space 'positive)
         (void)))))

;(verify-analysis
;   analyse-urinary-ketone
;   (list (observation-generator
;          get-urinary-ketone
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
     (if (>= (rosette-count
              (lambda (d) (enum<? (observed-property-value d)
                                  (carbohydrate-intake-value-space '-/+)))
              d-list)
             1)
         (carbohydrates-value-space 'insufficient)
         (void))))
  ((duration 7 0 0 0)
   (lambda (d-list)
     (if (>= (rosette-count
              (lambda (d) (not (eq? (observed-property-value d)
                                    (carbohydrate-intake-value-space '-/+))))
              d-list)
             2)
         (carbohydrates-value-space 'non-compliant)
         (void)))))

;(verify-analysis
;   analyse-carbohydrates-intake
;   (list (observation-generator
;          get-carbohydrate-intake
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
     (if (andmap
          (lambda (d) (or (and (systolic-blood-pressure? d)
                               (dim<? (observed-property-value d)
                                      (dimensioned 140 'mmHg)))
                          (and (diastolic-blood-pressure? d)
                               (dim<? (observed-property-value d)
                                      (dimensioned 90 'mmHg)))))
          d-list)
         (hypertension-value-space 'normal)
         (void))))
  ((duration 0 6 0 0)
   (lambda (d-list)
     (if (>= (rosette-count
              (lambda (d) (or (and (systolic-blood-pressure? d)
                                   (dim>=? (observed-property-value d)
                                           (dimensioned 140 'mmHg)))
                              (and (diastolic-blood-pressure? d)
                                   (dim>=? (observed-property-value d)
                                           (dimensioned 90 'mmHg)))))
              d-list)
             2)
         (hypertension-value-space 'sustained-high)
         (void))))
  ((duration 0 0 0 0)
   (lambda (d-list)
     (if (findf (lambda (d) (or (and (systolic-blood-pressure? d)
                                     (dim>=? (observed-property-value d)
                                             (dimensioned 160 'mmHg)))
                                (and (diastolic-blood-pressure? d)
                                     (dim>=? (observed-property-value d)
                                             (dimensioned 110 'mmHg)))))
                d-list)
         (hypertension-value-space 'extremely-high)
         (void))))
  ((duration 0 0 0 0)
   (lambda (d-list)
     (if (findf (lambda (d) (or (and (systolic-blood-pressure? d)
                                     (dim>=? (observed-property-value d)
                                             (dimensioned 150 'mmHg)))
                                (and (diastolic-blood-pressure? d)
                                     (dim>=? (observed-property-value d)
                                             (dimensioned 100 'mmHg)))))
                d-list)
         (hypertension-value-space 'very-high)
         (void))))
  ((duration 0 0 0 0)
   (lambda (d-list)
     (if (findf (lambda (d) (or (and (systolic-blood-pressure? d)
                                     (dim>=? (observed-property-value d)
                                             (dimensioned 140 'mmHg)))
                                (and (diastolic-blood-pressure? d)
                                     (dim>=? (observed-property-value d)
                                             (dimensioned 90 'mmHg)))))
                d-list)
         (hypertension-value-space 'high)
         (void)))))

;(verify-analysis
;   analyse-blood-pressure
;   (list (observation-generator
;          get-systolic-blood-pressure
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2)
;         (observation-generator
;          get-diastolic-blood-pressure
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
                      #:offset (duration 0 0 0 0)
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
                      #:offset (duration 7 0 0 0)
                      #:pattern (duration 0 0 0 0)
                      #:interval #t)
                     #t))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d) (and (glycemic-control? d)
                       (eq? (abstraction-value d)
                            (glycemic-control-value-space 'good))))
      d-list))))

;(verify-decision
;   decide-bg-twice-weekly
;   (list (abstraction-generator
;          get-glycemic-control
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
                      #:offset (duration 0 0 0 0)
                      #:pattern
                      (duration 0 7 0 0) (duration 0 9 0 0)
                      (duration 0 14 0 0) (duration 0 20 0 0)
                      #:interval (duration 1 0 0 0))
                     #t)
   (control-template 'decide-bg-nutrition-change
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:offset (duration 7 0 0 0)
                      #:pattern (duration 0 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-insulin
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:offset (duration 7 0 0 0)
                      #:pattern (duration 0 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-twice-weekly
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:offset (duration 7 0 0 0)
                      #:pattern (duration 0 0 0 0)
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

;(verify-decision
;   decide-bg-daily
;   (list (abstraction-generator
;          get-glycemic-control
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
                      #:offset (duration 7 0 0 0)
                      #:pattern (duration 0 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-insulin-adjust
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:offset (duration 7 0 0 0)
                      #:pattern (duration 0 0 0 0)
                      #:interval #t)
                     #t)
   (culminating-action-template 'administer-insulin-action
                                (relative-schedule
                                 #:rounding (duration 1 0 0 0)
                                 #:offset (duration 0 0 0 0)
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

;(verify-decision
;   decide-bg-insulin
;   (list (abstraction-generator
;          get-glycemic-control
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2)
;         (abstraction-generator
;          get-ketonuria
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
                      #:offset (duration 0 0 0 0)
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
                      #:offset (duration 7 0 0 0)
                      #:pattern (duration 0 0 0 0)
                      #:interval #t)
                     #t))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d) (and (glycemic-control? d)
                       (eq? (abstraction-value d)
                            (glycemic-control-value-space 'good))))
      d-list))))

;(verify-decision
;   decide-bg-twice-weekly-post-insulin
;   (list (abstraction-generator
;          get-glycemic-control
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
                      #:offset (duration 0 0 0 0)
                      #:pattern
                      (duration 0 7 0 0) (duration 0 9 0 0)
                      (duration 0 14 0 0) (duration 0 20 0 0)
                      #:interval (duration 1 0 0 0))
                     #t)
   (control-template 'decide-bg-insulin-adjust
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:offset (duration 7 0 0 0)
                      #:pattern (duration 0 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-twice-weekly-post-insulin
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:offset (duration 7 0 0 0)
                      #:pattern (duration 0 0 0 0)
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

;(verify-decision
;   decide-bg-daily-post-insulin
;   (list (abstraction-generator
;          get-glycemic-control
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
                                 #:offset (duration 0 0 0 0)
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

;(verify-decision
;   decide-bg-insulin-adjust
;   (list (abstraction-generator
;          get-glycemic-control
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
                      #:offset (duration 7 0 0 0)
                      #:pattern (duration 0 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-insulin-post-nutrition
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:offset (duration 7 0 0 0)
                      #:pattern (duration 0 0 0 0)
                      #:interval #t)
                     #t)
   (culminating-action-template 'change-diet-action
                                (relative-schedule
                                 #:rounding (duration 1 0 0 0)
                                 #:offset (duration 0 0 0 0)
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

;(verify-decision
;   decide-bg-nutrition-change
;   (list (abstraction-generator
;          get-glycemic-control
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2)
;         (abstraction-generator
;          get-ketonuria
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
                      #:offset (duration 0 0 0 0)
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
                      #:offset (duration 7 0 0 0)
                      #:pattern (duration 0 0 0 0)
                      #:interval #t)
                     #t))
  (#:criteria
   (lambda (d-list)
     (findf
      (lambda (d) (and (glycemic-control? d)
                       (eq? (abstraction-value d)
                            (glycemic-control-value-space 'good))))
      d-list))))

;(verify-decision
;   decide-bg-twice-weekly-post-nutrition
;   (list (abstraction-generator
;          get-glycemic-control
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
                      #:offset (duration 0 0 0 0)
                      #:pattern
                      (duration 0 7 0 0) (duration 0 9 0 0)
                      (duration 0 14 0 0) (duration 0 20 0 0)
                      #:interval (duration 1 0 0 0))
                     #t)
   (control-template 'decide-bg-insulin-post-nutrition
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:offset (duration 7 0 0 0)
                      #:pattern (duration 0 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-twice-weekly-post-nutrition
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:offset (duration 7 0 0 0)
                      #:pattern (duration 0 0 0 0)
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

;(verify-decision
;   decide-bg-daily-post-nutrition
;   (list (abstraction-generator
;          get-glycemic-control
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
                      #:offset (duration 7 0 0 0)
                      #:pattern (duration 0 0 0 0)
                      #:interval #t)
                     #t)
   (control-template 'decide-bg-insulin-adjust
                     (relative-schedule
                      #:rounding (duration 0 0 0 0)
                      #:offset (duration 7 0 0 0)
                      #:pattern (duration 0 0 0 0)
                      #:interval #t)
                     #t)
   (culminating-action-template 'administer-insulin-action
                                (relative-schedule
                                 #:rounding (duration 1 0 0 0)
                                 #:offset (duration 0 0 0 0)
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

;(verify-decision
;   decide-bg-insulin-post-nutrition
;   (list (abstraction-generator
;          get-glycemic-control
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 15 24 0 0)
;          2))
;   (get-datetime (datetime 2019 12 1 0 0 0) (datetime 2019 12 15 24 0 0)))
