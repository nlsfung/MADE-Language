#lang rosette/safe

(require (rename-in rosette/safe [count rosette-count])
         "./GdmIM.rkt"
         "../../lang/PMSyntax.rkt"
         "../../lang/VerifySyntax.rkt"
         "../../rim/BasicDataTypes.rkt"
         "../../rim/TemporalDataTypes.rkt"
         "../../rim/MadeDataStructures.rkt"
         "../../rpm/MadeProcess.rkt"
         "../../rpm/AnalysisProcess.rkt")

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
;          (datetime 2019 12 1 1 0 0)
;          (datetime 2019 12 8 1 0 0)
;          (duration 3 1 0 0))
;         (observation-generator
;          get-meal-event
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 8 0 0 0)
;          (duration 3 1 0 0))
;         (observation-generator
;          get-carbohydrate-intake
;          (datetime 2019 12 1 0 0 0)
;          (datetime 2019 12 8 0 0 0)
;          (duration 3 1 0 0)))
;   (get-datetime (datetime 2019 12 7 3 0 0) (datetime 2019 12 7 3 0 0)))

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