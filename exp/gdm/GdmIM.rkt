#lang rosette/safe

(require "../../lang/VerifySyntax.rkt"
         "../../lang/IMSyntax.rkt"
         "../../lang/NomEnumSyntax.rkt"
         "../../rim/BasicDataTypes.rkt")

(require "../../rpm/MonitoringProcess.rkt"
         "../../rim/TemporalDataTypes.rkt")

(provide (all-defined-out))

; This file contains the specification of the information model for the clinical
; guideline for gestational diabetes mellitus (GDM).

; No measurement types are specified by the guideline.

; Eight different types of observations can be identified from the GDM 
; workflows, namely for capturing:
; 1) Blood glucose levels (blood-glucocse).
; 2) Urinary ketone levels (urinary-ketone) (referred to as ketonuria in the
;    guideline but renamed here avoid confusion with the ketonuria abstraction).
; 3) Events of meals (meal-event) (which is implicitly required in the
;    guideline to detect abnormal blood glucose measurements).
; 4) Carbohydrates intake (carbohydrates-compliance) of a single meal (which
;    is qualified relative to the recommended intake levels).
; 5) Exercise intensity (exercise-intensity) (in terms of METs).
; 6) Systolic blood pressure (systolic-blood-pressure).
; 7) Diastolic blood pressure (diastolic-blood-pressure) (which must be
;    distinguished from systolic BP as each observation can only be
;    associated with one value).
; 8) Events of conception, i.e. becoming pregnant (conception-event) (which is
;    required to determine the gestational age of the patient).
(define-observation blood-glucose dimensioned 'mg/dL)

(define-observation urinary-ketone enumerated '-- '- '-/+ '+ '++)

(define-observation meal-event #:event)

(define-observation carbohydrate-intake enumerated '-- '- '-/+ '+ '++)

(define-observation exercise-intensity proportion)

(define-observation systolic-blood-pressure dimensioned 'mmHg)

(define-observation diastolic-blood-pressure dimensioned 'mmHg)

(define-observation conception-event #:event)

; Eight different types of abstractions can be identified from the GDM 
; workflows, namely for capturing:
; 1) Degree of glycemic control (glycemic-control).
; 2) Severity of ketonuria in the patient (ketonuria).
; 3) Degree of non-compliance to the recommended carbohydrates intake
;    (carbohydrates-compliance).
; 4) Exercise compliance in the resting context (exercise-compliance-resting).
; 5) Exercise compliance in the active context (exercise-compliance-active).
; 6) Degree of hypertension (hypertension).
; 7) Target organ damage (target-organ-damage).
; 8) Proteinuria (proteinuria).
(define-abstraction glycemic-control nominal 'good 'poor 'meal-compliant-poor
  'meal-incompliant-poor 'non-related-poor 'very-poor)

(define-abstraction ketonuria enumerated 'negative 'positive)

(define-abstraction carbohydrates-compliance enumerated 'insufficient 'non-compliant)

(define-abstraction exercise-compliance-resting bool)

(define-abstraction exercise-compliance-active bool)

(define-nominal hypertension-value-space
  'high 'very-high 'normal 'sustained-high 'extremely-high)
(define-abstraction hypertension hypertension-value-space)

(define-abstraction target-organ-damage bool)

(define-abstraction proteinuria bool)

; Three different types of action instructions can be identified from the 
; GDM workflows, namely for effectuating:
; 1) Administrations of insulin, which is assumed to be measured in 
;    International Units of insulin (administer-insulin-action). 
; 2) Changes in the diet (i.e. nutritional prescription), which is assumed 
;    to be measured in grams of carbohydrates (change-diet-action).
; 3) Increases in carbohydrates intake at dinner (change-dinner-action)
;    (Note: It is assumed that this instruction can occur in conjunction
;    with instruction 2).
(define-action-instruction administer-insulin-action #:culminating dimensioned 'IU)

(define-action-instruction change-diet-action #:culminating dimensioned 'g)

(define-action-instruction change-dinner-action #:culminating dimensioned 'g)

; 17 different types of control instructions can be identified from 
; the GDM workflows, namely for controlling:
; 1) Blood glucose (BG) measurements (monitor-bg-control).
; 2) The workflow associated with changing nutritional prescription
;    (bg-nutrition-change-control).
; 3) The workflow associated with starting or changing insulin therapy
;    (bg-insulin-control).
; 4) The workflow associated with monitoring BG for two days each week
;    (bg-twice-weekly-control).
; 5) The workflow associated with monitoring BG every day
;    (bg-daily-control).
; 6) Urinary ketone measurements (monitor-uk-control).
; 7) The workflow associated with increasing dincarbohydrates intake at dinner
;    (uk-dinner-increase-control).
; 8) The workflow associated with monitoring urinary ketones twice weekly
;    (uk-twice-weekly-control).
; 9) The workflow associated with monitoring urinary ketones daily
;    (uk-daily-control).
; 10) Systolic blood pressure measurements (monitor-systolic-bp-control).
; 11) Diastolic blood pressure measurements (monitor-diastolic-bp-control).
; 12) The workflow associated with monitoring blood pressure once each week
;     (bp-once-weekly-control).
; 13) The workflow associated with monitoring blood pressure twice each week
;     (bp-twice-weekly-control).
; 14) The workflow associated with chronic hypertension
;     (bp-chronic-control).
; 15) The workflow associated with gestational hypertension (every 2 days)
;     (bp-gestational-control).
; 16) The workflow associated with gestational hypertension (every week)
;     (bp-once-weekly-gestational-control).
; 17) The workflow associated with deciding to monitor BP every few hours
;     (bp-hourly-gestational-control).
(define-control-instruction monitor-bg-control
  'monitor-blood-glucose)

(define-control-instruction bg-nutrition-change-control
  'decide-bg-nutrition-change)

(define-control-instruction bg-insulin-control
  'decide-bg-insulin
  'decide-bg-insulin-post-nutrition
  'decide-bg-insulin-adjust)

(define-control-instruction bg-twice-weekly-control
  'decide-bg-twice-weekly
  'decide-bg-twice-weekly-post-insulin
  'decide-bg-twice-weekly-post-nutrition)

(define-control-instruction bg-daily-control
  'decide-bg-daily
  'decide-bg-daily-post-insulin
  'decide-bg-daily-post-nutrition)

(define-control-instruction monitor-uk-control
  'monitor-urinary-ketones)

(define-control-instruction uk-dinner-increase-control
  'decide-uk-dinner-increase)

(define-control-instruction uk-twice-weekly-control
  'decide-uk-twice-weekly
  'decide-uk-twice-weekly-post-dinner)

(define-control-instruction uk-daily-control
  'decide-uk-daily
  'decide-uk-daily-post-dinner)

(define-control-instruction monitor-systolic-bp-control
  'monitor-systolic-blood-pressure)

(define-control-instruction monitor-diastolic-bp-control
  'monitor-diastolic-blood-pressure)

(define-control-instruction bp-once-weekly-control
  'decide-bp-once-weekly)

(define-control-instruction bp-twice-weekly-control
  'decide-bp-twice-weekly)

(define-control-instruction bp-chronic-control
  'decide-bp-chronic)

(define-control-instruction bp-gestational-control
  'decide-bp-gestational
  'decide-bp-two-days-gestational)

(define-control-instruction bp-once-weekly-gestational-control
  'decide-bp-once-weekly-gestational)

(define-control-instruction bp-hourly-gestational-control
  'decide-bp-hourly-gestational)

; The twenty different instruction archetypes constitute 14 different
; types of action plans, namely for:
; 1) Monitoring blood glucose for two days every week (bg-twice-weekly-plan).
; 2) Adjusting the prescribed insulin therapy (adjust-insulin-plan).
; 3) Changing the nutritional prescription (change-nutrition-plan).
; 4) Starting insulin therapy (start-insulin-plan).
; 5) Monitoring blood glucose daily (bg-daily-plan).
; 6) Monitoring urinary ketones twice a week (uk-twice-weekly-plan).
; 7) Changing the carbohydrates intake at dinner (or before bed-time)
;    (increase-dinner-intake-plan)
; 8) Monitoring urinary ketones daily (uk-daily-plan).
; 9) Monitoring blood pressure once a week (for no hypertension)
;    (bp-once-weekly-plan).
; 10) Monitoring blood pressure in the context of chronic hypertension
;     (chronic-hypertension-plan).
; 11) Monitoring blood pressure every two days for gestational hypertension
;     (gestational-hypertension-plan).
; 12) Monitoring blood pressure twice a week (for no hypertension)
;     (bp-twice-weekly-plan).
; 13) Monitoring blood pressure once a week (for gestational hypertension)
;     (gestational-weekly-plan).
; 14) Monitoring blood pressure every few hours in the context of 
;     gestational hypertension (gestational-hours-plan). 
(define-action-plan bg-twice-weekly-plan
  (control 'monitor-blood-glucose
           'decide-bg-nutrition-change
           'decide-bg-insulin
           'decide-bg-insulin-post-nutrition
           'decide-bg-insulin-adjust
           'decide-bg-twice-weekly
           'decide-bg-twice-weekly-post-insulin
           'decide-bg-twice-weekly-post-nutrition
           'decide-bg-daily
           'decide-bg-daily-post-insulin
           'decide-bg-daily-post-nutrition))

(define-action-plan adjust-insulin-plan
  (culminating-action 'administer-insulin-action))

(define-action-plan change-nutrition-plan
  (culminating-action 'change-diet-action)
  (control 'decide-bg-twice-weekly
           'decide-bg-twice-weekly-post-nutrition
           'decide-bg-nutrition-change
           'decide-bg-insulin
           'decide-bg-insulin-post-nutrition))

(define-action-plan start-insulin-plan
  (culminating-action 'administer-insulin-action)
  (control 'decide-bg-nutrition-change
           'decide-bg-twice-weekly
           'decide-bg-twice-weekly-post-insulin
           'decide-bg-twice-weekly-post-nutrition
           'decide-bg-insulin
           'decide-bg-insulin-post-nutrition
           'decide-bg-insulin-adjust))

(define-action-plan bg-daily-plan
  (control 'monitor-blood-glucose
           'decide-bg-nutrition-change
           'decide-bg-insulin
           'decide-bg-insulin-post-nutrition
           'decide-bg-insulin-adjust
           'decide-bg-twice-weekly
           'decide-bg-twice-weekly-post-insulin
           'decide-bg-twice-weekly-post-nutrition
           'decide-bg-daily
           'decide-bg-daily-post-insulin
           'decide-bg-daily-post-nutrition))

(define-action-plan uk-twice-weekly-plan
  (control 'monitor-urinary-ketones
           'decide-uk-dinner-increase
           'decide-uk-twice-weekly
           'decide-uk-twice-weekly-post-dinner
           'decide-uk-daily
           'decide-uk-daily-post-dinner))

(define-action-plan uk-daily-plan
  (control 'monitor-urinary-ketones
           'decide-uk-dinner-increase
           'decide-uk-twice-weekly
           'decide-uk-twice-weekly-post-dinner
           'decide-uk-daily
           'decide-uk-daily-post-dinner))

(define-action-plan increase-dinner-intake-plan
  (culminating-action 'change-dinner-action)
  (control 'decide-uk-dinner-increase
           'decide-uk-twice-weekly
           'decide-uk-daily
           'decide-uk-twice-weekly-post-dinner))

(define-action-plan bp-once-weekly-plan
  (control 'monitor-systolic-blood-pressure
           'monitor-diastolic-blood-pressure
           'decide-bp-once-weekly
           'decide-bp-twice-weekly))

(define-action-plan bp-twice-weekly-plan
  (control 'monitor-systolic-blood-pressure
           'monitor-diastolic-blood-pressure
           'decide-bp-once-weekly
           'decide-bp-twice-weekly))

(define-action-plan chronic-hypertension-plan
  (control 'monitor-systolic-blood-pressure
           'monitor-diastolic-blood-pressure
           'decide-bp-once-weekly
           'decide-bp-twice-weekly
           'decide-bp-chronic))

(define-action-plan gestational-hypertension-plan
  (control 'monitor-systolic-blood-pressure
           'monitor-diastolic-blood-pressure
           'decide-bp-once-weekly
           'decide-bp-twice-weekly
           'decide-bp-gestational
           'decide-bp-once-weekly-gestational
           'decide-bp-hourly-gestational
           'decide-bp-two-days-gestational))

(define-action-plan gestational-weekly-plan
  (control 'monitor-systolic-blood-pressure
           'monitor-diastolic-blood-pressure
           'decide-bp-once-weekly-gestational
           'decide-bp-hourly-gestational
           'decide-bp-two-days-gestational))

(define-action-plan gestational-hours-plan
  (control 'monitor-systolic-blood-pressure
           'monitor-diastolic-blood-pressure
           'decide-bp-once-weekly-gestational
           'decide-bp-hourly-gestational))
