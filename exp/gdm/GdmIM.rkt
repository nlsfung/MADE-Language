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

; Only one measurement type is needed for the GDM workflow, namely bodily
; acceleration, which is for computing physical activity levels.
(define-measurement body-acceleration 'm/s2)

; Eight different types of observations can be identified from the GDM 
; workflows, namely for capturing:
; 1) Blood glucose levels
; 2) Urinary ketone levels (referred to as ketonuria in the guideline but 
;    renamed here avoid confusion with the ketonuria abstraction)
; 3) Events of meals (which is implicitly required in the guideline to detect
;    abnormal blood glucose measurements)
; 4) Carbohydrates intake of a single meal (which is qualified relative to 
;    the recommended intake levels)
; 5) Exercise intensity (in terms of METs)
; 6) Systolic blood pressure
; 7) Diastolic blood pressure (which must be distinguished from systolic BP
;    as each observation can only be associated with one value).
; 8) Events of conception, i.e. becoming pregnant (which is required to 
;    determine the gestational age of the patient)
(define-observation blood-glucose dimensioned 'mg/dL)

(define-enumerated urinary-ketone-value-space '-- '- '-/+ '+ '++)
(define-observation urinary-ketone urinary-ketone-value-space)

(define-observation meal-event 'event)

(define-enumerated carbohydrate-intake-value-space '-- '- '-/+ '+ '++)
(define-observation carbohydrate-intake carbohydrate-intake-value-space)

(define-observation exercise-intensity proportion)

(define-observation systolic-blood-pressure dimensioned 'mmHg)

(define-observation diastolic-blood-pressure dimensioned 'mmHg)

(define-observation conception-event 'event)

; Nine different types of abstractions can be identified from the GDM 
; workflows, namely for capturing:
; 1) Degree of glycemic control
; 2) Severity of ketonuria in the patient.
; 3) Degree of non-compliance to the recommended carbohydrates intake.
; 4) Exercise compliance in the resting context.
; 5) Exercise compliance in the active context.
; 6) Degree of hypertension.
; 7) Target organ damage.
; 8) Proteinuria.
(define-nominal glycemic-control-value-space
  'good 'poor 'meal-compliant-poor 'meal-incompliant-poor
  'non-related-poor 'very-poor)
(define-abstraction glycemic-control glycemic-control-value-space)

(define-enumerated ketonuria-value-space 'negative 'positive)
(define-abstraction ketonuria ketonuria-value-space)

(define-enumerated carbohydrates-value-space 'insufficient 'non-compliant)
(define-abstraction carbohydrates-compliance carbohydrates-value-space)

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
;    International Units (of insulin). 
; 2) Changes in the diet (i.e. nutritional prescription), which is assumed 
;    to be measured in grams (of carbohydrates).
; 3) Increases in carbohydrates intake at dinner (Note: It is assumed that
;    this instruction can occur in conjunction with instruction 2).
(define-action-instruction administer-insulin-action culminating dimensioned 'IU)

(define-action-instruction change-diet-action culminating dimensioned 'g)

(define-action-instruction change-dinner-action culminating dimensioned 'g)

; 17 different types of control instructions can be identified from 
; the GDM workflows, namely for controlling:
; 1) Blood glucose (BG) measurements.
; 2) The workflow associated with changing nutritional prescription.
; 3) The workflow associated with starting or changing insulin therapy.
; 4) The workflow associated with monitoring BG for two days each week.
; 5) The workflow associated with monitoring BG every day.
; 6) Urinary ketone measurements.
; 7) The workflow associated with increasing dincarbohydrates intake at dinner.
; 8) The workflow associated with monitoring urinary ketones twice weekly.
; 9) The workflow associated with monitoring urinary ketones daily.
; 10) Systolic blood pressure measurements.
; 11) Diastolic blood pressure measurements.
; 11) The workflow associated with monitoring blood pressure once each week.
; 12) The workflow associated with monitoring blood pressure twice each week.
; 13) The workflow associated with chronic hypertension.
; 14) The workflow associated with gestational hypertension (every 2 days).
; 15) The workflow associated with gestational hypertension (every week).
; 16) The workflow associated with deciding to monitor BP every few hours.
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

; The twenty three different instruction archetypes constitute 14 different
; types of action plans, namely for:
; 1) Monitoring blood glucose for two days every week.
; 2) Adjusting the prescribed insulin therapy.
; 3) Changing the nutritional prescription.
; 4) Starting insulin therapy.
; 5) Monitoring blood glucose daily.
; 6) Monitoring urinary ketones twice a week.
; 7) Changing the carbohydrates intake at dinner (or before bed-time)
; 8) Monitoring urinary ketones daily.
; 9) Monitoring blood pressure once a week (for no hypertension).
; 10) Monitoring blood pressure in the context of chronic hypertension.
; 11) Monitoring blood pressure every two days for gestational hypertension.
; 12) Monitoring blood pressure twice a week (for no hypertension).
; 13) Monitoring blood pressure once a week (for gestational hypertension).
; 14) Monitoring blood pressure every few hours in the context of 
;     gestational hypertension. 
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
