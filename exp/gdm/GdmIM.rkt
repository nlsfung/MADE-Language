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

; Twenty one different types of control instructions can be identified from 
; the GDM workflows, namely for controlling:
; 1) Blood glucose measurements.
; 2) The workflow associated with monitoring BG for two days each week.
; 3) The workflow associated with monitoring BG every day.
; 4) The (sub-)workflow associated with changing nutritional prescription.
; 5) The workflow associated with starting insulin therapy before changing
;    nutritional prescription.
; 6) The workflow associated with adjusting the insulin therapy.
; 7) The workflow associated with starting insulin therapy after changing the
;    nutritional prescription.
; 8) Urinary ketone measurements.
; 9) The workflow associated with monitoring urinary ketones twice weekly.
; 10) The workflow associated with monitoring urinary ketones daily.
; 11) The (sub-)workflow associated with increasing carbohydrates intake.
; 12) The workflow associated with occurrences of ketonuria after increasing
;     carbohydrates intake at dinner.
; 13) Blood pressure measurements.
; 14) The workflow associated with monitoring blood pressure once each week.
; 15) The workflow associated with monitoring blood pressure twice each week.
; 16) The workflow associated with chronic hypertension.
; 17) The workflow associated with gestational hypertension (every 2 days).
; 18) The workflow associated with measuring blood pressure again.
; 19) The workflow associated with gestational hypertension (every week).
; 20) The workflow associated with starting blood pressure treatment.
; 21) The workflow associated with deciding to monitor BP every few hours.
(define-control-instruction monitor-bg-control 'monitor-blood-glucose)

(define-control-instruction bg-twice-weekly-workflow-control 'bg-twice-weekly-workflow)

(define-control-instruction bg-daily-workflow 'bg-daily-workflow)

(define-control-instruction diet-workflow-control 'diet-workflow)

(define-control-instruction start-insulin-workflow-control 'start-insulin-workflow)

(define-control-instruction adjust-insulin-workflow-control 'adjust-insulin-workflow)

(define-control-instruction post-diet-insulin-workflow-control 'post-diet-insulin-workflow)

(define-control-instruction monitor-urinary-ketones-control 'monitor-urinary-ketones)

(define-control-instruction ktn-twice-weekly-workflow-control 'ktn-twice-weekly-workflow)

(define-control-instruction ktn-daily-workflow-control 'ktn-daily-workflow)

(define-control-instruction dinner-workflow-control 'dinner-workflow)

(define-control-instruction monitor-bp-control 'monitor-blood-pressure)

(define-control-instruction bp-once-weekly-workflow 'bp-once-weekly-workflow)

(define-control-instruction bp-twice-weekly-workflow 'bp-twice-weekly-workflow)

(define-control-instruction normal-bp-workflow-control 'normal-bp-workflow)

(define-control-instruction chronic-bp-workflow-control 'chronic-bp-workflow)

(define-control-instruction gestational-bp-two-days-workflow-control 'gestational-bp-two-days-workflow)

(define-control-instruction bp-repeat-workflow-control 'bp-repeat-control-workflow)

(define-control-instruction gestational-bp-weekly-workflow-control 'gestational-bp-weekly-workflow)

(define-control-instruction gestational-bp-treatment-workflow-control 'gestational-bp-treatment-workflow)

(define-control-instruction gestational-bp-hours-workflow-control 'gestational-bp-hours-workflow)

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
  (control 'monitor-blood-pressure
           'decide-bp-once-weekly
           'decide-bp-twice-weekly))

(define-action-plan bp-twice-weekly-plan
  (control 'monitor-blood-pressure
           'decide-bp-once-weekly
           'decide-bp-twice-weekly))

(define-action-plan chronic-hypertension-plan
  (control 'monitor-blood-pressure
           'decide-bp-once-weekly
           'decide-bp-twice-weekly
           'decide-bp-chronic))

(define-action-plan gestational-hypertension-plan
  (control 'monitor-blood-pressure
           'decide-bp-once-weekly
           'decide-bp-twice-weekly
           'decide-bp-gestational
           'decide-bp-once-weekly-gestational
           'decide-bp-hourly-gestational
           'decide-bp-two-days-gestational))

(define-action-plan gestational-weekly-plan
  (control 'monitor-blood-pressure
           'decide-bp-once-weekly-gestational
           'decide-bp-hourly-gestational
           'decide-bp-two-days-gestational))

(define-action-plan gestational-hours-plan
  (control 'monitor-blood-pressure
           'decide-bp-once-weekly-gestational
           'decide-bp-hourly-gestational))
