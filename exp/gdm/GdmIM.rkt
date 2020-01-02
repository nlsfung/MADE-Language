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
(define-observation carbohydrate-inake carbohydrate-intake-value-space)

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

(define-enumerated ketonuria-value-space'negative 'positive)
(define-abstraction ketonuria ketonuria-value-space)

(define-enumerated carbohydrates-value-space 'insufficient 'non-compliant)
(define-abstraction carbohydrates-compliance carbohydrates-value-space)

(define-abstraction exercise-compliance-resting bool)

(define-abstraction exercise-compliance-active bool)

(define-enumerated hypertension-value-space
  'high 'very-high 'normal 'sustained-high 'extremely-high)
(define-abstraction hypertension-suspicion hypertension-value-space)

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

; The twenty three different instruction archetypes constitute 18 different
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
; 12) Monitoring blood pressure again after 4-6 hours, which is modelled as 
;     a separate parallel workflow.
; 13) Re-activate plan for monitoring blood pressure again, which is only
;     necessary for the workflow to monitor blood pressure every two days in
;     the context of no hypertension.
; 14) Monitoring blood pressure twice a week (for no hypertension).
; 15) Monitoring blood pressure once a week (for gestational hypertension).
; 16) Starting treatment for high blood pressure (gestational hypertension). 
;     Note: Since no details are provided about the treatment, it is assumed
;     that the treatment itself is prescribed by the clinician and that the
;     system should only notify the clinician when treatment may be needed.
;     Thus this plan is only needed to ensure that the clinician will not
;     be notified more than once.
; 17) Monitoring blood pressure every few hours in the context of 
;     gestational hypertension. 
; 18) Switching from monitoring blood pressure once a week in the context of
;     gestational hypertenation.
(define-action-plan bg-twice-weekly-plan
  (control 'monitor-bg-control 'bg-twice-weekly-workflow-control 'bg-daily-workflow-control))

(define-action-plan adjust-insulin-plan (culminating-action 'administer-insulin-action))

(define-action-plan change-nutrition-plan
  (culminating-action 'change-diet-action)
  (control 'diet-workflow-control 'start-insulin-workflow-control 'post-diet-insulin-workflow-control))

(define-action-plan start-insulin-plan
  (culminating-action 'administer-insulin-action)
  (control 'diet-workflow-control 'start-insulin-workflow-control
           'adjust-insulin-workflow-control 'post-diet-insulin-workflow-control))

(define-action-plan bg-daily-plan
  (control 'monitor-bg-control 'bg-twice-weekly-workflow-control 'bg-daily-workflow-control))

(define-action-plan ktn-twice-weekly-plan
  (control 'monitor-urinary-ketones-control 'ktn-twice-weekly-workflow-control 'ktn-daily-workflow-control))

(define-action-plan increse-dinner-intake-plan
  (culminating 'change-dinner-action)
  (control 'dinner-workflow-control))

(define-action-plan ktn-daily-plan
  (control 'monitor-urinary-ketones-control 'ktn-twice-weekly-workflow-control 'ktn-daily-workflow-control))

(define-action-plan bp-weekly-plan
  (control 'monitor-bp-control 'bp-once-weekly-workflow-control 'bp-twice-weekly-workflow-control))

(define-action-plan chronic-hypertension-plan
  (control 'monitor-bp-control 'chronic-bp-workflow-control 'normal-bp-workflow-control))

(define-action-plan gestational-hypertension-plan
  (control 'monitor-bp-control 'gestational-bp-two-days-workflow-control 'normal-bp-workflow-control))

(define-action-plan bp-repeat-plan
  (control 'monitor-bp-control 'bp-repeat-workflow-control))

(define-action-plan bp-reactivate-repeat-plan
  (control 'monitor-bp-control 'bp-repeat-workflow-control))

(define-action-plan bp-twice-weekly-plan
  (control 'monitor-bp-control 'bp-once-weekly-workflow-control
           'bp-twice-weekly-workflow-control 'bp-repeat-workflow-control))

(define-action-plan gestational-weekly-plan
  (control 'monitor-bp-control 'gestational-bp-two-days-workflow-control
           'gestational-bp-weekly-workflow-control))

(define-action-plan gestational-treatment-plan
  (control 'gestational-bp-treatment-workflow-control))

(define-action-plan gestational-hours-plan
  (control 'monitor-bp-control 'gestational-bp-two-days-workflow-control
           'gestational-bp-hours-workflow-control))

(define-action-plan gestational-weekly-two-days-plan
  (control 'monitor-bp-control 'gestational-bp-two-days-workflow-control
           'gestational-bp-weekly-workflow-control))
