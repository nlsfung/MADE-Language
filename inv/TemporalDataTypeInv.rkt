#lang rosette/safe

(require (only-in "../rim/TemporalDataTypes.rkt"
                  datetime dt+ dt=? dt-
                  duration dur>? dur<?
                  normalized?))

; Symbolic constants for checking properties of date-time manipulation.
(define-symbolic dt-year dt-month dt-day dt-hour dt-minute dt-second integer?)
(define-symbolic dur-day dur-hour dur-minute dur-second integer?)
(define dt-sym (datetime dt-year dt-month dt-day dt-hour dt-minute dt-second))
(define dur-sym (duration dur-day dur-hour dur-minute dur-second))

; Check that date-times can be normalized after adding/subtracting 31 days max.
(define (verify-normalize)
  (verify #:assume (assert (and (normalized? dt-sym)
                                (dur>? dur-sym (duration -31 0 0 0))
                                (dur<? dur-sym (duration  31 0 0 0))))
          #:guarantee (assert (normalized? (dt+ dt-sym dur-sym)))))

; Check that date-time manipulations are reversible.
; Tested with datetime-unwind set to 0.
(define (verify-reversible)
  (verify #:assume (assert (and (= dt-year 2000)
                                (normalized? dt-sym)
                                (dur>? dur-sym (duration -1 0 0 0))
                                (dur<? dur-sym (duration 1 0 0 0))))
          #:guarantee (assert (dt=? dt-sym (dt- (dt+ dt-sym dur-sym) dur-sym)))))