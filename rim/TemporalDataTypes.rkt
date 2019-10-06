#lang rosette/safe

(provide (struct-out datetime) dt=? dt- dt+ dt>? dt<? normalized? dt-between? datetime->number)
(provide (struct-out schedule) on-schedule?)
(provide (struct-out duration) dur- dur+ dur<? dur>? dur=? duration->second)

; This file contains the specification of the 3 primitive temporal data types
; in the MADE RIM (viz. DateTime, Duration and Schedule).

; The datetime-unwind parameter determines if the recursive date-time operations
; should be unwound a finite number of times or not, and if so, the number
; of unwindings. It must take the value #f or a natural number.
; Set to 2 by default.
(define datetime-unwind 2)

; The schedule-unwind parameter determines how many times the on-schedule
; operation should be unwound, if any. 
; Set to 5 by default.
(define schedule-unwind 5)

; Duration contains a day, hour, minute and second. Months (and therefore
; year) are not included as a month can contain a variable number of days.
; Durations support comparison as well as arithmetic operations.
(define-generics dur
  [dur=? dur elem]
  [dur>? dur elem]
  [dur<? dur elem]
  [dur+ dur elem]
  [dur- dur elem]
  [dur* dur elem]
  [dur/ dur elem])

(struct duration (day hour minute second)
  #:transparent
  #:methods gen:dur
  [(define-syntax-rule (compare-with-duration op self dur)
     (op (duration->second self) (duration->second dur)))
   (define (dur=? self dur) (compare-with-duration = self dur))
   (define (dur>? self dur) (compare-with-duration > self dur))
   (define (dur<? self dur) (compare-with-duration < self dur))

   (define-syntax-rule (add/sub-with-duration op self dur)
    (duration (op (duration-day self) (duration-day dur))
              (op (duration-hour self) (duration-hour dur))
              (op (duration-minute self) (duration-minute dur))
              (op (duration-second self) (duration-second dur))))
   (define (dur+ self dur) (add/sub-with-duration + self dur))
   (define (dur- self dur) (add/sub-with-duration - self dur))

   (define-syntax-rule (mul/div-with-duration op self num)
     (duration (op (duration-day self) num)
               (op (duration-hour self) num)
               (op (duration-minute self) num)
               (op (duration-second self) num)))
   (define (dur* self num) (mul/div-with-duration * self num))
   (define (dur/ self num) (mul/div-with-duration / self num))])

; Helper function to convert a duration into seconds for comparison.
(define (duration->second dur)
  (+ (duration-second dur)
     (* 60 (+ (duration-minute dur)
              (* 60 (+ (duration-hour dur)
                       (* 24 (duration-day dur))))))))

; As expected, a datetime contains a year, month, day, hour, minute and second.
; It supports comparisons as well as addition and subtraction with durations.
; Values are truncated if the duration is not exact.
(define-generics dt
  [dt=? dt elem]
  [dt>? dt elem]
  [dt<? dt elem]
  [dt+ dt elem]
  [dt- dt elem])

(struct datetime (year month day hour minute second)
  #:transparent
  #:methods gen:dt
  [(define-syntax-rule (compare-with-datetime op self dt)
     (op (datetime->number self) (datetime->number dt)))
   (define (dt=? self dt) (compare-with-datetime = self dt))
   (define (dt>? self dt) (compare-with-datetime > self dt))
   (define (dt<? self dt) (compare-with-datetime < self dt))

   (define-syntax-rule (add/sub-with-datetime op self dur)
     (normalize-datetime
      (datetime-year self)
      (datetime-month self)
      (truncate (op (datetime-day self) (duration-day dur)))
      (truncate (op (datetime-hour self) (duration-hour dur)))
      (truncate (op (datetime-minute self) (duration-minute dur)))
      (truncate (op (datetime-second self) (duration-second dur)))))
   (define (dt+ self dur) (add/sub-with-datetime + self dur))
   (define (dt- self dur) (add/sub-with-datetime - self dur))])

; Helper function to determine if a year is a leap year.
(define (leap-year? year)
  (and (= (remainder year 4) 0)
       (implies (= (remainder year 100) 0)
                (= (remainder year 400) 0))))

; Helper function to determine the number of days in a month.
(define (days-in-month year month)
  (cond
    [(= month 1) 31]
    [(= month 2) (if (leap-year? year) 29 28)]
    [(= month 3) 31]
    [(= month 4) 30]
    [(= month 5) 31]
    [(= month 6) 30]
    [(= month 7) 31]
    [(= month 8) 31]
    [(= month 9) 30]
    [(= month 10) 31]
    [(= month 11) 30]
    [(= month 12) 31]))

; Helper function to normalize a date-time stamp, i.e. return another
; date-time with all components in the appropriate ranges.
(define (normalize-datetime year month day hour minute second)
  (normalize-datetime-rec year month day hour minute second datetime-unwind))

(define (normalize-datetime-rec year month day hour minute second unwind)
  (let* ([sec-norm (if (and (< second 0) (not (= 0 (remainder second 60))))
                       (+ (remainder second 60) 60)
                       (remainder second 60))]

         [min-carry (if (and (< second 0) (not (= 0 (remainder second 60))))
                        (+ minute (quotient second 60) -1)
                        (+ minute (quotient second 60)))]

         [min-norm (if (and (< min-carry 0) (not (= 0 (remainder min-carry 60))))
                       (+ (remainder min-carry 60) 60)
                       (remainder min-carry 60))]

         [hr-carry (if (and (< min-carry 0) (not (= 0 (remainder min-carry 60))))
                       (+ hour (quotient min-carry 60) -1)
                       (+ hour (quotient min-carry 60)))]

         [hr-norm (if (and (< hr-carry 0) (not (= 0 (remainder hr-carry 24))))
                      (+ (remainder hr-carry 24) 24)
                      (remainder hr-carry 24))]

         [day-carry (if (and (< hr-carry 0) (not (= 0 (remainder hr-carry 24))))
                        (+ day (quotient hr-carry 24) -1)
                        (+ day (quotient hr-carry 24)))]

         [mth-norm (if (and (< month 1) (not (= 0 (remainder (- month 1) 12))))
                       (+ (remainder (- month 1) 12) 12 1)
                       (+ (remainder (- month 1) 12) 1))]

         [yr-norm (if (and (< month 1) (not (= 0 (remainder (- month 1) 12))))
                      (+ year (quotient (- month 1) 12) -1)
                      (+ year (quotient (- month 1) 12)))])

    (cond
      [(and (< day-carry 1) (not (eq? unwind 0)))
       (normalize-datetime-rec yr-norm (- mth-norm 1)
                           (+ day-carry
                              (days-in-month yr-norm
                                             (+ (remainder
                                                 (+ (remainder (- mth-norm 1 1) 12) 12) 12) 1)))
                           hr-norm min-norm sec-norm
                           (if (not unwind) #f (- unwind 1)))]
      
      [(and (> day-carry (days-in-month yr-norm mth-norm)) (not (eq? unwind 0)))
       (normalize-datetime-rec yr-norm (+ mth-norm 1)
                           (- day-carry (days-in-month yr-norm mth-norm))
                           hr-norm min-norm sec-norm
                           (if (not unwind) #f (- unwind 1)))]

      [else (datetime yr-norm mth-norm day-carry hr-norm min-norm sec-norm)])))

; Helper function to check if a date-time is normalized.
(define (normalized? dt)
  (and (>= (datetime-month dt) 1)
       (<= (datetime-month dt) 12)
       (>= (datetime-day dt) 1)
       (<= (datetime-day dt) (days-in-month (datetime-year dt) (datetime-month dt)))
       (>= (datetime-hour dt) 0)
       (<= (datetime-hour dt) 23)
       (>= (datetime-minute dt) 0)
       (<= (datetime-minute dt) 59)
       (>= (datetime-second dt) 0)
       (<= (datetime-second dt) 59)))

; Helper function for converting a date-time into a number for comparisons.
(define (datetime->number dt)
  (let ([dt-norm (normalize-datetime
                  (datetime-year dt)
                  (datetime-month dt)
                  (datetime-day dt)
                  (datetime-hour dt)
                  (datetime-minute dt)
                  (datetime-second dt))])
    (+ (datetime-second dt-norm)
       (* 100 (+ (datetime-minute dt-norm)
                 (* 100 (+ (datetime-hour dt-norm)
                           (* 100 (+ (datetime-day dt-norm)
                                     (* 100 (+ (datetime-month dt-norm)
                                               (* 100 (datetime-year dt-norm)))))))))))))

; Helper function to determine if a datetime is in between a given range.
(define (dt-between? dt dt-low dt-high)
  (and (or (dt>? dt dt-low) (dt=? dt dt-low))
       (or (dt<? dt dt-high) (dt=? dt dt-high))))

; Schedules are modelled as a pair containing a set of starting pattern and
; a duration indicating the period with which the pattern repeats. If the
; duration is set to #f, then the starting pattern is never repeated. If the
; starting pattern is null, then the scheduled activity will never occur.
(define-generics sched
  [on-schedule? sched elem])

(struct schedule (pattern interval)
  #:transparent
  #:methods gen:sched
  [(define (update-interval self)
     (map (lambda (sched-dt) (dt+ sched-dt (schedule-interval self)))
          (schedule-pattern self)))

   (define (on-schedule-rec? self dt unwind)
     (if (memf (lambda (sched-dt) (dt=? sched-dt dt)) (schedule-pattern self))
         #t
         (if (or (eq? unwind 0)
                 (not (schedule-interval self))
                 (null? (schedule-pattern self)))
             #f
             (on-schedule-rec?
              (schedule (update-interval self) (schedule-interval self))
              dt
              (if (not unwind) #f (- unwind 1))))))
   
   (define (on-schedule? self dt)
     (on-schedule-rec? self dt schedule-unwind))])

;; Symbolic constants for checking properties of date-time manipulation.
;(define-symbolic dt-year dt-month dt-day dt-hour dt-minute dt-second integer?)
;(define-symbolic dur-day dur-hour dur-minute dur-second integer?)
;(define dt-sym (datetime dt-year dt-month dt-day dt-hour dt-minute dt-second))
;(define dur-sym (duration dur-day dur-hour dur-minute dur-second))
;
;; Check that date-times can be normalized after adding/subtracting 31 days max.
;(define (verify-normalize dt-sym dur-sym)
;  (verify #:assume (assert (and (normalized? dt-sym)
;                                (dur>? dur-sym (duration -31 0 0 0))
;                                (dur<? dur-sym (duration  31 0 0 0))))
;          #:guarantee (assert (normalized? (dt+ dt-sym dur-sym)))))
;
;; Check that date-time manipulations are reversible.
;; Tested with datetime-unwind set to 0.
;(define (verify-reversible dt-sym dur-sym)
;  (verify #:assume (assert (and (= dt-year 2000)
;                                (normalized? dt-sym)
;                                (dur>? dur-sym (duration -1 0 0 0))
;                                (dur<? dur-sym (duration 1 0 0 0))))
;          #:guarantee (assert (dt=? dt-sym (dt- (dt+ dt-sym dur-sym) dur-sym)))))