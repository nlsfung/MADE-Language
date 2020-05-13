#lang rosette/safe

(require "../lang/VerifySyntax.rkt"
         "../lang/IMSyntax.rkt"
         "../lang/NomEnumSyntax.rkt"
         "../rim/BasicDataTypes.rkt")

(provide (all-defined-out))

; This file contains the specification of the archetypes used to test the
; implementation of the MADE archetype language. Please refer to Table 5.2
; and Sec. 5.4 of the PhD thesis for more details about the tests. The specific
; outputs of each test are documented as comments in this specification.

; > (verify-archetype arch-01)
; Example #<procedure:arch-01>: 
; #(struct:arch-01 #f #(struct:datetime 2019 12 15 0 0 0)
;   #(struct:dimensioned dim$0 units))
(define-measurement arch-01 'units)

; > (verify-archetype arch-02)
; Example #<procedure:arch-02>: 
; (unsat)
(define-measurement arch-02 'units (lambda (v) #f))

; > (verify-archetype arch-03)
; Example #<procedure:arch-03>: 
; #(struct:arch-03 #f #(struct:datetime 2019 12 15 0 0 0)
;   #(struct:arch-03-value-space a))
(define-observation arch-03 nominal 'a 'b 'c)

; > (verify-archetype arch-04)
; Example #<procedure:arch-04>: 
; #(struct:arch-04 #f #(struct:datetime 2019 12 15 0 0 0) #(struct:count 0))
(define-observation arch-04 count)

; > (verify-archetype arch-05)
; Example #<procedure:arch-05>: 
; (unsat)
(define-observation arch-05 count
  (lambda (d) (and (> (get-value d) 50) (< (get-value d) 10))))

; > (verify-archetype arch-06)
; Example #<procedure:arch-06>: 
; #(struct:arch-06 #f #(struct:datetime-range
;   #(struct:datetime 2019 12 15 1 0 0) #(struct:datetime 2019 12 15 23 0 0))
;   #(struct:bool bool-val$0))
(define-observation arch-06 #:event)

; > (verify-archetype arch-07)
; Example #<procedure:arch-07>: 
; #(struct:arch-07 #f #(struct:datetime-range #(struct:datetime 2019 12 15 1 0 0)
;   #(struct:datetime 2019 12 15 1 0 0)) #(struct:arch-07-value-space b))
(define-abstraction arch-07 enumerated 'a 'b)

; > (verify-archetype arch-08)
; Example #<procedure:arch-08>: 
; #(struct:arch-08 #f #(struct:datetime-range #(struct:datetime 2019 12 15 1 0 0)
;   #(struct:datetime 2019 12 15 1 0 0)) #(struct:proportion 11))
(define-abstraction arch-08 proportion (lambda (d) (> (get-value d) 10)))

; > (verify-archetype arch-09)
; Example #<procedure:arch-09>: 
; (unsat)
(define-abstraction arch-09 proportion (lambda (d) (eq? (get-value d) #f)))

; > (verify-archetype arch-10)
; Example #<procedure:arch-10>: 
; #(struct:arch-10 #f #(struct:datetime 2019 12 15 0 0 0)
;   (#(struct:scheduled-homogeneous-action arch-13 #(struct:schedule
;   (#(struct:datetime 2019 12 15 0 0 0)) {2773771508841512397:2})
;    #(struct:dimensioned dim$0 units)
;    #(struct:duration dur-part$0 dur-part$1 dur-part$2 dur-part$3))))
(define-action-plan arch-10 (homogeneous-action 'arch-13))

; > (verify-archetype arch-11)
; Example #<procedure:arch-11>: 
; #(struct:arch-11 #f #(struct:datetime 2019 12 15 0 0 0)
;   (#(struct:scheduled-culminating-action arch-15 #(struct:schedule
;   (#(struct:datetime 2019 12 15 0 0 0)) {2128385024956610424:2})
;   #(struct:count 0))))
(define-action-plan arch-11 (culminating-action 'arch-15))

; > (verify-archetype arch-12)
; Example #<procedure:arch-12>: 
; #(struct:arch-12 #f #(struct:datetime 2019 12 15 0 0 0)
;   (#(struct:scheduled-control proc-1 #(struct:schedule
;   (#(struct:datetime 2019 12 15 0 0 0)) {-1411548690416635397:2}) status$0)
;   #(struct:scheduled-culminating-action arch-15 #(struct:schedule
;   (#(struct:datetime 2019 12 15 0 0 0)) {-2807555162316629719:2})
;   #(struct:count 0))))
(define-action-plan arch-12 (culminating-action 'arch-15) (control 'proc-1))

; > (verify-archetype arch-13)
; Example #<procedure:arch-13>: 
; #(struct:arch-13 #f #(struct:datetime 2019 12 15 0 0 0)
;   #(struct:dimensioned dim$0 units)
;   #(struct:duration dur-part$8 dur-part$9 dur-part$10 dur-part$11))
(define-action-instruction arch-13 #:homogeneous 'units)

; > (verify-archetype arch-14)
; Example #<procedure:arch-14>: 
; (unsat)
(define-action-instruction arch-14 #:homogeneous 'units (lambda (r v) #f))

; > (verify-archetype arch-15)
; Example #<procedure:arch-15>: 
; #(struct:arch-15 #f #(struct:datetime 2019 12 15 0 0 0) #(struct:count 0))
(define-action-instruction arch-15 #:culminating count)

; > (verify-archetype arch-16)
; Example #<procedure:arch-16>: 
; (unsat)
(define-action-instruction arch-16 #:culminating bool (lambda (v) (eq? v 1)))

; > (verify-archetype arch-17)
; Example #<procedure:arch-17>: 
; #(struct:arch-17 #f proc-1 #(struct:datetime 2019 12 15 0 0 0)
;   #(struct:schedule (#(struct:datetime 2019 12 15 0 0 0))
;   {1484878925140550989:2}) #<void>)
(define-control-instruction arch-17 'proc-1 'proc-2)