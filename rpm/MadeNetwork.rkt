#lang rosette/safe

; This file contains the implementation of a MADE network.

; A MADE network is simply a set of MADE processes that can be executed.
; The execution is implemented as comprising the following steps:
; 1) Update the data state of the monitoring processes and generate their data.
; 2) Repeat for analysis, decision and effectuation, with the output of the
;    previous processes appended to the original input data.
; 3) Update the control state of all MADE processes using the output of the
;    effectuation processes.
; 4) Return the updated MADE network and all generated data.
(define-generics made-net
  [execute made-net in-data datetime])

(struct made-network (monitoring analysis decision effectuation)
  #:transparent
  #:methods gen:made-net
  [(define-syntax-rule (update-data-state-list process-list in-data)
     (map (lambda (p) (update-data-state p in-data)) process-list))
   (define-syntax-rule (generate-data-list process-list datetime)
     (map (lambda (p) (generate-data p datetime)) process-list))
   (define-syntax-rule (update-control-state-list process-list in-data datetime)
     (map (lambda (p) (update-control-state p in-data datetime)) process-list))

   (define (execute self in-data datetime)
     (let* ([updated-monitoring (update-data-state-list
                                 (made-network-monitoring self)
                                 in-data)]
            [monitoring-output (generate-data-list updated-monitoring datetime)]
            
            [updated-analysis (update-data-state-list
                               (made-network-analysis self)
                               (append in-data monitoring-output))]
            [analysis-output (generate-data-list updated-analysis date-time)]

            [updated-decision (update-data-state-list
                               (made-network-decision self)
                               (append in-data analysis-output))]
            [decision-output (generate-data-list updated-decision date-time)]

            [updated-effectuation (update-data-state-list
                                   (made-network-effectuation self)
                                   (append in-data decision-output))]
            [effectuation-output (generate-data-list updated-effectuation date-time)]

            [next-monitoring (update-control-state-list
                              updated-monitoring effectuation-output datetime)]
            [next-analysis (update-control-state-list
                            updated-analysis effectuation-output datetime)]
            [next-decision (update-control-state-list
                            updated-decision effectuation-output datetime)]
            [next-effectuation (update-control-state-list
                                updated-effectuation effectuation-output)])

       (list (made-network next-monitoring next-analysis next-decision next-effectuation)
             (append monitoring-output analysis-output decision-output effectuation-output))))])
