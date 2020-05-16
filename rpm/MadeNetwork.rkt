#lang rosette/safe

(require "./MadeProcess.rkt"
         "../rim/MadeDataStructures.rkt"
         "../rim/TemporalDataTypes.rkt")

(provide (struct-out made-network)
         gen:made-net execute-network execute-stream
         generate-data-stream (struct-out stream-item))

; This file contains the implementation of a MADE network.

; A MADE network is simply a set of MADE processes that can be executed.
; The execution is implemented as comprising the following steps:
; 1) Generate data from the MADE processes (in order from M to A to D to E).
; 2) Update the data state of the MADE processes. 
; 3) Update the control state of all MADE processes using the output of the
;    effectuation processes.
; 4) Return the updated MADE network and all generated data.
(define-generics made-net
  [execute-network made-net in-data datetime]
  [execute-stream made-net data-stream])

(struct made-network (monitoring analysis decision effectuation)
  #:transparent
  #:methods gen:made-net
  [(define-syntax-rule (update-data-state-list process-list in-data datetime)
     (map (lambda (p) (update-data-state p in-data datetime)) process-list))
   (define-syntax-rule (generate-data-list process-list in-data datetime)
     (foldl (lambda (out-list result) (append out-list result))
            null
            (map (lambda (p) (generate-data p in-data datetime)) process-list)))
   (define-syntax-rule (update-control-state-list process-list in-data datetime)
     (map (lambda (p) (update-control-state p in-data datetime)) process-list))

   (define (execute-stream self data-stream)
     (if (null? data-stream)
         null
         (let* ([cur-dt (stream-item-datetime (list-ref data-stream 0))]
                [cur-dset (stream-item-dataset (list-ref data-stream 0))]
                [cur-out (execute-network self cur-dset cur-dt)])
           (append (list (list cur-dt (list-ref cur-out 0) (list-ref cur-out 1)))
                   (execute-stream (list-ref cur-out 0) (list-tail data-stream 1))))))
   
   (define (execute-network self in-data datetime)
     (let* ([in-measurements (filter (lambda (d) (measurement? d)) in-data)]
            [in-observations (filter (lambda (d) (observation? d)) in-data)]
            [in-abstractions (filter (lambda (d) (abstraction? d)) in-data)]
            [in-action-plans (filter (lambda (d) (action-plan? d)) in-data)]
            [in-instructions (filter (lambda (d) (or (action-instruction? d)
                                                     (control-instruction? d))) in-data)]           

            [monitoring-output (generate-data-list
                                (made-network-monitoring self)
                                in-measurements
                                datetime)]
            [analysis-output (generate-data-list
                              (made-network-analysis self)
                              (append in-observations monitoring-output)
                              datetime)]
            [decision-output (generate-data-list
                              (made-network-decision self)
                              (append in-abstractions analysis-output)
                              datetime)]
            [effectuation-output (generate-data-list
                                  (made-network-effectuation self)
                                  (append in-action-plans decision-output)
                                  datetime)]

            [out-data (append monitoring-output
                              analysis-output
                              decision-output
                              effectuation-output)]

            [updated-monitoring (update-control-state-list
                                 (update-data-state-list
                                  (made-network-monitoring self)
                                  (append in-data out-data)
                                  datetime)
                                 (append in-data out-data)
                                 datetime)]
            [updated-analysis (update-control-state-list
                               (update-data-state-list
                                (made-network-analysis self)
                                (append in-data out-data)
                                datetime)
                               (append in-data out-data)
                               datetime)]
            [updated-decision (update-control-state-list
                               (update-data-state-list
                                (made-network-decision self)
                                (append in-data out-data)
                                datetime)
                               (append in-data out-data)
                               datetime)]
            [updated-effectuation (update-control-state-list
                                   (update-data-state-list
                                    (made-network-effectuation self)
                                    (append in-data out-data)
                                    datetime)
                                   (append out-data)
                                   datetime)])

       (list (made-network updated-monitoring
                           updated-analysis
                           updated-decision
                           updated-effectuation)
             out-data)))])

; A data stream item is modelled as comprising a datetime stamp and a set of data items.
(struct stream-item (datetime dataset) #:transparent)

; generate-data-stream generates a symbolic data stream from the following inputs:
; 1) The start datetime.
; 2) The end datetime.
; 3) A duration indicating the repeat interval.
; 4) A list of getters for the MADE data.
(define (generate-data-stream start-datetime end-datetime repeat-interval getter-list)
  (if (dt>? start-datetime end-datetime)
      null
      (append (list (stream-item
                     start-datetime
                     (map (lambda (getter) (getter start-datetime start-datetime))
                          getter-list)))
              (generate-data-stream
               (dt+ start-datetime repeat-interval)
               end-datetime
               repeat-interval
               getter-list))))
