#lang rosette/safe

(require (only-in rosette symbol? syntax->datum eval-syntax check-duplicates raise-argument-error))
(require "../rim/BasicDataTypes.rkt")
(require "../rim/MadeDataStructures.rkt")

; This file contains the syntax for specifying new MADE information models.

