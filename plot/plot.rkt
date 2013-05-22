#lang typed/racket/base

;; Comment out until NToronto's typed Plot makes it into the repo.

;(provide 
; histogram)
;
;(require 
; racket/match
; (only-in "../stats/tabulate.rkt"
;          Tabulation
;          tabulate)
; (only-in "../frame/categorical-series.rkt"
;          CSeries
;          CSeries-data
;          CSeries-nominals)
; (only-in plot
;          Plot-Result
;          Discrete-Histogram-Data
;          plot discrete-histogram))
;
;(: histogram (Tabulation -> Plot-Result))
;(define (histogram tabulation)  
;  
;  (: zip ((Vectorof Symbol) (Vectorof Natural) -> (Listof (List Symbol Natural))))
;  (define (zip noms cnts)
;    (for/list: : (Listof (List Symbol Natural)) 
;      ([n : Symbol noms]     
;       [c : Natural cnts])
;      (list n c)))
;  
;  (match tabulation
;    [(Tabulation noms cnts)    
;     (plot (discrete-histogram (zip noms cnts)))]))