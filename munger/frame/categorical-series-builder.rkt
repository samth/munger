;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's Data Munger Library
;; Copyright (C) 2007-2013  Raymond Paul Racine
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#lang typed/racket/base

(provide
 CSeriesBuilder
 CSeriesBuilder?
 new-CSeriesBuilder
 append-CSeriesBuilder
 complete-CSeriesBuilder)

(require
 (only-in "../frame/categorical-series.rkt"
          CSeries))

(struct: CSeriesBuilder ([index : Index]
			 [ord : Index]
			 [data : (Vectorof Index)]
			 [nominals : (HashTable Symbol Index)]) #:mutable)

(define default-cseries-size 512)

;; Make a SeriesBuilder for a CSeries
(: new-CSeriesBuilder (case-> (-> CSeriesBuilder)
			      (Index -> CSeriesBuilder)))
(define (new-CSeriesBuilder [size default-cseries-size])
  (CSeriesBuilder 0 0 (make-vector size 0) (make-hasheqv)))

;; Have builder construct and return a CSeries
(: complete-CSeriesBuilder (CSeriesBuilder -> CSeries))
(define (complete-CSeriesBuilder builder)
  
  (: compacted-data (Vectorof Index))
  (define compacted-data
    (let* ((data (CSeriesBuilder-data builder))
           (len (CSeriesBuilder-index builder)))
      (let: ((new-data : (Vectorof Index) (make-vector len 0)))
	    ((inst vector-copy! Index) new-data 0 data 0 len)
	    new-data)))
  
  (: nominals (Vectorof Symbol))
  (define nominals
    (let* ((nom-map (CSeriesBuilder-nominals builder)) 
           (len (hash-count nom-map))
           (noms (make-vector len 'NA)))
      (hash-for-each nom-map (λ: ((n : Symbol) (i : Index))
				 (vector-set! noms i n)))
      noms))
  
  (CSeries #f compacted-data nominals))

;; Extend a builder with next data element
(: append-CSeriesBuilder (CSeriesBuilder (U Symbol String) -> Void))
(define (append-CSeriesBuilder builder nominal)
  
  (: nominalizer (String -> Symbol))
  (define (nominalizer str)
    (string->symbol str))
  
  (define-syntax bump
    (syntax-rules ()
      [(bump x)
       (assert (add1 x) index?)]))
  
  (define (bump-ord)
    (let ((ord (CSeriesBuilder-ord builder)))
      (set-CSeriesBuilder-ord! builder (bump ord))     
      ord))
  
  (define (bump-index)
    (let ((idx (CSeriesBuilder-index builder)))
      (set-CSeriesBuilder-index! builder (bump idx))
      idx))
  
  (: nominal-ordinal (Symbol -> Index))
  (define (nominal-ordinal sym)    
    (hash-ref! (CSeriesBuilder-nominals builder) sym bump-ord))
  
  (: extend-data (-> Void))
  (define (extend-data)
    (let* ((data (CSeriesBuilder-data builder))
           (curr-len (vector-length data))
           (new-len (assert (inexact->exact (round (* 2.0 curr-len))) exact-integer?)))
      (let: ((new-data : (Vectorof Index) (make-vector new-len 0)))
	    ((inst vector-copy! Index) new-data 0 data)
	    (set-CSeriesBuilder-data! builder new-data))))
  
  (if (< (CSeriesBuilder-index builder) 
         (vector-length (CSeriesBuilder-data builder)))      
      (vector-set! (CSeriesBuilder-data builder)
                   (bump-index)
                   (nominal-ordinal (if (string? nominal)
					(nominalizer nominal)
					nominal)))
      (begin
        (extend-data)
        (append-CSeriesBuilder builder nominal))))
