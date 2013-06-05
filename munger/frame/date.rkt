#lang typed/racket/base

(require 
 racket/fixnum
 (only-in grip/data/datetime/convert
	  date->julian-day-number)
(only-in grip/data/datetime/parse
	  parse-date)
 (only-in munger/frame/categorical-series
	  CSeries
	  cseries-count cseries-referencer)
 (only-in munger/frame/integer-series
	  ISeries
	  new-ISeries)
 (only-in munger/frame/integer-series-builder
	  new-ISeriesBuilder 
	  append-ISeriesBuilder
	  complete-ISeriesBuilder))

(: as-julian-day-numbers (CSeries -> ISeries))
(define (as-julian-day-numbers cseries)
  (let* ((clen (cseries-count cseries))
	 (cref (cseries-referencer cseries))
	 (ibuilder (new-ISeriesBuilder clen)))
    (do: : ISeries ([idx : Fixnum #{0 : Fixnum} (fx+ idx #{1 : Fixnum})])
	((= idx clen) (complete-ISeriesBuilder ibuilder))
      (let ((date (parse-date (symbol->string (cref idx)))))
	(if (and date (fixnum? date))
	    (append-ISeriesBuilder ibuilder (assert (date->julian-day-number date) fixnum?))
	    (error 'as-julian-day-numbers "Invalid date: ~s" date))))))
