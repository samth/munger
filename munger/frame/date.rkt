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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide:
 [as-julian-day-numbers (CSeries -> ISeries)])

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
	(if date
	    (append-ISeriesBuilder ibuilder (assert (date->julian-day-number date) fixnum?))
	    (error 'as-julian-day-numbers "Invalid date: ~s" date))))))
