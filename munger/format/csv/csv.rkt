;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's Typed Racket Library
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket

(provide:
 [read-string-field (Input-Port -> String)]
 [read-integer-field (Input-Port -> Integer)]
 [read-float-field (Input-Port -> Float)])

(provide
 doit)

(: double-quote? (Char -> Boolean))
(define (double-quote? ch)
  (char=? #\" ch))

(: quote-escape? (Char -> Boolean))
(define (quote-escape? ch)
  (char=? #\\ ch))

(: comma-delimiter? (Char -> Boolean))
(define (comma-delimiter? ch)
  (char=? #\, ch))

(: eol-delimiter? (Char -> Boolean))
(define (eol-delimiter? ch)
  (or (char=? #\linefeed ch)
      (char=? #\newline ch)))

(: eol? (Char Input-Port -> Boolean))
(define (eol? ch inp)
  (let ((rs (eol-delimiter? ch)))
    (let ((ch (peek-char inp)))
      (and (char? ch)
	   (eol-delimiter? ch))
      (read-char inp))
    rs))

(: end-of-field (Input-Port -> Void))
(define (end-of-field inp)
  (let ((ch (read-char inp)))
    (unless (or (eof-object? ch)
		(eol-delimiter? ch)
		(comma-delimiter? ch))
	    (error 'end-of-field "Invalid CSV.  Expected a EOF or ',' delimiter."))))

(: read-string-field (Input-Port -> String))
(define (read-string-field inp)

  (define outp (open-output-string))

  (let ((ch (read-char inp)))
    (if (eof-object? ch)
	(error 'read-string-field "Invalid CSV.  Unexpected EOF")
	(unless (double-quote? ch)
		(error 'read-string-value "Invalid CSV, string field value must be quoted."))))

  (let: loop : String ((ch : (U Char EOF) (read-char inp)))
	(if (eof-object? ch)
	    (error "Invalid CSV, EOF found in a string field.  Expecting closing quote in: ~s" (get-output-string outp))
	    (if (double-quote? ch)
		(let ((next-ch (peek-char inp)))
		  (if (eof-object? next-ch)
		      (error "Invalid CSV unexpected EOF.")
		      (if (double-quote? next-ch)
			  (begin
			    (read-char inp)
			    (display next-ch outp)
			    (loop (read-char inp)))
			  (begin
			    (end-of-field inp)
			    (get-output-string outp)))))
		(begin
		  (display ch outp)
		  (loop (read-char inp)))))))

(: read-number-string (Input-Port -> String))
(define (read-number-string inp)
  (define outp (open-output-string))
  (let: loop : String ((ch : (U Char EOF) (read-char inp)))
	(if (eof-object? ch)
	    (get-output-string outp)
	    (cond
	     ((or (char-numeric? ch)
		  (char=? #\. ch))
	      (display ch outp)
	      (loop (read-char inp)))
	     ((or (comma-delimiter? ch)
		  (eol? ch inp))
	      (get-output-string outp))
	     (else (error 'read-number-value "Invalid CSV, expected a numeric field."))))))

(: s->i (String -> Integer))
(define (s->i s)
  (let ((n (string->number s)))
    (if (exact-integer? n)
	n
	(error "Expected Integer field: ~s" s))))

(: s->r (String -> Float))
(define (s->r s)
  (let ((n (string->number s)))
    (if (real? n)
	(real->double-flonum n)
	(error "Expected Real number field: ~s" s))))

(: read-integer-field (Input-Port -> Integer))
(define (read-integer-field inp)
  (s->i (read-number-string inp)))

(: read-float-field (Input-Port -> Float))
(define (read-float-field inp)
  (s->r (read-number-string inp)))

;; WIP Test.
;; (define (doit)
;;   (define line "123,\"A\"\"B\"\"C\",1.2\n456,\"Ray\",1.5")
;;   (define inp (open-input-string line))
;;   (port-count-lines! inp)

;;   (pretty-print (read-integer-value inp))
;;   (pretty-print (read-string-value inp))
;;   (pretty-print (read-float-field inp))

;;   (displayln "----------------------")

;;   (pretty-print (read-integer-value inp))
;;   (pretty-print (read-string-value inp))
;;   (pretty-print (read-float-field inp)))
