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


#lang typed/racket/base

(provide
 define-fixed-layout)

(require
 (only-in "../layout-types.rkt"
	  Field Layout)
 (for-syntax
  syntax/parse
  typed/racket
  (only-in racket/syntax
	   format-id)
  (only-in "../layout-types.rkt"
	   Field Layout)
  (only-in "../layout.rkt"
	   tr-type-for-field-type)))

(define-syntax (define-fixed-layout stx)

  (define-syntax-class field
    (pattern (fid:id type:id length:nat)))

  (: offset : Natural)
  (define offset 0)

  (: field (Listof Field))
  (define fields '())

  (define (field-syntax-with-offsets fs)
    (for/list ([f fs])
	      (syntax-parse f
			    ((fid:id ftype:id flen:nat)
			     (let ((curr-offset offset))
			       (with-syntax ((tr-type (tr-type-for-field-type (syntax->datum #'ftype)))
					     (foffset curr-offset)
					     (fname (syntax->datum #'fid)))
					    (set! fields (cons (Field (syntax->datum #'fid)
								      'String
								      curr-offset
								      (syntax->datum #'flen))
							       fields))
					    (set! offset (+ offset (syntax->datum #'flen)))
					    #`(Field 'fname 'tr-type foffset flen)))))))

  (syntax-parse stx
		[(_ name:id f0:field f1:field ...)
		 (with-syntax ((lo  #`(list #,@(field-syntax-with-offsets
						(syntax->list #'(f0 f1 ...)))))
			       (desc-name (format-id #'name "~a-desc" (syntax-e #'name))))
			      (let ((name-id (symbol->string (syntax->datum #'name))))
				#'(begin
				    (define-syntax desc-name (Layout 'name 'Fixed lo))
				    (define name (Layout 'name 'Fixed lo)))))]))
