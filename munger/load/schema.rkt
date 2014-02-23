#lang typed/racket/base

(provide:
 [generate-anon-series-names (Integer -> (Listof String))])

(provide
 alter-schema-columns
 alter-schema-no-headers
 SeriesTypes
 (struct-out Schema)
 Schema-headers
 Schema-SeriesTypes
 (struct-out ColumnInfo))

(require
 racket/set)

;; Schema definition of a data file.
;; May be defined as meta information along with the data file
;; OR
;; Dynamically determined by sampling the data files.

(define-type SeriesTypes (U 'CATEGORICAL 'NUMERIC 'INTEGER))

(struct: ColumnInfo ([name : Symbol]
		     [type : SeriesTypes]))

(struct: Schema ([has-headers : Boolean]
		 [meta : (Listof ColumnInfo)]))

(: generate-anon-series-names (Integer -> (Listof String)))
(define (generate-anon-series-names n)
  (define base "x")
  (for/list ([x (in-range n)])
	    (string-append base (number->string x))))

(: Schema-headers (Schema -> (Listof Symbol)))
(define (Schema-headers schema)
  (map (λ: ((meta : ColumnInfo))
	   (ColumnInfo-name meta))
       (Schema-meta schema)))

(: Schema-SeriesTypes (Schema -> (Listof SeriesTypes)))
(define (Schema-SeriesTypes schema)
  (map (λ: ((meta : ColumnInfo))
	   (ColumnInfo-type meta))
       (Schema-meta schema)))

(: alter-schema-no-headers (Schema -> Schema))
(define (alter-schema-no-headers schema)
  (if (Schema-has-headers schema)
      (let* ((metas (Schema-meta schema))
	     (hdrs (generate-anon-series-names (length metas))))
	(let: loop : Schema ((hdrs : (Listof Symbol) (map string->symbol hdrs))
			     (metas : (Listof ColumnInfo) metas)
			     (accum : (Listof ColumnInfo) '()))
	      (if (null? hdrs)
		  (Schema #f (reverse accum))
		  (loop (cdr hdrs) (cdr metas)
			(cons (ColumnInfo (car hdrs) (ColumnInfo-type (car metas)))
			      accum)))))
      schema))

(: alter-schema-columns (Schema (Listof (Pair Symbol ColumnInfo)) -> Schema))
(define (alter-schema-columns schema metas)

  (define meta-map
    (let ((hmap ((inst make-hash Symbol ColumnInfo))))
      (for-each (λ: ((meta : (Pair Symbol ColumnInfo)))
		    (hash-set! hmap (car meta) (cdr meta)))
		metas)
      hmap))

  (: replaced-metas (Listof ColumnInfo))
  (define replaced-metas (let: loop : (Listof ColumnInfo)
			       ((old-metas : (Listof ColumnInfo) (Schema-meta schema))
				(accum : (Listof ColumnInfo) '()))
			       (if (null? old-metas)
				   (reverse accum)
				   (let ((curr-meta (car old-metas)))
				     (loop (cdr old-metas)
					   (cons
					    (hash-ref meta-map
						      (ColumnInfo-name curr-meta)
						      (λ () curr-meta))
					    accum))))))

  (struct-copy Schema schema [meta replaced-metas]))
