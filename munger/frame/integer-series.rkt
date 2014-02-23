#lang typed/racket/base

(provide
 (struct-out ISeries))

(provide:
 [new-ISeries ((Vectorof Fixnum) (Option (U (Listof Label) SIndex)) -> ISeries)]
 [iseries-ref (ISeries Index -> Fixnum)]
 [iseries-count (ISeries -> Index)]
 [iseries-referencer (ISeries -> (Index -> Integer))]
 [map/is (ISeries (Fixnum -> Fixnum) -> ISeries)]
 [bop/is (ISeries ISeries (Fixnum Fixnum -> Fixnum) -> ISeries)]
 [+/is (ISeries ISeries -> ISeries)]
 [-/is (ISeries ISeries -> ISeries)]
 [*/is (ISeries ISeries -> ISeries)]
 [//is (ISeries ISeries -> ISeries)]
 [+./is (Fixnum ISeries -> ISeries)]
 [-./is (ISeries Fixnum -> ISeries)]
 [*./is (Fixnum ISeries -> ISeries)]
 [/./is (ISeries Fixnum -> ISeries)])

(require
 racket/unsafe/ops
 (only-in "indexed-series.rkt"
	  build-index-from-labels
	  Label SIndex LabelIndex))

(struct: ISeries LabelIndex ([data : (Vectorof Fixnum)]))

(: new-ISeries ((Vectorof Fixnum) (Option (U (Listof Label) SIndex)) -> ISeries))

(define (new-ISeries data labels)

  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (vector-length data) (hash-count index))
      (let ((k (current-continuation-marks)))
	(raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))

  (if (hash? labels)
      (begin
	(check-mismatch labels)
	(ISeries labels data))
      (if labels
	  (let ((index (build-index-from-labels labels)))
	    (check-mismatch index)
	    (ISeries index data))
	  (ISeries #f data))))

(: iseries-referencer (ISeries -> (Index -> Integer)))
(define (iseries-referencer cseries)
  (let ((data (ISeries-data cseries)))
    (λ: ((idx : Index))
	(vector-ref data idx))))

(: iseries-ref (ISeries Index -> Fixnum))
(define (iseries-ref series idx)
  (vector-ref (ISeries-data series) idx))

(: iseries-count (ISeries -> Index))
(define (iseries-count series)
  (vector-length (ISeries-data series)))

(: map/is (ISeries (Fixnum -> Fixnum) -> ISeries))
(define (map/is series fn)
  (let ((old-data (ISeries-data series)))
    (ISeries #f (build-vector (vector-length old-data)
			      (λ: ((idx : Natural))
				  (fn (vector-ref old-data idx)))))))

;; Binary ISeries Ops

(: bop/is (ISeries ISeries (Fixnum Fixnum -> Fixnum) -> ISeries))
(define (bop/is ns1 ns2 bop)
  (define v1 (ISeries-data ns1))
  (define v2 (ISeries-data ns2))
  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (vector-length v2))
	  (error '+/is "Series must be of equal length."))
  
  (define: v-bop : (Vectorof Fixnum) (make-vector len #{0 : Fixnum}))

  (do: : ISeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (ISeries #f v-bop))
       (vector-set! v-bop idx (bop (vector-ref v1 idx)
				   (vector-ref v2 idx)))))

(: +/is (ISeries ISeries -> ISeries))
(define (+/is ns1 ns2)
  (bop/is ns1 ns2 unsafe-fx+))

(: -/is (ISeries ISeries -> ISeries))
(define (-/is ns1 ns2)
  (bop/is ns1 ns2 unsafe-fx-))

(: */is (ISeries ISeries -> ISeries))
(define (*/is ns1 ns2)
  (bop/is ns1 ns2 unsafe-fx*))

(: //is (ISeries ISeries -> ISeries))
(define (//is ns1 ns2)
  (bop/is ns1 ns2 unsafe-fxquotient))

;; Scalar ISeries Ops

(: bop./is (Fixnum ISeries (Fixnum Fixnum -> Fixnum) -> ISeries))
(define (bop./is fx is bop)
  (define: v1 : (Vectorof Fixnum) (ISeries-data is))
  (define: len : Index (vector-length v1))
  (define: v-bop : (Vectorof Fixnum) ((inst make-vector Fixnum) len #{0 : Fixnum}))
  (do: : ISeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (ISeries #f v-bop))
       (vector-set! v-bop idx (bop fx #{(vector-ref v1 idx) : Fixnum}))))

(: +./is (Fixnum ISeries -> ISeries))
(define (+./is fx ns)
  (bop./is fx ns unsafe-fx+))

(: -./is (ISeries Fixnum -> ISeries))
(define (-./is ns fx )
  (bop./is fx ns unsafe-fx-))

(: *./is (Fixnum ISeries -> ISeries))
(define (*./is fx ns)
  (bop./is fx ns unsafe-fx*))

(: /./is (ISeries Fixnum -> ISeries))
(define (/./is ns fx)
  (bop./is fx ns unsafe-fxquotient))
