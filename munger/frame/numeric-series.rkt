#lang typed/racket/base

(provide:
 [nseries-ref (NSeries Index -> Float)]
 [nseries-label-ref (NSeries Label -> Float)]
 [nseries-count (NSeries -> Index)] 
 [map/ns (NSeries (Float -> Float) -> NSeries)]
 [bop/ns (NSeries NSeries (Float Float -> Float) -> NSeries)]
 [+/ns (NSeries NSeries -> NSeries)]
 [-/ns (NSeries NSeries -> NSeries)]
 [*/ns (NSeries NSeries -> NSeries)]
 [//ns (NSeries NSeries -> NSeries)]
 [+./ns (Float NSeries -> NSeries)]
 [-./ns (NSeries Float -> NSeries)]
 [*./ns (Float NSeries -> NSeries)]
 [/./ns (NSeries Float -> NSeries)])

(provide
 flvector-print
 (struct-out NSeries)
 mkNSeries)

(require
 racket/unsafe/ops
 racket/flonum
 (only-in "settings.rkt"
	  Settings-decimals
	  Settings-max-output
	  settings)
 (only-in "indexed-series.rkt"
	  label-index label->idx
	  build-index-from-labels
	  Label SIndex
	  GSeries GSeries-data
	  LabelIndex LabelIndex-index))

(: flvector-print (FlVector Output-Port -> Void))
(define (flvector-print flv port)
  (let ((len (flvector-length flv))
	(out (current-output-port))
	(decs (Settings-decimals (settings)))
	(max-output (Settings-max-output (settings))))
    (if (zero? len)
	(displayln "[ ]" port)
	(begin
	  (display "[ " port)
	  (do ((i 0 (add1 i)))
	      ((>= i len) (void))
	    (let ((num (flvector-ref flv i)))
	      (if (eqv? num +nan.0)
		  (display num port)
		  (display (real->decimal-string num decs) port)))
	    (display " " port))))
    (display "]" port)))

(: writer-NSeries (NSeries Output-Port Boolean -> Void))
(define (writer-NSeries series port mode)
  (let* ([data (NSeries-data series)]
	 [len (flvector-length data)])
    (displayln (format "(NSeries #:length ~s)" len) port)))

;; An NSeries is an optimized Series for computation over vectors of Float
;; i.e., NSeries should be faster then (Series Float)
(struct: NSeries LabelIndex ([data : FlVector])
	 ;;  #:methods gen:custom-write [(define write-proc writer-NSeries)]
	 )

(: mkNSeries (FlVector (Option (U (Listof Label) SIndex)) -> NSeries))
(define (mkNSeries data labels)

  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (flvector-length data) (hash-count index))
	    (let ((k (current-continuation-marks)))
	      (raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))

  (if (hash? labels)
      (begin
	(check-mismatch labels)
	(NSeries labels data))
      (if labels
	  (let ((index (build-index-from-labels labels)))
	    (check-mismatch index)
	    (NSeries index data))
	  (NSeries #f data))))

(: nseries-ref (NSeries Index -> Float))
(define (nseries-ref series idx)
  (flvector-ref (NSeries-data series) idx))

(: nseries-label-ref (NSeries Label -> Float))
(define (nseries-label-ref series label)
  (nseries-ref series (label->idx series label)))

(: nseries-count (NSeries -> Index))
(define (nseries-count nseries)
  (flvector-length (NSeries-data nseries)))

;; Map a function

(: map/ns (NSeries (Float -> Float) -> NSeries))
(define (map/ns series fn)
  (let ((old-data (NSeries-data series))
	(new-data (make-flvector (flvector-length (NSeries-data series)))))
    (let ((len (flvector-length old-data)))
      (let loop ((idx 0))
	(if (< idx len)
	    (begin
	      (flvector-set! new-data idx (fn (flvector-ref old-data idx)))
	      (loop (add1 idx)))
	    (void))))
    (NSeries (LabelIndex-index series) new-data)))

;; Binary NSeries Ops

(: bop/ns (NSeries NSeries (Float Float -> Float) -> NSeries))
(define (bop/ns ns1 ns2 bop)
  (define v1 (NSeries-data ns1))
  (define v2 (NSeries-data ns2))
  (define: len : Index (flvector-length v1))
  
  (unless (eqv? len (flvector-length v2))
	  (error '+/ns "Series must be of equal length."))
  
  (define: v-bop : FlVector(make-flvector len))

  (do: : NSeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (NSeries #f v-bop))
       (flvector-set! v-bop idx (bop (flvector-ref v1 idx)
				 (flvector-ref v2 idx)))))

(: +/ns (NSeries NSeries -> NSeries))
(define (+/ns ns1 ns2)
  (bop/ns ns1 ns2 fl+))

(: -/ns (NSeries NSeries -> NSeries))
(define (-/ns ns1 ns2)
  (bop/ns ns1 ns2 fl-))

(: */ns (NSeries NSeries -> NSeries))
(define (*/ns ns1 ns2)
  (bop/ns ns1 ns2 fl*))

(: //ns (NSeries NSeries -> NSeries))
(define (//ns ns1 ns2)
  (bop/ns ns1 ns2 fl/))

;; Scalar NSeries Ops

(: bop./ns (Float NSeries (Float Float -> Float) -> NSeries))
(define (bop./ns fl ns bop)
  (define v1 (NSeries-data ns))
  (define: len : Index (flvector-length v1))
  (define: v-bop : FlVector (make-flvector len))
  (do: : NSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (NSeries #f v-bop))
       (flvector-set! v-bop idx (bop fl (flvector-ref v1 idx)))))

(: +./ns (Float NSeries -> NSeries))
(define (+./ns fl ns)
  (bop./ns fl ns fl+))

(: -./ns (NSeries Float -> NSeries))
(define (-./ns ns fl )
  (bop./ns fl ns fl-))

(: *./ns (Float NSeries -> NSeries))
(define (*./ns fl ns)
  (bop./ns fl ns fl*))

(: /./ns (NSeries Float -> NSeries))
(define (/./ns ns fl)
  (bop./ns fl ns fl/))
