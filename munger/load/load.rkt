#lang typed/racket/base

(provide:
 [determine-schema (FilePath Integer -> Schema)]
 [load-csv-file (FilePath [#:schema (Option Schema)] -> Frame)]
 [load-tab-delimited-file (FilePath [#:schema (Option Schema)] -> Frame)])

(require
 racket/match
 (only-in grip/data/list
	  zip)
 (only-in grip/system/filepath
	  FilePath FilePath->string)
 (only-in "schema.rkt"
	  generate-anon-series-names
	  Schema SeriesTypes Schema-has-headers
	  Schema-SeriesTypes Schema-headers)
 (only-in "../frame/series-builder.rkt"
	  SeriesBuilder)
 (only-in "../frame/integer-series-builder.rkt"
	  new-ISeriesBuilder
	  ISeriesBuilder
	  ISeriesBuilder?
	  complete-ISeriesBuilder)
 (only-in "../frame/numeric-series-builder.rkt"
	  new-NSeriesBuilder
	  NSeriesBuilder
	  NSeriesBuilder?
	  complete-NSeriesBuilder)
 (only-in "../frame/categorical-series-builder.rkt"
	  new-CSeriesBuilder
	  CSeriesBuilder
	  CSeriesBuilder?
	  complete-CSeriesBuilder
	  append-CSeriesBuilder)
 (only-in "../frame/series-description.rkt"
	  Series)
 (only-in "../frame/frame.rkt"
	  Frame
	  new-frame)
 "frame-builder.rkt"
 (only-in "delimited-common.rkt"
	  sample-formatted-file
	  check-data-file-exists)
 (only-in "schema.rkt"
	  Schema)
 (only-in "sample.rkt"
	  determine-schema-from-sample)
 (only-in "csv-delimited.rkt"
	  read-csv-file)
 (only-in "tab-delimited.rkt"
	  read-tab-delimited-file))


(: new-FrameBuilder-from-Schema (Schema -> FrameBuilder))
(define (new-FrameBuilder-from-Schema schema)

  (: determine-SeriesBuilder (SeriesTypes -> SeriesBuilder))
  (define (determine-SeriesBuilder stypes)
    (match stypes
	   ['CATEGORICAL (new-CSeriesBuilder)]
	   ['INTEGER     (new-ISeriesBuilder)]
	   ['NUMERIC     (new-NSeriesBuilder)]))

  (FrameBuilder ((inst map SeriesBuilder SeriesTypes)
		 determine-SeriesBuilder
		 (Schema-SeriesTypes schema))))

(: complete-SeriesBuilders (FrameBuilder -> (Listof Series)))
(define (complete-SeriesBuilders frame-builder)
  (map (λ: ((builder : SeriesBuilder))
	   (cond
	    [(CSeriesBuilder? builder)
	     (complete-CSeriesBuilder builder)]
	    [(ISeriesBuilder? builder)
	     (complete-ISeriesBuilder builder)]
	    [(NSeriesBuilder? builder)
	     (complete-NSeriesBuilder builder)]
	    [else (error "Inconsistent FrameBuilder")]))
       (FrameBuilder-builders frame-builder)))

(: anon-headers (Integer -> (Listof Symbol)))
(define (anon-headers cnt)
  (map string->symbol (generate-anon-series-names cnt)))

(: make-frame (Schema FrameBuilder -> Frame))
(define (make-frame schema builder)
  (let ((cols (complete-SeriesBuilders builder)))
    (let ((headers (if (Schema-has-headers schema)
		       (Schema-headers schema)
		       (anon-headers (length cols)))))
      (new-frame ((inst zip Symbol Series) headers cols)))))

(: schema-if-needed ((Option Schema) FilePath -> Schema))
(define (schema-if-needed schema fpath)
  (define SAMPLE-SIZE 20)
  (if schema schema (determine-schema fpath SAMPLE-SIZE)))

(: load-csv-file (FilePath [#:schema (Option Schema)] -> Frame))
(define (load-csv-file fpath #:schema [schema #f])
  (let ((schema (schema-if-needed schema fpath)))
    (make-frame schema (read-csv-file fpath
				      (Schema-has-headers schema)
				      (new-FrameBuilder-from-Schema schema)))))

(: load-tab-delimited-file (FilePath [#:schema (Option Schema)] -> Frame))
(define (load-tab-delimited-file fpath #:schema [schema #f])
  (let ((schema (schema-if-needed schema fpath)))
    (make-frame schema (read-tab-delimited-file fpath
						(Schema-has-headers schema)
						(new-FrameBuilder-from-Schema schema)))))

;; Only works for tab delimited at the moment.
(: determine-schema (FilePath Integer -> Schema))
(define (determine-schema fpath cnt)
  (check-data-file-exists fpath)
  (determine-schema-from-sample (sample-formatted-file fpath cnt)))
