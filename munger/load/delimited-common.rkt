#lang typed/racket/base

(provide:
 [check-data-file-exists (FilePath -> Void)]
 [sample-formatted-file (FilePath Integer -> (Listof String))]
 [frame-builder-tank (FrameBuilder LineParser -> (Tank Line FrameBuilder))]
 [read-formatted-file (FilePath Boolean (Tank String FrameBuilder) -> FrameBuilder)])

(require
 (only-in grip/system/filepath
	  FilePath FilePath->string)
 (only-in pipe
	  Tank Stream Continue Done
	  pump/text-input-port
	  drain head-n)
 (only-in "../frame/series-builder.rkt"
	  SeriesBuilder)
 (only-in "../frame/categorical-series-builder.rkt"
	  CSeriesBuilder CSeriesBuilder?
	  append-CSeriesBuilder)
 (only-in "../frame/integer-series-builder.rkt"
	  ISeriesBuilder ISeriesBuilder?
	  append-ISeriesBuilder)
 (only-in "../frame/numeric-series-builder.rkt"
	  NSeriesBuilder NSeriesBuilder?
	  append-NSeriesBuilder)
 (only-in "frame-builder.rkt"	  
	  FrameBuilder FrameBuilder-builders
	  append-data-fields)
 (only-in "schema.rkt"
	  Schema)
 (only-in "types.rkt"
	  LineParser Line Line?))

(: frame-builder-tank (FrameBuilder LineParser -> (Tank Line FrameBuilder)))
(define (frame-builder-tank frame-builder line-parser)

  (: appenders (Listof (Line -> Void)))
  (define appenders (map (λ: ((builder : SeriesBuilder))
			     (cond
			      [(CSeriesBuilder? builder)
			       (λ: ((str : String))
				   (append-CSeriesBuilder builder str))]
			      [(ISeriesBuilder? builder)
			       (λ: ((str : String))
				   (append-ISeriesBuilder builder str))]
			      [(NSeriesBuilder? builder)
			       (λ: ((str : String))
				   (append-NSeriesBuilder builder str))]
			      [else (λ: ((str : String)) (void))]))
			 (FrameBuilder-builders frame-builder)))

  (: step ((Stream String) -> (Tank String FrameBuilder)))
  (define (step input)
    (cond
     ((Line? input)
      (append-data-fields appenders (line-parser input))
      (Continue step))
     ((eq? input 'EOS)
      (Done 'EOS frame-builder))
     ((eq? input 'Nothing)
      (Continue step))))

  (Continue step))


(: check-data-file-exists (FilePath -> Void))
(define (check-data-file-exists fpath)
  (unless (file-exists? (FilePath->string fpath))
	  (error (format "File not found: ~s" (FilePath->string fpath)))))

(: read-formatted-file (FilePath Boolean (Tank String FrameBuilder) -> FrameBuilder))
(define (read-formatted-file fpath headers frame-tank)
  (check-data-file-exists fpath)
  (let ((fpath (FilePath->string fpath)))
    (call-with-input-file*
     fpath
     (λ: ((inp : Input-Port))
	 (when headers (read-line inp))
	 (drain (((inst pump/text-input-port FrameBuilder) inp) frame-tank))))))

(: sample-formatted-file (FilePath Integer -> (Listof String)))
(define (sample-formatted-file fpath cnt)
  (check-data-file-exists fpath)
  (let ((fpath (FilePath->string fpath)))
    (call-with-input-file*
     fpath
     (λ: ((inp : Input-Port))
	 (drain (((inst pump/text-input-port (Listof String)) inp)
		 ((inst head-n String) cnt)))))))
