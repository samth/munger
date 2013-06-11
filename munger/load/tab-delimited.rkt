#lang typed/racket/base

(provide:
 [parse-tab-line LineParser]
 [read-tab-delimited-file (FilePath Boolean FrameBuilder -> FrameBuilder)])

(require
 (only-in grip/system/filepath
	  FilePath)
 (only-in "types.rkt"
	  LineParser)
 (only-in "delimited-common.rkt"
	  frame-builder-tank
	  read-formatted-file)
 (only-in "frame-builder.rkt"
	  FrameBuilder))

(require/typed racket/string
	       [string-trim (String -> String)]
	       [string-split (String (U String Regexp) -> (Listof String))])

(: parse-tab-line LineParser)
(define (parse-tab-line str)
  (map string-trim (string-split str "\t")))

;; One day - Pass in the parser.  i.e More efficient Dynamic or static.
(: read-tab-delimited-file (FilePath Boolean FrameBuilder -> FrameBuilder))
(define (read-tab-delimited-file fpath header? frame-builder)
  (read-formatted-file fpath header? 
		       (frame-builder-tank frame-builder parse-tab-line)))
