#lang typed/racket/base

(provide
 parse-csv-line)

(provide:
 [read-csv-file (FilePath Boolean FrameBuilder -> FrameBuilder)])

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

(: double-quote? (Char -> Boolean))
(define (double-quote? ch)
  (char=? #\" ch))

(: comma-delimiter? (Char -> Boolean))
(define (comma-delimiter? ch)
  (char=? #\, ch))

(: whitespace? (Char -> Boolean))
(define (whitespace? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)))

(: consume-delimiter (Input-Port -> Boolean))
(define (consume-delimiter inp)
  (let ((ch (peek-char inp)))
    (if (char? ch)
	(if (comma-delimiter? ch)
	    (begin
	      (read-char inp)
	      #t)
	    #f)
	#f)))

(: maybe-consume-whitespace (Input-Port -> Void))
(define (maybe-consume-whitespace inp)
  (let loop ((ch (peek-char inp)))
    (if (eof-object? ch)
	(void)
	(if (whitespace? ch)
	    (begin
	      (read-char inp)
	      (loop (peek-char inp)))
	    (void)))))

(: read-quoted-field (Input-Port Output-Port -> String))
(define (read-quoted-field inp outp)
  (read-char inp) ;; toss opening quote
  (let: loop : String ((ch : (U Char EOF) (read-char inp)))
	(if (eof-object? ch)
	    (trim-right (get-output-string outp)) ;; no closing quote, best effort.
	    (if (double-quote? ch)
		(let ((next-ch (peek-char inp)))
		  (if (eof-object? next-ch)
		      (trim-right (get-output-string outp))
		      (if (double-quote? next-ch)
			  (begin
			    (read-char inp)        ;; toss 2nd of "" in a row
			    (display next-ch outp) ;; write out just one "
			    (loop (read-char inp)))
			  (trim-right (get-output-string outp)))))
		(begin
		  (display ch outp)
		  (loop (read-char inp)))))))

(: trim-right (String -> String))
(define (trim-right s)
  (let ((idx (string-length s)))
    (if (and (> idx 0)
	     (not (whitespace? (string-ref s (sub1 idx))))) ;; fast path
	s
	(let loop ((idx idx))
	  (if (zero? idx)
	      s
	      (if (whitespace? (string-ref s (sub1 idx)))
		  (loop (sub1 idx))
		  (substring s 0 idx)))))))

(: read-unquoted-field (Input-Port Output-Port -> String))
(define (read-unquoted-field inp outp)
  (let: loop : String ((ch : (U Char EOF) (peek-char inp)))
	(if (eof-object? ch)
	    (get-output-string outp)
	    (if (comma-delimiter? ch)
		(trim-right (get-output-string outp))
		(begin
		  (display ch outp)
		  (read-char inp)
		  (loop (peek-char inp)))))))

(: read-field (Input-Port -> String))
(define (read-field inp)
  (define outp (open-output-string))
  (let ((ch (peek-char inp)))
    (cond
     ((eof-object? ch)
      (get-output-string outp))
     ((double-quote? ch)
      (read-quoted-field inp outp))
     (else (read-unquoted-field inp outp)))))

(: parse-csv-line LineParser)
(define (parse-csv-line line)
  (let ((inp (open-input-string line)))
    (maybe-consume-whitespace inp)
    (let loop ((fields (list (read-field inp))))
      (maybe-consume-whitespace inp)
      (if (consume-delimiter inp)
	  (loop (cons (read-field inp) fields))
	  (reverse fields)))))

(: read-csv-file (FilePath Boolean FrameBuilder -> FrameBuilder))
(define (read-csv-file fpath header? frame-builder)
  (read-formatted-file fpath header?
		       (frame-builder-tank frame-builder parse-csv-line)))
