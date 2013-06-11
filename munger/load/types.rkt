#lang typed/racket/base

(provide
 Line Line?
 LineParser)

(define-type Line String)
(define-type LineParser (Line -> (Listof String)))

(define Line? string?)

