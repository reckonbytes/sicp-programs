#lang sicp

(define (make-stack-entry name val) (cons name val))
(define (get-entry-name stack-entry) (car stack-entry))
(define (get-entry-value stack-entry) (cdr stack-entry))

(#%provide (all-defined))
