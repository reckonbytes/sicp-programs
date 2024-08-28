#lang sicp
(define (remove-duplicates x)
  (define (iter-proc y ans)
    (cond ((null? y) ans)
          ((member (car y) ans) (iter-proc (cdr y) ans))
          (else (iter-proc (cdr y) (cons (car y) ans)))))
  (iter-proc x '()))

(#%provide remove-duplicates)
