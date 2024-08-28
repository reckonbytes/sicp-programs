#lang sicp

(define (make-table)
  (let ((table (list '*table*)))

    (define (lookup key)
      (let ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            false)))

    (define (insert! key value)
      (let ((record (assoc key (cdr table))))
        (if record
            (set-cdr! record value)
            (set-cdr! table
                      (cons (cons key value) (cdr table)))))
      'ok)
  
    (lambda (msg)
      (cond ((eq? msg 'lookup) lookup)
            ((eq? msg 'insert!) insert!)
            (else (error "Unknown message -- TABLE" msg))))))

(#%provide make-table)
