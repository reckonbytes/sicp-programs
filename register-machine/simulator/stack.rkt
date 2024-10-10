#lang sicp

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (curr-depth 0)
        (trace #f))

    (define (do-trace)
      (display "\nStack:\n")
      (for-each (lambda (x)
                  (display x)
                  (newline))
                s)
      (display "<stack-end>\n"))

    (define (set-stack val)
      (set! s val)
      (if trace (do-trace)))
    
    (define (push x)
      (set-stack (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! curr-depth (+ 1 curr-depth))
      (set! max-depth (max curr-depth max-depth))
      'ok)
    
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set-stack (cdr s))
            (set! curr-depth (- curr-depth 1))
            top)))
    
    (define (initialize)
      (set-stack '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! curr-depth 0)
      'intialization-done)
    
    (define (print-statistics)
      (for-each display (list "\nStack-statistics:\n"
                              'curr-depth " = " curr-depth "\n"
                              'number-pushes " = " number-pushes "\n"
                              'max-depth " = " max-depth "\n")))
    
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))

            ((eq? message 'get-stat)
             (lambda (stat)
               (cond ((eq? stat 'max-depth) max-depth)
                     ((eq? stat 'curr-depth) curr-depth)
                     ((eq? stat 'number-pushes) number-pushes)
                     (else (error "Unknown stat -- STACK" stat)))))
            
            ((eq? message 'print-statistics)
             (print-statistics))

            ((eq? message 'trace-on) (set! trace #t))
            ((eq? message 'trace-off) (set! trace #f))

            ((eq? message 'list)
             (map (lambda (x) x) s))
            
            (else
             (error "Unknown request -- STACK" message))))
    
    dispatch))                 

(#%provide make-stack)
