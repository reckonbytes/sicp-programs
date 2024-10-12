#lang sicp

(#%require "make-test-machine.rkt"
           "../controllers/recursive-exponentiation.rkt"
 )

(define init-cmds (append (list
                           ;'trace-on
                           ;'(stack (trace-on))
                           ;'(set-breakpoint (base-case 1))
                           )
                          (map (lambda (reg)
                                 `(get-register (,reg) (trace-on)))
                               '(
                                 ;n val
                                 ))
                          ))

(define recr-expt-tester
  (apply make-test-machine
         (cons recursive-exponentiation-controller
               (cons '()
                     init-cmds))))

((recr-expt-tester 'simulated-fn) expt '(b n))

(for-each (lambda (inputs)
            ((recr-expt-tester 'test)
             (list 'inputs
                   (list 'b (car inputs))
                   (list 'n (cadr inputs)))))
          (list
           '(5 3)
           '(2 3)
           '(0 1)
           '(1 1)
           '(45 0)
           '(-2 3)
           (list (random 60) (random 60))
           ))
