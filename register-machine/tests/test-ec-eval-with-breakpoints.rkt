#lang sicp

(#%require "test-make-machine.rkt"
           "../controllers/ec-eval.rkt"
           "../controllers/ec-eval-operations.rkt"
 )

(define ec-eval-test-mach
  (apply make-test-machine
         (list ec-eval ec-eval-ops-assoc)))

((ec-eval-test-mach 'simulated-fn)
 (lambda (exp) (eval exp (scheme-report-environment 5)))
 '(exp))

((ec-eval-test-mach 'machine) 'trace-on)
(for-each (lambda (reg)
            ((((ec-eval-test-mach 'machine)
               'get-register) reg) 'trace-on))
          '(exp val))

(((ec-eval-test-mach 'machine) 'set-breakpoint) 'ev-definition 1)

((ec-eval-test-mach 'test)
 `(ignore-breakpoints ,false)
 '(inputs (exp (begin
                 (define (factorial x)
                   (if (= x 0)
                       1
                       (* x (factorial (- x 1)))))
              
                 (factorial 4)))))

(ec-eval-test-mach 'step)
; p = proceed (till next breakpoint)
; s = step (execute 1 instruction)
; x = quit stepping
