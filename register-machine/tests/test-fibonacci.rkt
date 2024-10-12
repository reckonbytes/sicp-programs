#lang sicp

(#%require "make-test-machine.rkt"
           "../controllers/fibonacci.rkt"
           (rename math/number-theory
                   rk-fibonacci fibonacci))

(define init-cmds (append (list
                           ;'trace-on
                           ;'(stack (trace-on))
                           )
                          (map (lambda (reg)
                                 `(get-register (,reg) (trace-on)))
                               '(
                                 ;n val
                                 ))
                          ))

(define fib-tester
  (apply make-test-machine
         (cons fibonacci-controller
               (cons '()
                     init-cmds))))

((fib-tester 'simulated-fn) rk-fibonacci '(n))

(for-each (lambda (input)
            ((fib-tester 'test)
             `(inputs (n ,input))))
          (list 5 8 (random 20)))

(define fib-mach (fib-tester 'machine))

((((fib-mach 'get-register) 'n) 'set) 3)

((fib-mach 'set-breakpoint) 'leaf-case 1)

(fib-mach 'trace-on)
(((fib-mach 'get-register) 'val) 'trace-on)
(fib-mach 'start)

(fib-mach 'step)
; p = proceed (till next breakpoint)
; s = step (execute 1 instruction)
; x = quit stepping
