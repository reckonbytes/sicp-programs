#lang sicp

(#%require "test-make-machine.rkt"
           "../controllers/fibonacci.rkt"
           (rename math/number-theory
                   rk-fibonacci fibonacci))

(define init-cmds (append (list
                           'trace-on
                           ;'(stack (trace-on))
                           '(set-breakpoint (leaf-case 1))                           
                           )
                          (map (lambda (reg)
                                 `(get-register (,reg) (trace-on)))
                               '(
                                 n val
                                 ))
                          ))

(define fib-tester
  (apply make-test-machine
         (cons fibonacci-controller
               (cons '()
                     init-cmds))))

((fib-tester 'simulated-fn) rk-fibonacci '(n))

((fib-tester 'test)
 `(inputs (n 3))
 `(ignore-breakpoints ,false))

(fib-tester 'step)
; p = proceed (till next breakpoint)
; s = step (execute 1 instruction)
; x = quit stepping
