#lang sicp

(#%require "../simulator/make-machine.rkt"
           "../controllers/ec-eval.rkt"
           "../controllers/ec-eval-operations.rkt"
 )

(define test-mach
  (make-machine ec-eval ec-eval-ops-assoc))

(test-mach 'trace-on)

(for-each (lambda (reg)
            (((test-mach
               'get-register) reg) 'trace-on))
          '(
            ;exp val argl proc
            ))
;((test-mach 'stack) 'trace-on)

((test-mach 'set-breakpoint) 'ev-application 3)
;((test-mach 'set-breakpoint) 'ev-compile 3)

((((test-mach 'get-register) 'exp) 'set)
 '(begin
    (compile
     (define (factorial x)
       (if (= x 0)
           1
           (* x (factorial (- x 1))))))
              
    (factorial 3)))

(test-mach 'start)

(test-mach 'step)
; p = proceed (till next breakpoint)
; s = step (execute 1 instruction)
; x = quit stepping

; EC-Eval input: exit
; (Input 'exit' to exit the REPL)
