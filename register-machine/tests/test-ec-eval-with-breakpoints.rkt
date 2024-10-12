#lang sicp

(#%require "../simulator/make-machine.rkt"
           "../controllers/ec-eval.rkt"
           "../controllers/ec-eval-operations.rkt"
           rackunit
 )

(define test-mach
  (make-machine ec-eval ec-eval-ops-assoc))

;(test-mach 'trace-on)

(for-each (lambda (reg)
            (((test-mach
               'get-register) reg) 'trace-on))
          '(
            ;val argl proc unev exp env 
            ))
;((test-mach 'stack) 'trace-on)


(define (check-evaluator-tail-recursion)

  (test-mach 'repl-off)

  ((((test-mach 'get-register) 'exp) 'set)
   '(begin
    
      (define (iter-fact n ans)
        (if (= n 0)
            ans
            (iter-fact (- n 1) (* ans n))))
    
      (define (factorial x)
        (iter-fact x 1))
              
      (factorial 3)

      (factorial 15)
      ))

  (test-mach 'cancel-all-breakpoints)
  ((test-mach 'set-breakpoint) 'if-true 1)

  (test-mach 'start)
  
  ((test-mach 'cancel-breakpoint) 'if-true 1)
  ((test-mach 'set-breakpoint) 'ev-seq-with-env-unev 1)
  
  (test-mach 'proceed)

  (check-equal? (((test-mach 'get-register) 'val) 'get)
                6 )

  (define stack-max (((test-mach 'stack) 'get-stat)
                     'max-depth))

  (test-mach 'cancel-all-breakpoints)  
  (test-mach 'proceed)

  (check-equal? (((test-mach 'get-register) 'val) 'get)
                1307674368000 )

  (check-equal? (((test-mach 'stack) 'get-stat)
                 'max-depth)
                stack-max
                "Evaluator is not tail recursive.")

  "Checked evaluator for tail recursion."
  )

(check-evaluator-tail-recursion)

(define (check-compiler-tail-recursion)
  
  (test-mach 'repl-off)

  ((((test-mach 'get-register) 'exp) 'set)
   '(begin
    
      (compile
       (begin (define (iter-fact n ans)
                (if (= n 0)
                    ans
                    (iter-fact (- n 1) (* ans n))))
    
              (define (factorial x)
                (iter-fact x 1))))
              
      (factorial 3)

      (factorial 15)
      ))

  (test-mach 'cancel-all-breakpoints)
  ((test-mach 'set-breakpoint) 'ev-compile 1)

  (test-mach 'start)

  ((test-mach 'set-breakpoint) 'ev-seq-with-env-unev 1)

  (test-mach 'proceed)
  (test-mach 'proceed)

  (check-equal? (((test-mach 'get-register) 'val) 'get)
                6 )

  (define stack-max (((test-mach 'stack) 'get-stat)
                     'max-depth))

  (test-mach 'proceed)

  (check-equal? (((test-mach 'get-register) 'val) 'get)
                1307674368000 )

  (check-equal? (((test-mach 'stack) 'get-stat)
                 'max-depth)
                stack-max
                "Compiler is not tail recursive.")

  "Checked compiler for tail recursion."
  )

(check-compiler-tail-recursion)

;TESTING REPL
(test-mach 'repl-on)

((((test-mach 'get-register) 'exp) 'set)
 '(begin
    
    (define (factorial x)
      (if (= x 0)
          1
          (* x (factorial (- x 1)))))
              
    (factorial 3)))

(test-mach 'cancel-all-breakpoints)
; ((test-mach 'set-breakpoint) 'ev-application 3)
; ((test-mach 'set-breakpoint) 'ev-seq-with-env-unev 1)
; ((test-mach 'set-breakpoint) 'ev-compile 3)
; ((test-mach 'set-breakpoint) 'read-eval-print-loop 1)
; ((test-mach 'set-breakpoint) 'apply-dispatch 1)
; ((test-mach 'set-breakpoint) 'print-result 1)

(test-mach 'start)

;(test-mach 'step)
; p = proceed (till next breakpoint)
; s = step (execute 1 instruction)
; x = quit stepping

; EC-Eval input: exit
; (Input 'exit' to exit the REPL)
