#lang sicp

(#%require "controller-syntax.rkt")

(define (make-primitive-exp exp machine)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((register-exp? exp)
         (let ((r ((machine 'get-register) (register-exp-reg exp))))
           (lambda () (r 'get))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))


(define (make-operation-exp exp machine)

  (define (lookup-prim symbol)
    (let ((val (assoc symbol (machine 'operations))))
      (if val
          (cdr val)
          (error "Unknown operation -- ASSEMBLE" symbol))))
  
  (let ((op (lookup-prim (operation-exp-op exp)))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine))
              (operation-exp-operands exp))))
    (lambda ()
      (let ((operands (map (lambda (p) (p)) aprocs)))
        (apply op operands)))))


(#%provide (all-defined))
