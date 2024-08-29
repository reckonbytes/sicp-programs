#lang sicp

(#%require "make-new-machine.rkt"
           "get-machine-specs.rkt"
           "assembler.rkt"
           "filter.rkt")

(define (make-machine controller-text . init-cmds)
  
  (let ((machine (make-new-machine))
        (specs (get-machine-specs controller-text)))

    ((machine 'install-specs) specs)
      
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              (specs 'regs))

    (for-each (lambda (cmd)
                (if (pair? cmd)
                    (apply (machine (car cmd)) (cdr cmd))
                    (machine cmd)))
              init-cmds)

    ;Ops not installed by init-cmds are taken from scheme-report-environment
    ((machine 'install-operations)
     (map (lambda (op)
            (cons op (eval op (scheme-report-environment 5))))
          
          (filter (lambda (op) (not (assoc op (machine 'operations))))
                  ((machine 'specs) 'ops))))
      
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
      
    machine)) 

(#%provide make-machine)
