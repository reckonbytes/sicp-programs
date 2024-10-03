#lang sicp

(#%require "make-new-machine.rkt"
           "get-machine-specs.rkt"
           "assembler.rkt"
           "filter.rkt"
           "../compiler/compile.rkt")

(define (make-machine controller-text init-ops . init-cmds)
  
  (let ((machine (make-new-machine))
        (specs (get-machine-specs controller-text)))

    ((machine 'install-specs) specs)
      
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              (specs 'regs))

    (define (install-uninstalled-ops ops-list)
      (let ((uninstalled-ops (filter (lambda (op)
                                       (not (assoc op (machine 'operations))))
                                     ops-list)))
        (if (not (null? uninstalled-ops))
            (begin
              (display "\nInstalling ops from scheme-report-environment:\n")
              ((machine 'install-operations)
               (map (lambda (op)
                      (for-each display (list op " "))
                      (cons op (eval op (scheme-report-environment 5))))
                    uninstalled-ops))
              (newline)))))

    ((machine 'install-operations)
     (append (list (cons 'compile (lambda (exp) (compile exp)))
                   (cons 'assemble (lambda (compiled-exp)
                                     (let ((c-specs (get-machine-specs compiled-exp)))
                                       (install-uninstalled-ops (c-specs 'ops))
                                       (assemble compiled-exp machine)))))
             init-ops))

    ;Ops not installed by init-ops are taken from scheme-report-environment
    (install-uninstalled-ops ((machine 'specs) 'ops))
      
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))

    (define (recur-cmd cmd)
      (let ((cmd-result (apply (car cmd) (cadr cmd))))
        (if (pair? (cddr cmd))
            (recur-cmd (cons cmd-result (cddr cmd)))
            cmd-result)))
    
    (define (exec-cmd cmd)
      (cond ((symbol? cmd) (machine cmd))
            ((pair? (cdr cmd)) (recur-cmd (cons (machine (car cmd)) (cdr cmd))))
            (else (machine (car cmd)))))

    (for-each exec-cmd init-cmds)
      
    machine)) 

(#%provide make-machine)
