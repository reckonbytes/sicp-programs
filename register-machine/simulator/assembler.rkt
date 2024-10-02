#lang sicp

(#%require "execution-procedures-for-instructions.rkt"
           "filter.rkt"
           "controller-syntax.rkt"
           "labels.rkt")

(define (make-instruction text)
  (let ((inst (list text '())))
    (lambda (msg)
      (cond ((eq? msg 'text) (car inst))
            ((eq? msg 'exec-proc) (cadr inst))
            ((eq? msg 'set-exec-proc)
             (lambda (proc) (set-car! (cdr inst) proc)))
            (else (error "Unknown msg -- INSTRUCTION" msg))
            ))))

(define (update-inst-seq! inst-seq machine)
  (for-each (lambda (inst)
              (if (not (label? inst))
                  ((inst 'set-exec-proc)
                   (make-execution-procedure (inst 'text) machine))))
     inst-seq))

(define (assemble controller-text machine)
  (let* ((ctrl-seq (map (lambda (inst)
                          (if (label-inst? inst)
                              inst
                              (make-instruction inst)))
                        controller-text))

         (inst-seq (filter (lambda (i) (not (restored-regs-exp? i)))
                           ctrl-seq)))

    ((machine 'add-labels) (extract-labels ctrl-seq inst-seq))
    
    (update-inst-seq! inst-seq machine)

    inst-seq))

(#%provide assemble)
