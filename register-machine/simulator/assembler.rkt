#lang sicp

(#%require "execution-procedures-for-instructions.rkt"
           "filter.rkt"
           "controller-syntax.rkt"
           "labels.rkt")

(define (make-instruction text)
  (let ((inst (cons text '())))
    (lambda (msg)
      (cond ((eq? msg 'text) (car inst))
            ((eq? msg 'exec-proc) (cdr inst))
            ((eq? msg 'set-exec-proc)
             (lambda (proc) (set-cdr! inst proc)))))))

(define (update-inst-seq! inst-seq machine)
  (for-each (lambda (inst)
              ((inst 'set-exec-proc)
               (make-execution-procedure (inst 'text) machine)))
     inst-seq))

(define (assemble controller-text machine)
  (let* ((inst-text (map (lambda (inst)
                           (if (label-inst? inst)
                               inst
                               (make-instruction inst)))
                         controller-text))

         (inst-seq (filter (lambda (i) (not (label-inst? i)))
                           inst-text)))

    ((machine 'install-labels) (get-labels-table inst-seq inst-text))
    
    (update-inst-seq! inst-seq machine)

    inst-seq))

(#%provide assemble)
