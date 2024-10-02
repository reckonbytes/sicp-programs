#lang sicp

(#%require "controller-syntax.rkt")

(define (make-label-entry label-name inst-seq label-regs max-breakpoint)
  (cons label-name
        (lambda (key)
          (cond ((eq? key 'inst-seq) inst-seq)
                ((eq? key 'label-regs) label-regs)
                ((eq? key 'max-breakpoint) max-breakpoint)
                (else (error "Unknown key -- LABEL-ENTRY" key))))))

(define (extract-label-regs seq)
  (cond ((null? seq) (list '() seq))
        
        ((restored-regs-exp? (car seq))
         (list (restored-reg-names (car seq))
               (cdr seq)))

        (else (list '() seq))))

(define (find-max-breakpoint label-insts)
  (if (or (null? label-insts)
          (label? (car label-insts)))
      0
      (+ 1 (find-max-breakpoint (cdr label-insts)))))

(define (extract-labels ctrl-seq inst-seq)
  (if (null? ctrl-seq)
      '()
      (let ((top (car ctrl-seq)))
        (cond ((label? top)
               (if (memq top (cdr ctrl-seq))
                   (error "Label repeated -- EXTRACT-LABELS" top))
               (let* ((data (extract-label-regs (cdr ctrl-seq)))
                      (restored-regs (car data))
                      (rest-seq (cadr data)))
            
                 (cons (make-label-entry
                        top inst-seq restored-regs (find-max-breakpoint (cdr inst-seq)))
                       (extract-labels rest-seq (cdr inst-seq)))))

              ((restored-regs-exp? top)
               (error "Restored-regs instruction must follow a label -- EXTRACT-LABELS"
                      top))

              (else
               (extract-labels (cdr ctrl-seq) (cdr inst-seq)))))))

(#%provide extract-labels)
