#lang sicp

(#%require "controller-syntax.rkt")

(define (make-label-entry label-name inst-seq label-regs max-breakpoint)
  (list label-name inst-seq label-regs max-breakpoint))

(define (get-label label-entry)
  (car label-entry))

(define (get-inst-seq label-entry)
  (cadr label-entry))

(define (get-label-regs label-entry)
  (caddr label-entry))

(define (get-max-breakpoint label-entry)
  (cadddr label-entry))

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

;the labels are gathered in assoc-list by extract-labels
(define (dispatch-labels labels-assoc)
  
  (define (get-label-entry label)
    (let ((entry (assoc label labels-assoc)))
      (if entry
          entry
          (error "Label entry not found -- LABEL-LOOKUP." label))))
  
  (lambda (msg)
    (cond ((eq? msg 'get-all-labels)
           (map get-label labels-assoc))
            
          ((eq? msg 'lookup)
           (lambda (label)
             (get-inst-seq (get-label-entry label))))

          ((eq? msg 'lookup-regs)
           (lambda (label)
             (get-label-regs (get-label-entry label))))

          ((eq? msg 'max-breakpoint)
           (lambda (label)
             (get-max-breakpoint (get-label-entry label))))

          (else (error "Unknown msg -- LABELS TABLE" msg)))))

(define (get-labels-dispatch ctrl-seq inst-seq)
  (dispatch-labels (extract-labels ctrl-seq inst-seq)))

(#%provide get-labels-dispatch)
