#lang sicp

(#%require "table.rkt"
           "controller-syntax.rkt")

(define (make-label-entry label-name inst-seq label-regs)
  (list label-name inst-seq label-regs))

;the labels are gathered in assoc-list by extract-labels
;labels-table gives table interface to the labels
(define (make-labels-table labels-assoc)
  (let ((lb-tb (make-table)))

    (for-each (lambda (entry)
                ((lb-tb 'insert!) (car entry) (cdr entry)))
              labels-assoc)

    (lambda (msg)
      (cond ((eq? msg 'lookup-label)
             (lambda (label)
               (car ((lb-tb 'lookup) label))))

            ((eq? msg 'lookup-label-regs)
             (lambda (label)
               (cadr ((lb-tb 'lookup) label))))

            (else (error "Unknown msg -- LABELS TABLE" msg))))))


(define (get-labels-table inst-seq inst-text)
  (make-labels-table (extract-labels inst-seq '() inst-text)))


(define (extract-labels inst-seq label-entries inst-text)
  (if (null? inst-text)
      label-entries
      
      (let ((next-inst (car inst-text)))
        (if (symbol? next-inst)
            (if (assoc next-inst label-entries)
                (error "Label repeated -- ASSEMBLE" next-inst)

                (let* ((label-1st-inst (if (null? (cdr inst-text))
                                           '()
                                           (cadr inst-text)))
                       (regs-saved (restored-reg-names label-1st-inst)))
                  
                  (extract-labels
                   inst-seq
                   (cons (make-label-entry next-inst inst-seq regs-saved)
                         label-entries)
                   (if (restored-regs-exp? label-1st-inst)
                       (cddr inst-text)
                       (cdr inst-text)))))
            
            (extract-labels (cdr inst-seq) label-entries (cdr inst-text))))))

(#%provide get-labels-table)
