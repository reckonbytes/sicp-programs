#lang sicp

(#%require "controller-syntax.rkt"
           "table.rkt"
           "filter.rkt"
           "remove-duplicates.rkt")


(define (add-to-table table key . add-vals)
  (let* ((prev-vals ((table 'lookup) key))

         (new-vals (remove-duplicates
                    (append (or prev-vals '()) add-vals))))

    ((table 'insert!) key new-vals)))


(define (make-specs)
  (let ((tb (make-table))
        (reg-srcs-tb (make-table)))

    (for-each (lambda (key)
                ((tb 'insert!) key '()))
              '(regs ops inst-types labels))
    
    (lambda (msg)
      (cond ((eq? msg 'add-spec)
             (lambda (spec . vals)
               (if (eq? spec 'reg-sources)
                   (apply add-to-table
                          (cons reg-srcs-tb vals))
                   (apply add-to-table
                          (cons tb (cons spec vals))))))

            ((eq? msg 'reg-sources)
             (lambda (reg-name)
               ((reg-srcs-tb 'lookup) reg-name)))

            ((memq msg '(regs ops inst-types labels))
             ((tb 'lookup) msg))

            (else (error "Unknown msg to specs" msg))))))


(define (get-machine-specs controller-text)
  (let ((specs (make-specs)))
    
    (for-each (lambda (inst)
                (for-each (lambda (spec-vals)
                            (apply (specs 'add-spec) spec-vals))
                          (analyze inst)))
              controller-text)

    specs))


(define (analyze inst)
  (cond ((not (pair? inst))
         (list (list 'labels inst)))

        ((restored-regs-exp? inst)
         (list (cons 'regs (restored-reg-names inst))))

        (else

         (let* ((inst-type (car inst))
                (specs-list (list (list 'inst-types inst-type)))
                (add-specs (lambda (list1 . lists)
                             (set! specs-list
                                   (append (cons list1 lists)
                                           specs-list)))))

           (cond ((eq? inst-type 'assign)
                  (let ((a-reg (assign-reg-name inst))
                        (a-val-exp (assign-value-exp inst)))
                 
                    (add-specs (list 'regs a-reg)
                               (list 'reg-sources a-reg a-val-exp))
                    (apply add-specs
                           (if (operation-exp? a-val-exp)
                               (analyze-op-exp a-val-exp)
                               (analyze-inputs a-val-exp)))))
        
                 ((eq? inst-type 'test)
                  (apply add-specs (analyze-op-exp (test-condition inst))))
        
                 ((eq? inst-type 'branch)
                  (add-specs (list 'labels (branch-label inst))))
        
                 ((eq? inst-type 'goto)
                  (let ((dest (goto-dest inst)))
                    (cond ((label? dest) (add-specs (list 'labels dest)))
                          ((register-exp? dest)
                           (add-specs (list 'regs (register-exp-reg dest))))
                          (else (error "Goto destination neither a label nor a register expression. -- GET-MACHINE-SPECS" dest)))))

                 ((eq? inst-type 'label-save)
                  (add-specs (list 'labels (save-exp-label inst))
                             (cons 'regs (save-exp-reg-names inst))))

                 ((eq? inst-type 'label-restore))
         
                 ((eq? inst-type 'perform)
                  (apply add-specs (analyze-op-exp (perform-action inst))))
        
                 (else (error "Unknown inst-type -- GET-MACHINE-SPECS"
                              inst-type)))

           specs-list)
         )))


(define (analyze-inputs . inputs)
  (list (cons 'regs (map register-exp-reg (filter register-exp? inputs)))))
           
         
(define (analyze-op-exp exp)
  (cons (list 'ops (operation-exp-op exp))
        (apply analyze-inputs (operation-exp-operands exp))))
  

(#%provide get-machine-specs)
