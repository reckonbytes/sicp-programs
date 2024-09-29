#lang sicp

(#%require "controller-syntax.rkt"
           "execution-procedures-for-subexpressions.rkt")

(define (make-execution-procedure inst machine)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine))
        ((eq? (car inst) 'test)
         (make-test inst machine))
        ((eq? (car inst) 'branch)
         (make-branch inst machine))
        ((eq? (car inst) 'goto)
         (make-goto inst machine))
        ((eq? (car inst) 'label-save)
         (make-save inst machine))
        ((eq? (car inst) 'label-restore)
         (make-restore inst machine))
        ((eq? (car inst) 'perform)
         (make-perform inst machine))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(define (make-assign inst machine)
  (let ((target
         ((machine 'get-register) (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp value-exp machine)
               (make-primitive-exp (car value-exp) machine))))
      (lambda () ; execution procedure for assign
        ((target 'set) (value-proc))
        (machine 'advance-pc)))))

(define (make-test inst machine)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp condition machine)))
          (lambda ()
            ((((machine 'get-register) 'flag)
              'set)
             (condition-proc))
            (machine 'advance-pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (make-branch inst machine)
  (let ((insts
         ((machine 'lookup-label) (branch-label inst))))
    (lambda ()
      (if (((machine 'get-register) 'flag) 'get)
          ((((machine 'get-register) 'pc) 'set) insts)
          (machine 'advance-pc)))))

(define (make-goto inst machine)
  (let ((dest (goto-dest inst)))
    (if (register-exp? dest)
        (let ((reg ((machine 'get-register)
                    (register-exp-reg dest))))
          (lambda ()
            ((((machine 'get-register) 'pc) 'set)
             ((machine 'lookup-label) (reg 'get)))))

        (let ((insts ((machine 'lookup-label) dest)))
          (lambda () ((((machine 'get-register) 'pc) 'set) insts))))))

(define (make-perform inst machine)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp action machine)))
          (lambda ()
            (action-proc)
            (machine 'advance-pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))


(define (make-stack-entry name val) (cons name val))
(define (get-entry-name stack-entry) (car stack-entry))
(define (get-entry-value stack-entry) (cdr stack-entry))

;(label-save <label> <reg1> <reg2> ... <regn> )
(define (make-save inst machine)
  (let* ((regs-to-save (save-exp-reg-names inst))
         (label (save-exp-label inst))
         (label-insts ((machine 'lookup-label) label))
         (restore-inst-regs ((machine 'lookup-label-regs) label)))

    (if (not (equal? regs-to-save restore-inst-regs))
        (error "Regs to save different from restore instruction -- ASSEMBLE"
               'label label
               'regs-to-save regs-to-save
               'regs-to-restore restore-inst-regs)

        (let ((regs (map (machine 'get-register) regs-to-save))
              (stack (machine 'stack)))
          (lambda ()
            (apply (stack 'label-push)
                   (map make-stack-entry
                        (cons label regs-to-save)
                        (cons label-insts
                              (map (lambda (reg) (reg 'get)) regs))))
            (machine 'advance-pc))))))

;(label-restore)
(define (make-restore inst machine)
  (lambda ()
    (let* ((restored-entries ((machine 'stack) 'label-pop))
           (label-entry (car restored-entries))
           (label-insts (get-entry-value label-entry))
           (reg-entries (cdr restored-entries))
           (reg-names (map get-entry-name reg-entries)))
      
      ((((machine 'get-register) 'pc) 'set) label-insts)
          
      (for-each (lambda (reg-name reg-val)
                  ((((machine 'get-register) reg-name)
                    'set) reg-val))
                reg-names
                (map get-entry-value reg-entries)))))

(#%provide make-execution-procedure)
