#lang sicp

(#%require "../controllers/ec-eval-operations.rkt"
           "make-label.rkt" 
           "inst-seq-fns.rkt"
           )

(define (compile exp)
  (get-seq-stmts (recr-compile exp)))

(define all-regs '(val proc argl env))

(define (recr-compile exp)
  (cond ((self-evaluating? exp)
         `((assign val (const ,exp))))

        ((variable? exp)
         `((assign val
                   (op lookup-variable-value)
                   (const ,exp) (reg env))))

        ((quoted? exp)
         `((assign val
                   (op text-of-quotation)
                   (const ,exp))))

        ((assignment? exp)
         (let ((var (assignment-variable exp))
               (aval (assignment-value exp)))
           (preserve-append-seqs
            'after-set-val-compile
            '(env)
            (recr-compile aval)
            `((perform (op set-variable-value!)
                       (const ,var) (reg val) (reg env))
              (assign val (const ok))))))

        ((definition? exp)
         (let ((var (definition-variable exp))
               (dval (definition-value exp)))
           (preserve-append-seqs
            'after-define-val-compile
            '(env)
            (recr-compile dval)
            `((perform (op define-variable!)
                       (const ,var) (reg val) (reg env))
              (assign val (const ok))))))

        
        ((if? exp)
         (let ((t-label (make-label 'if-true))
               (if-end (make-label 'if-end)))

           (let ((alt-seq (append-seqs
                           (recr-compile (if-alternative exp))
                           `((goto ,if-end)
                             ,t-label)))

                 (conseq-seq (append-seqs
                              (recr-compile (if-consequent exp))
                              `(,if-end))))
             
             (preserve-append-seqs
              'after-if-predicate-compile
              '(env)

              (recr-compile (if-predicate exp))
              
              (append-seqs
               `((test (op true?) (reg val))
                 (branch ,t-label))
               (append-seq-alternatives alt-seq conseq-seq))))))

        
        ((begin? exp)
         (let ((exp-seq (begin-actions exp)))
           (preserve-append-seq-list
            'after-seq-exp-compile
            '(env) (map compile exp-seq))))
        
        
        ((lambda? exp)
         (let ((proc-entry (make-label 'proc-entry))
               (proc-end (make-label 'proc-end))
               (params (lambda-parameters exp))
               (body (lambda-body exp)))

           (append-seqs
            `((assign val
                      (op make-compiled-procedure)
                      (const ,proc-entry) (reg env))
              (goto ,proc-end))

            (make-inst-seq
             '() '()
             (append
              `(,proc-entry
                (assign env (op compiled-procedure-env) (reg proc))
                (assign env
                        (op extend-environment)
                        (const ,params)
                        (reg argl)
                        (reg env)))
            
              (get-seq-stmts (recr-compile (cons 'begin body)))
            
              `((label-restore)
                ,proc-end)))
            )))
        
        ((application? exp)
         (let* ((ops (operands exp))

                (first-operand-seq
                 (if (null? ops)
                     '((assign argl (op empty-arglist)))

                     (append-seqs (recr-compile (car ops)) 
                                  '((assign argl (op list) (reg val)))))) 
                
                (rest-operands-seq
                 (map (lambda (arg)
                        (preserve-append-seqs
                         'before-argl-update
                         '(argl)
                         (recr-compile arg)
                         '((assign argl (op adjoin-arg)
                                   (reg val) (reg argl)))))
                      (if (null? ops) '() (cdr ops))))

                (operands-seq (preserve-append-seq-list
                               'after-arg-compile
                               '(env)
                               (cons first-operand-seq
                                     rest-operands-seq)))

                (primitive-branch (make-label 'primitive-branch))
                (compiled-branch (make-label 'compiled-branch))
                (after-call (make-label 'after-call))

                (test-seq `((test (op primitive-procedure?) (reg proc))
                            (branch ,primitive-branch)
                            (test (op compiled-procedure?) (reg proc))
                            (branch ,compiled-branch)
                            (test (op scheme-procedure?) (reg proc))
                            (branch ,primitive-branch)
                            (perform (op display)
                                     (const "\nERROR: Application procedure is neither primitive nor compiled. -- COMPILER.\n"))
                            (assign val (op reset-reg) (const val))
                            (goto ,after-call)))
                            
                            

                (primitive-branch-seq `(,primitive-branch
                                        (assign val (op apply-primitive-procedure)
                                                (reg proc) (reg argl))
                                        (goto ,after-call)))

                (compiled-branch-seq (append-seqs
                                      `(,compiled-branch
                                        (label-save ,after-call)
                                        (assign val (op compiled-procedure-entry) (reg proc)))

                                      (make-inst-seq
                                       '(val) all-regs
                                       `((goto (reg val))
                                         ,after-call)))))
           
           (preserve-append-seqs
            'after-operator-compile '(env)
                                 
            (recr-compile (operator exp))

            (append-seqs

             '((assign proc (reg val)))

             (preserve-append-seqs
              'after-operands-compile '(proc)

              operands-seq

              (append-seqs test-seq (append-seq-alternatives
                                     primitive-branch-seq
                                     compiled-branch-seq)))))))
        
        (else (error "Unknown exp -- COMPILE" exp))))

(#%provide compile)
