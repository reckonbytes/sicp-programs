#lang sicp

(#%require "../controllers/ec-eval-operations.rkt"
           "make-label.rkt"
           )

(define (get-preserved-stmts label regs stmts)
  (let ((plabel (make-label label)))
    (append (list (append `(label-save ,plabel) regs))
            stmts
            `((label-restore)
              ,plabel
              ,(cons 'restored-regs regs)))))

(define (get-preserved-seq-stmts label regs stmts-list)
  (cond ((null? stmts-list) '())
        ((null? (cdr stmts-list)) (apply append stmts-list))

        (else (append (get-preserved-stmts label regs (car stmts-list))
                      (get-preserved-seq-stmts label regs (cdr stmts-list))))))

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
           
           (append (get-preserved-stmts
                    'value-to-set-evald
                    '(env) (recr-compile aval))
                   
                   `((perform (op set-variable-value!)
                              (const ,var) (reg val) (reg env))
                     (assign val (const ok))))))

        ((definition? exp)
         (let ((var (definition-variable exp))
               (dval (definition-value exp)))
           
           (append (get-preserved-stmts
                    'value-to-define-evald
                    '(env) (recr-compile dval))
                   
                   `((perform (op define-variable!)
                              (const ,var) (reg val) (reg env))))))

        ((if? exp)
         (let ((t-label (make-label 'if-true))
               (if-end (make-label 'if-end)))
           
           (append (get-preserved-stmts
                    'if-predicate-evald
                    '(env) (recr-compile (if-predicate exp)))
                   
                   `((test (op true?) (reg val))
                     (branch ,t-label))
                   
                   (recr-compile (if-alternative exp))
                   
                   `((goto ,if-end)
                     ,t-label)
                   
                   (recr-compile (if-consequent exp))
                   `(,if-end))))
        
        ((begin? exp)
         (let* ((exp-seq (begin-actions exp))
                (compiled-stmts-list (map recr-compile exp-seq)))
           (get-preserved-seq-stmts
            'seq-exp-evaluated
            '(env) compiled-stmts-list)))
        
        ((lambda? exp)
         (let ((proc-entry (make-label 'proc-entry))
               (proc-end (make-label 'proc-end))
               (params (lambda-parameters exp))
               (body (lambda-body exp)))
           
           (append `((assign val
                             (op make-compiled-procedure)
                             (const ,proc-entry) (reg env))
                     (goto ,proc-end)
                     ,proc-entry
                     (assign env (op compiled-procedure-env) (reg proc))
                     (assign env
                             (op extend-environment)
                             (const ,params)
                             (reg argl)
                             (reg env)))
                   
                   (recr-compile (cons 'begin body))

                   `((label-restore)
                     ,proc-end))))
        
        ((application? exp)
         (let* ((ops (operands exp))

                (first-operand-stmts
                 (if (null? ops)
                     '((assign argl (op empty-arglist)))

                     (append (recr-compile (car ops)) 
                             '((assign argl (op list) (reg val))))))
                
                (rest-operands-stmts
                 (map (lambda (arg)
                        (append (get-preserved-stmts 'arg-evald
                                                     '(argl) (recr-compile arg))
                                '((assign argl (op adjoin-arg)
                                          (reg val) (reg argl)))))
                     
                      (if (null? ops) '() (cdr ops))))

                (operands-stmts
                 (get-preserved-seq-stmts
                  'arg-added-to-argl
                  '(env) (cons first-operand-stmts
                               rest-operands-stmts)))

                (primitive-branch (make-label 'primitive-branch))
                (compiled-branch (make-label 'compiled-branch))
                (compound-branch (make-label 'compound-branch))
                (after-call (make-label 'after-call)))
           
           (append (get-preserved-stmts 'proc-evald
                                        '(env) (recr-compile (operator exp)))
                   
                   '((assign proc (reg val)))

                   (get-preserved-stmts 'args-evald
                                        '(proc) operands-stmts)

                   `((test (op primitive-procedure?) (reg proc))
                     (branch ,primitive-branch)
                     (test (op compiled-procedure?) (reg proc))
                     (branch ,compiled-branch)
                     (test (op scheme-procedure?) (reg proc))
                     (branch ,primitive-branch)
                     (test (op compound-procedure?) (reg proc))
                     (branch ,compound-branch)
                     
                     ,primitive-branch
                     (assign val (op apply-primitive-procedure)
                             (reg proc) (reg argl))
                     (goto ,after-call)
                     
                     ,compiled-branch
                     (label-save ,after-call)
                     (assign val (op compiled-procedure-entry) (reg proc))
                     (goto (reg val))

                     ,compound-branch
                     (label-save ,after-call)
                     (perform (op display)
                              (const "\nALERT: Application procedure is not compiled. So, NOT COMPILING application, EVALUATING it. -- COMPILER.\n"))
                     (goto compound-apply)
                     
                     ,after-call))
           ))

        (else (error "Unknown exp -- COMPILE" exp))))

(define (compile exp) (recr-compile exp))

(#%provide compile)
