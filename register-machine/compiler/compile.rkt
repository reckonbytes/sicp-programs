#lang sicp

(#%require "../controllers/ec-eval-operations.rkt"
           "make-label.rkt" 
           "inst-seq-fns.rkt"
           )

(define (compile exp)
  (get-seq-stmts (recr-compile exp '())))

(define all-regs '(val proc argl env))

(define (recr-compile exp link-list)
  
  (define (get-jump-link jump-label)
    (if (null? link-list)
        `((goto ,jump-label))
        link-list))
        
  (cond ((self-evaluating? exp)
         `((assign val (const ,exp))
           ,@link-list))

        ((variable? exp)
         `((assign val
                   (op lookup-variable-value)
                   (const ,exp) (reg env))
           ,@link-list))

        ((quoted? exp)
         `((assign val
                   (op text-of-quotation)
                   (const ,exp))
           ,@link-list))

        ((assignment? exp)
         (let ((var (assignment-variable exp))
               (aval (assignment-value exp)))

           (preserve-append-seqs
            'value-to-set-evald '(env)

            (recr-compile aval '())
            
            `((perform (op set-variable-value!)
                       (const ,var) (reg val) (reg env))
              (assign val (const ok))
              ,@link-list))))

        ((definition? exp)
         (let ((var (definition-variable exp))
               (dval (definition-value exp)))
          
           (preserve-append-seqs
            'value-to-define-evald '(env)
            
            (recr-compile dval '())

            `((perform (op define-variable!)
                       (const ,var) (reg val) (reg env))
              (assign val (const ok))
              ,@link-list))))
        
        ((if? exp)
         (let ((t-label (make-label 'if-true))
               (if-end (make-label 'if-end)))

           (let ((alt-seq (recr-compile (if-alternative exp)
                                        (get-jump-link if-end)))

                 (conseq-seq (append-seqs
                              `(,t-label)
                              (recr-compile (if-consequent exp) link-list))))
             
             (preserve-append-seqs
              'if-predicate-evald '(env)

              (recr-compile (if-predicate exp) '())
              
              (append-seqs
               `((test (op true?) (reg val))
                 (branch ,t-label))
               (append-seq-alternatives alt-seq conseq-seq)
               `(,if-end))))))

        
        ((begin? exp)
         (let ((exp-seq (begin-actions exp)))
           (if (null? exp-seq)
               `(,@link-list)
               (compile-sequence exp-seq link-list))))        
        
        ((lambda? exp)
         (let ((proc-entry (make-label 'proc-entry))
               (proc-end (make-label 'proc-end))
               (params (lambda-parameters exp))
               (body (lambda-body exp)))

           (append-seqs
            `((assign val
                      (op make-compiled-procedure)
                      (const ,proc-entry) (reg env))
              ,@(get-jump-link proc-end))

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
            
              (if (null? body)
                  '((label-restore))
                  (get-seq-stmts
                   (compile-sequence body '((label-restore)))))
            
              `(,proc-end)))
            )))
        
        ((application? exp)
         (let* ((operands-seq (compile-operands-seq (operands exp)))

                (primitive-branch (make-label 'primitive-branch))
                (compiled-branch (make-label 'compiled-branch))
                (compound-branch (make-label 'compound-branch))
                (after-call (make-label 'after-call))

                (test-seq `((test (op primitive-procedure?) (reg proc))
                            (branch ,primitive-branch)
                            (test (op compiled-procedure?) (reg proc))
                            (branch ,compiled-branch)
                            (test (op scheme-procedure?) (reg proc))
                            (branch ,primitive-branch)
                            (test (op compound-procedure?) (reg proc))
                            (branch ,compound-branch)))

                (primitive-branch-seq `(,primitive-branch
                                        (assign val (op apply-primitive-procedure)
                                                (reg proc) (reg argl))
                                        ,@(get-jump-link after-call)))

                (pre-call-label-save (cond ((null? link-list)
                                            `((label-save ,after-call)))

                                           ((eq? (caar link-list) 'goto)
                                            `((label-save ,(cadar link-list))))

                                           (else '())))

                (compiled-branch-seq (append-seqs
                                      `(,compiled-branch
                                        ,@pre-call-label-save)
                                      (make-inst-seq
                                       '(proc) all-regs
                                       `((assign val (op compiled-procedure-entry) (reg proc))
                                         (goto (reg val))))))
                
                (compound-branch-seq `(,compound-branch
                                       (perform (op display)
                                                (const "\nALERT: Application procedure is not compiled. So, NOT COMPILING application, EVALUATING it. -- COMPILER.\n"))
                                       ,@pre-call-label-save
                                       (goto compound-apply)))
                )
           
           (preserve-append-seqs
            'proc-evald '(env)

            (recr-compile (operator exp) '())

            (append-seqs

             '((assign proc (reg val)))

             (preserve-append-seqs
              'args-evald '(proc)

              operands-seq

              (append-seqs test-seq
                           (append-seq-alternatives
                            primitive-branch-seq
                            compiled-branch-seq
                            compound-branch-seq)
                           `(,after-call)))))))
        
        (else (error "Unknown exp -- COMPILE" exp))))

(define (compile-sequence seq link-list)
  (if (null? (cdr seq))
      (recr-compile (car seq) link-list)

      (preserve-append-seqs
       'seq-exp-evaluated '(env)
       (recr-compile (car seq) '())
       (compile-sequence (cdr seq) link-list))))


(define (compile-operands-seq ops)
  (let* ((first-operand-seq
          (if (null? ops)
              '((assign argl (op empty-arglist)))

              (append-seqs (recr-compile (car ops) '()) 
                           '((assign argl (op list) (reg val))))))
        
         (rest-operands-seq-list
          (map (lambda (arg)
                 (preserve-append-seqs 'arg-evald '(argl)
                                       (recr-compile arg '())
                                       '((assign argl (op adjoin-arg)
                                                 (reg val) (reg argl)))))
               (if (null? ops) '() (cdr ops))))

         (op-seq-list (cons first-operand-seq
                            rest-operands-seq-list)))
        
    (define (recr-append seqs)
      (if (null? (cdr seqs))
          (car seqs)
        
          (preserve-append-seqs
           'arg-added-to-argl '(env)
           (car seqs) (recr-append (cdr seqs)))))
    
    (if (null? op-seq-list)
        '()
        (recr-append op-seq-list))))

(#%provide compile)
