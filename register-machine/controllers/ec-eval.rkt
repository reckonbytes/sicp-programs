#lang sicp

;Explicit Control Evaluator's Registers:
;exp: expression to be evaluated,
;env: evaluation environment,
;val: evaluation result,
;proc: saves evaluated proc (while the arguments are evaluated),
;unev: contains unevaluated expression or unevaluated part(s) of the expression,
;argl: saves evaluated arguments.

(define ec-eval
  '(
    (label-save done)

    (assign env (op get-global-environment))

    (branch eval-dispatch)
    (test (op reg-set?) (const exp))
    (branch eval-print-loop)

    read-eval-print-loop
    (perform
     (op prompt-for-input) (const ";;; EC-Eval input:"))
    (assign exp (op read))
    (test (op eq?) (reg exp) (const exit))
    (branch done)
    eval-print-loop
    (label-save print-result)
    (goto eval-dispatch)
    
    print-result
    (perform
     (op announce-output) (const ";;; EC-Eval value:"))
    (perform (op user-print) (reg val))
    (goto read-eval-print-loop)
    
    eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch ev-self-eval)
    (test (op variable?) (reg exp))
    (branch ev-variable)
    (test (op quoted?) (reg exp))
    (branch ev-quoted)
    (test (op assignment?) (reg exp))
    (branch ev-assignment)
    (test (op definition?) (reg exp))
    (branch ev-definition)
    (test (op if?) (reg exp))
    (branch ev-if)
    (test (op lambda?) (reg exp))
    (branch ev-lambda)
    (test (op begin?) (reg exp))
    (branch ev-begin)
    (test (op compile?) (reg exp))
    (branch ev-compile)
    (test (op application?) (reg exp))
    (branch ev-application)
    (goto unknown-expression-type)

    ev-self-eval
    (assign val (reg exp))
    (label-restore)
    
    ev-variable
    (assign val (op lookup-variable-value) (reg exp) (reg env))
    (label-restore)

    ev-quoted
    (assign val (op text-of-quotation) (reg exp))
    (label-restore)

    ev-lambda
    (assign unev (op lambda-parameters) (reg exp))
    (assign exp (op lambda-body) (reg exp))
    (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
    (label-restore)

    ev-assignment
    (assign unev (op assignment-variable) (reg exp)) 
    (label-save do-assignment unev env)
    (assign exp (op assignment-value) (reg exp))
    (goto eval-dispatch) 

    do-assignment
    (restored-regs unev env)
    (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (label-restore)

    ev-definition
    (assign unev (op definition-variable) (reg exp))
    (label-save do-define unev env)
    (assign exp (op definition-value) (reg exp))
    (goto eval-dispatch) 

    do-define
    (restored-regs unev env)
    (perform (op define-variable!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (label-restore)

    ev-if
    (assign unev (reg exp))
    (label-save if-decide unev env)
    (assign exp (op if-predicate) (reg exp))
    (goto eval-dispatch)

    if-decide
    (restored-regs unev env)
    (test (op true?) (reg val))
    (branch if-true)
    (assign exp (op if-alternative) (reg unev))
    (goto eval-dispatch)

    if-true
    (assign exp (op if-consequent) (reg unev))
    (goto eval-dispatch)

    ev-begin
    (assign unev (op begin-actions) (reg exp))
    (goto ev-seq-with-env-unev)

    ev-sequence
    (restored-regs unev env)
    ev-seq-with-env-unev
    (assign exp (op first-exp) (reg unev))
    (test (op last-exp?) (reg unev))
    (branch eval-dispatch)
    (assign unev (op rest-exps) (reg unev))
    (label-save ev-sequence unev env)
    (goto eval-dispatch)

    ev-application
    (assign unev (op operands) (reg exp))
    (label-save after-proc-eval env unev)
    (assign exp (op operator) (reg exp))
    (goto eval-dispatch)

    after-proc-eval
    (restored-regs env unev)
    (assign proc (reg val))
    (label-save apply-dispatch proc)
    
    (assign argl (op empty-arglist))

    (test (op no-operands?) (reg unev))
    (branch all-operands-evald)
    (goto get-next-operand-with-env-unev)

    get-next-operand
    (restored-regs env unev)
    get-next-operand-with-env-unev
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch eval-operand)
    (assign unev (op rest-operands) (reg unev))
    (label-save get-next-operand env unev)

    eval-operand
    (label-save after-operand-eval argl)
    (goto eval-dispatch)

    after-operand-eval
    (restored-regs argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (label-restore)

    all-operands-evald
    (label-restore)

    apply-dispatch
    (restored-regs proc)
    (test (op primitive-procedure?) (reg proc))
    (branch primitive-apply)
    (test (op compound-procedure?) (reg proc))
    (branch compound-apply)
    (test (op compiled-procedure?) (reg proc))
    (branch compiled-apply)
    (test (op scheme-procedure?) (reg proc)) ;Any procedure not defined as primitive and not found in env is seeked from the scheme-report-environment.
    (branch primitive-apply)
    (goto unknown-procedure-type)

    primitive-apply
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (label-restore)

    compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
    (assign unev (op procedure-body) (reg proc))
    (goto ev-seq-with-env-unev)

    compiled-apply
    (assign exp (op compiled-procedure-entry) (reg proc))
    (goto (reg exp))

    ev-compile
    (assign exp (op exp-to-compile) (reg exp))
    (assign exp (op compile) (reg exp))
    (assign exp (op append) (reg exp) (const ((label-restore))))
    (assign exp (op assemble) (reg exp))
    (goto (reg exp))    

    unknown-expression-type
    (assign val (const unknown-expression-type-error))
    (goto signal-error)
    
    unknown-procedure-type
    (assign val (const unknown-procedure-type-error))
    (goto signal-error)
    
    signal-error
    (perform (op user-print) (reg val))
    (label-restore)
    
    done
    ))
    
(#%provide ec-eval)  
