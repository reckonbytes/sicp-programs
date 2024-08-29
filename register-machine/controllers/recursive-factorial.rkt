#lang sicp

(define recursive-factorial-controller
  '((label-save fact-done) ; set up final return address

    fact-loop
    (test (op =) (reg n) (const 0))
    (branch base-case)
    (label-save after-fact n)
    (assign n (op -) (reg n) (const 1))
    (goto fact-loop)

    after-fact (restored-regs n)
    (assign val (op *) (reg n) (reg val)) ; val now contains n(n - 1)!
    (label-restore) ; return to caller

    base-case
    (assign val (const 1)) ; base case: 1! = 1
    (label-restore) ; return to caller
    
    fact-done))

(#%provide recursive-factorial-controller)
