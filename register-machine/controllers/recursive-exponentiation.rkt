#lang sicp

(define recursive-exponentiation-controller
  '((label-save end)

    expt-loop
    (test (op =) (reg n) (const 0))
    (branch base-case)
    (label-save after-loop)
    (assign n (op -) (reg n) (const 1))
    (goto expt-loop)

    after-loop
    (assign val (op *) (reg b) (reg val))
    (label-restore)

    base-case
    (assign val (const 1))
    (label-restore)
    
    end))

(#%provide recursive-exponentiation-controller)
