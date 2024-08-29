#lang sicp

(define fibonacci-controller
  '((label-save end)

    branch-loop
    (test (op <) (reg n) (const 2))
    (branch leaf-case)
    (assign n (op -) (reg n) (const 1))
    (label-save other-branch n)
    (assign n (op -) (reg n) (const 1))
    (goto branch-loop)

    other-branch (restored-regs n)
    (assign val1 (reg val))
    (label-save sum-branches val1)
    (goto branch-loop)

    sum-branches (restored-regs val1)
    (assign val (op +) (reg val) (reg val1))
    (label-restore)

    leaf-case
    (assign val (reg n))
    (label-restore)

    end))

(#%provide fibonacci-controller)
