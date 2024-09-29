#lang sicp

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (let ((operands (cdr operation-exp)))
    (for-each (lambda (r)
                (or (register-exp? r)
                    (constant-exp? r)
                    (error "Operations can be used only with register-exp and constant-exp. -- ASSEMBLE."
                           r)))
              operands)
    operands))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (branch-label branch-instruction)
  (cadr branch-instruction))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (save-exp-label exp) (cadr exp))
(define (save-exp-reg-names exp) (cddr exp))

(define (restored-regs-exp? exp) (tagged-list? exp 'restored-regs))
(define (restored-reg-names exp)
  (if (restored-regs-exp? exp)
      (cdr exp)
      '()))

(define (label-inst? inst)
  (or (symbol? inst)
      (restored-regs-exp? inst)))
(define label? symbol?)

(define (perform-action inst) (cdr inst))

(#%provide (all-defined-except tagged-list?))
