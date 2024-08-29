#lang sicp

(#%require "register.rkt"
           (only "stack.rkt" make-label-stack))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-label-stack))
        (the-instruction-sequence '()))
    
    (let ((the-ops (list ))
          (register-table
           (list (list 'pc pc) (list 'flag flag)))
          (labels '())
          (instruction-count 0)
          (trace 'off)
          (specs '())
          (in-regs '())
          (out-regs '()))

      (define (reset-machine)
        (stack 'initialize)
        (for-each (lambda (reg) (reg 'reset))
                  (map cadr register-table))
        (set! instruction-count 0))
      
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register -- MACHINE" name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register -- MACHINE" name))))
      
      (define (execute)
        (let ((insts (pc 'get)))
          (if (null? insts)
              'execution-complete
              (let ((next-inst (car insts)))
                (if (eq? trace 'on)
                    (begin (newline)
                           (display (next-inst 'text))
                           (newline)))
                ((next-inst 'exec-proc))
                (set! instruction-count (+ 1 instruction-count))
                (execute)))))
      
      (define (dispatch message)
        (cond ((eq? message 'start)
               ((pc 'set) the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))

              ((eq? message 'reset) (reset-machine))

              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)

              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'operations) the-ops)

              ((eq? message 'stack) stack)
              
              ((eq? message 'advance-pc) ((pc 'set) (cdr (pc 'get))))

              ((eq? message 'install-labels)
               (lambda (labels-table) (set! labels labels-table)))                                           
              ((eq? message 'lookup-label)
               (lambda (label) ((labels 'lookup-label) label)))
              ((eq? message 'lookup-label-regs)
               (lambda (label) ((labels 'lookup-label-regs) label)))

              ((eq? message 'install-specs)
               (lambda (mach-specs) (set! specs mach-specs)))
              ((eq? message 'specs) specs)

              ((eq? message 'set-in-regs)
               (lambda (reg . regs) (set! in-regs (cons reg regs))))
              ((eq? message 'set-out-regs)
               (lambda (reg . regs) (set! out-regs (cons reg regs))))
              ((eq? message 'get-in-regs) in-regs)
              ((eq? message 'get-out-regs) out-regs)

              ((eq? message 'reset-instruction-count) (set! instruction-count 0))
              ((eq? message 'get-instruction-count) instruction-count)

              ((eq? message 'trace-on) (set! trace 'on)
                                       (for-each display (list "\nTrace:" trace "\n")))
              ((eq? message 'trace-off) (set! trace 'on)
                                        (for-each display (list "\nTrace:" trace "\n")))
              ((eq? message 'trace-on-regs)
               (lambda (reg-names)
                 (for-each (lambda (r) ((lookup-register r) 'trace-on))
                           reg-names)))
              ((eq? message 'trace-off-regs)
               (lambda (reg-names)
                 (for-each (lambda (r) ((lookup-register r) 'trace-off))
                           reg-names)))

              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(#%provide make-new-machine)
