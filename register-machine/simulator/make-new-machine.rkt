#lang sicp

(#%require "register.rkt"
           (only "stack.rkt" make-label-stack)
           (rename rnrs/lists-6
                   rnrs-remove remove)
           (rename rnrs/lists-6
                   rnrs-find find)
           "filter.rkt"
           )             

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-label-stack))
        (the-instruction-sequence '()))
    
    (let ((the-ops (list ))
          (register-table
           (list (list 'pc pc) (list 'flag flag)))
          (labels-alist '())
          (inst-count 0)
          (trace 'off)
          (specs '())
          (breakpoints '())
          (curr-label #f)
          (label-inst-count 0)
          (breakpoint-n #f))

      (define (advance-pc)
        ((pc 'set) (cdr (pc 'get))))

      (define (get-label-data label)
        (let ((entry (assoc label labels-alist)))
          (if entry
              (cdr entry)
              (error "Label entry not found -- MACHINE." label))))

      (define (add-labels new-alist)
        (for-each (lambda (label)
                    (if (assoc label labels-alist)
                        (error "Existing label repeated -- MACHINE." label)))
                  new-alist)
        (set! labels-alist (append new-alist labels-alist)))

      (define (set-breakpoint label n)
        (let ((max-bp ((get-label-data label) 'max-breakpoint)))
          (if (or (< n 1) (> n max-bp))
              (error "Breakpoint n must be >0 and < number of instructions before the next label. -- SET-BREAKPOINT." n)))
        
        (let ((label-bps (assoc label breakpoints)))
          (if label-bps
              (if (not (memq n label-bps))
                  (set-cdr! label-bps (cons n (cdr label-bps))))
              (set! breakpoints (cons (list label n) breakpoints)))))

      (define (cancel-breakpoint label n)
        (let ((label-bps (assoc label breakpoints)))
          (if (and label-bps (memq n label-bps))
              (set-cdr! label-bps (rnrs-remove n (cdr label-bps)))
              (error "Breakpoint not found -- CANCEL-BREAKPOINT." label n))))

      (define (get-breakpoint-n curr-count)
        (let ((bp-entry (assoc curr-label breakpoints)))
          (and bp-entry
              (rnrs-find (lambda (x) (> x curr-count))
                         (cdr bp-entry)))))
      
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

      (define (reset-machine)
        (stack 'initialize)
        (for-each (lambda (reg) (reg 'reset))
                  (map cadr register-table))
        (((lookup-register 'flag) 'set) false)
        (set! inst-count 0))
      (reset-machine)

      (define (install-operations new-ops)
        (let ((redef-ops (filter (lambda (op)
                                   (assoc op the-ops))
                                 new-ops)))
          (if (not (null? redef-ops))
              (for-each display "\nRedefined operations: " redef-ops "\n"))
          (set! the-ops (append new-ops the-ops))))

      (install-operations
       (list (cons 'reg-set?
                   (lambda (reg-name)
                     ((lookup-register reg-name) 'is-set?)))
             (cons 'reset-reg
                   (lambda (reg-name)
                     ((lookup-register reg-name) 'reset)))
             ))
      
      (define (execute-1-inst)
        (let ((insts (pc 'get)))
          
          (if (null? insts)
              'no-more-insts
                
              (let ((next-inst (car insts)))
                (cond ((symbol? next-inst)
                       (set! curr-label next-inst)
                       (set! label-inst-count 0)
                       (set! breakpoint-n (get-breakpoint-n 0))
                       (advance-pc)
                       (if (eq? trace 'on)
                           (for-each display (list "\n" next-inst "\n")))
                       (execute-1-inst))

                      ((and breakpoint-n
                            (= (+ label-inst-count 1) breakpoint-n))
                        (for-each display (list "\nBreakpoint..."
                                                curr-label " "
                                                breakpoint-n "\n"))
                        (set! breakpoint-n (get-breakpoint-n breakpoint-n))
                        'breakpoint)

                      (else (let ((inst-text (next-inst 'text)))
                              (if (eq? trace 'on)
                                  (for-each display (list "\n" inst-text "\n")))
                              ((next-inst 'exec-proc))
                              (set! inst-count (+ 1 inst-count))
                              (set! label-inst-count (+ 1 label-inst-count))
                              'done)))))))

      (define (execute)
        (let ((e1 (execute-1-inst)))
          (if (eq? e1 'done)
              (execute)
              e1)))
      
      (define (step-loop . args)
        (if (and (pair? args)
                 (eq? (car args) 'no-more-insts))
            'no-more-insts
            
            (let ((cmd (read)))
              (cond ((eq? cmd 'x) args)

                    ((eq? cmd 's) (step-loop (execute-1-inst)))
                
                    ((eq? cmd 'p) (step-loop (execute)))
                
                    (else (for-each display (list "Unknown input to step-loop: " cmd "\n"))
                          (step-loop))))))

      (define (dispatch message)
        (cond ((eq? message 'start)
               ((pc 'set) the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))

              ((eq? message 'reset) (reset-machine))

              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)

              ((eq? message 'install-operations) install-operations)
              ((eq? message 'operations) the-ops)

              ((eq? message 'stack) stack)
              
              ((eq? message 'advance-pc) (advance-pc))

              ((eq? message 'add-labels) add-labels)

              ((eq? message 'get-all-labels) (map car labels-alist))
              ((eq? message 'lookup-label) 
               (lambda (label) ((get-label-data label) 'inst-seq)))
              ((eq? message 'lookup-label-regs)
               (lambda (label) ((get-label-data label) 'label-regs)))

              ((eq? message 'install-specs)
               (lambda (mach-specs) (set! specs mach-specs)))
              ((eq? message 'specs) specs)

              ((eq? message 'reset-instruction-count) (set! inst-count 0))
              ((eq? message 'get-instruction-count) inst-count)

              ((eq? message 'trace-on) (set! trace 'on)
                                       (for-each display (list "\nTrace:" trace "\n")))
              ((eq? message 'trace-off) (set! trace 'on)
                                        (for-each display (list "\nTrace:" trace "\n")))
              
              ((eq? message 'set-breakpoint) set-breakpoint)
                                             
              ((eq? message 'cancel-breakpoint) cancel-breakpoint)
              
              ((eq? message 'cancel-all-breakpoints)
               (for-each (lambda (bp) (apply cancel-breakpoint bp))
                         breakpoints))
              
              ((eq? message 'proceed) (display "\n<Continuing execution till next breakpoint>\n")
                                      (execute))
              
              ((eq? message 'step) (display "\n<Stepping through instructions>\n")
                                   (step-loop))

              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(#%provide make-new-machine)
