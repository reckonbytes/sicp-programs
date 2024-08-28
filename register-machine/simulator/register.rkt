#lang sicp

(define (make-register name)
  (let* ((contents '*unassigned*)
         (trace 'off)
         (trace-off-set (lambda (value) (set! contents value)))
         (trace-on-set (lambda (value)
                         (for-each display (list "\nRegister " name " : " contents " -> " value "\n"))
                         (trace-off-set value)))
         (set-reg trace-off-set))
    
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set) set-reg )
            ((eq? message 'reset) (set-reg '*unassigned*))
            ((eq? message 'trace-on)
             (set! set-reg trace-on-set)
             (set! trace 'on)
             (for-each display (list "\nRegister " name " trace " trace)))
            ((eq? message 'trace-off)
             (set! set-reg trace-off-set)
             (set! trace 'off)
             (for-each display (list "\nRegister " name " trace " trace)))
            (else
             (error "Unknown request -- REGISTER" message))))    
      
    dispatch))

(#%provide make-register)
