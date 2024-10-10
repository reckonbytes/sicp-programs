#lang sicp

(#%require "stack.rkt"
           "label-stack-entry.rkt")

(define end-frame-tag '*end*)

(define (make-label-stack)
  (let ((s (make-stack))
        (trace #f)
        (trace-entry? #f) ;default trace shows only entry-names
        )

    (define (regs-pop pops)
      (let ((p (s 'pop)))
        (if (eq? p end-frame-tag)
            pops
            (regs-pop (cons p pops)))))

    (define (split-out-regs regs stack-list)
      (let ((r (car stack-list)))
        (if (eq? r end-frame-tag)
            (cons regs (cdr stack-list))
            (split-out-regs (cons r regs)
                            (cdr stack-list)))))

    (define (split-out-top-frame stack-list)
      (let ((e (car stack-list)))
        (if (eq? e end-frame-tag)
            (cons '() (cdr stack-list))

            (let* ((split (split-out-regs '() (cdr stack-list)))
                   (regs (car split))
                   (rest-list (cdr split)))
              (cons (cons e regs) ;e is the label-entry
                    rest-list)))))

    (define (list-frames stack-list)
      (if (null? stack-list)
          '()
          (let* ((split (split-out-top-frame stack-list))
                 (frame (car split))
                 (trace-frame (if trace-entry?
                                  frame
                                  (map get-entry-name frame)))
                 (rest-list (cdr split)))
            (cons trace-frame (list-frames rest-list)))))

    (define (do-trace)
      (display "\nLabel Stack:\n")
      (for-each (lambda (x)
                  (display x)
                  (newline))
                (list-frames (s 'list)))
      (display "<label-stack-end>\n"))
             
    (lambda (msg)
      (cond ((eq? msg 'label-push)
             (lambda (label-entry . reg-entries)
               ((s 'push) end-frame-tag)
               (for-each (s 'push) reg-entries)
               ((s 'push) label-entry)
               (if trace (do-trace))
               'ok))

            ((eq? msg 'label-pop)
             (let ((label-entry (s 'pop))
                   (reg-entries (regs-pop '())))
               (if trace (do-trace))
               (cons label-entry reg-entries)))

            ((memq msg '(pop push))
             (error "Use 'label-push' and 'label-pop', not 'push' or 'pop' -- LABEL STACK"))

            ;((eq? msg 'list) (list-frames (s 'list)))
            
            ((eq? msg 'trace-on) (set! trace #t))
            ((eq? msg 'trace-off) (set! trace #f))
            ((eq? msg 'trace-entry-on) (set! trace-entry? #t))
            ((eq? msg 'trace-entry-off) (set! trace-entry? #f))
            
            (else (s msg))))))

(#%provide make-label-stack)
