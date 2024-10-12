#lang sicp

(#%require "../simulator/controller-syntax.rkt"
           "../simulator/remove-duplicates.rkt"
           "../simulator/filter.rkt"
            "make-label.rkt")

(define (make-inst-seq needs modifies statements)
  (list 'analyzed needs modifies statements))

(define (analyzed? seq)
  (and (not (null? seq))
       (eq? (car seq) 'analyzed)))

(define (get-needs inst-seq) (cadr inst-seq))
(define (get-mods inst-seq) (caddr inst-seq))
(define (get-stmts inst-seq) (cadddr inst-seq))

(define (get-seq-stmts seq)
  (if (analyzed? seq)
      (get-stmts seq)
      seq))

(define (stmts->inst-seq statements)

  (define (get-regs exp)
    (map register-exp-reg
         (filter register-exp? exp)))
  
  (define (recur-parse stmts needs mods)
    (if (null? stmts)
        (make-inst-seq
         (remove-duplicates needs)
         (remove-duplicates mods)
         statements)

        (let* ((inst (car stmts))
               (cmd (if (pair? inst) (car inst) #f)))

          (cond ((eq? cmd 'assign)
                 (recur-parse (cdr stmts)
                              (append (get-regs (assign-value-exp inst)) needs)
                              (cons (assign-reg-name inst) mods)))

                ((memq cmd '(test perform))

                 (let* ((op-exp (if (eq? cmd 'test)
                                   (test-condition inst)
                                   (perform-action inst)))
                       
                       (env-mod? (and (eq? cmd 'perform)
                                      (memq (operation-exp-op op-exp)
                                            '(set-variable-value! define-variable!)))))
                                   
                   (recur-parse (cdr stmts)
                                (append (get-regs (operation-exp-operands op-exp))
                                        needs)
                                (if env-mod?
                                    (cons 'env mods)
                                    mods))))

                (else (recur-parse (cdr stmts) needs mods))))))

  (recur-parse statements '() '()))

(define (analyze seq)
  (if (analyzed? seq)
      seq
      (stmts->inst-seq seq)))

(define (get-preserved-stmts label regs stmts)
  (if (null? regs)
      stmts
      (append (list (append `(label-save ,label) regs))
              stmts
              `((label-restore)
                ,label)
              (list (cons 'restored-regs regs)))))

(define (preserve-append-seqs label-entry regs seq1 next-seq)
  (let* ((iseq1 (analyze seq1))
         (inext-seq (analyze next-seq))
         (mods1 (get-mods iseq1))
         (next-needs (get-needs inext-seq))
         (p-regs (filter (lambda (r) (and (memq r mods1)
                                          (memq r next-needs)))
                          regs)))
    (append-2-seqs
     (make-inst-seq
      (get-needs iseq1)
      (filter (lambda (r) (not (memq r p-regs))) mods1)
      (get-preserved-stmts
       (make-label label-entry)
       p-regs (get-stmts iseq1)))
     inext-seq)))

(define (union . lists)
  (remove-duplicates (apply append lists)))

(define (append-2-seqs seq1 seq2)
  (let ((iseq1 (analyze seq1))
        (iseq2 (analyze seq2)))
    (let ((needs1 (get-needs iseq1))
          (needs2 (get-needs iseq2))
          (mods1 (get-mods iseq1))
          (mods2 (get-mods iseq2)))
      (make-inst-seq
       (union needs1 (filter (lambda (r) (not (memq r mods1)))
                             needs2))
       (union mods1 mods2)
       (append (get-stmts iseq1)
               (get-stmts iseq2))))))

(define (append-seqs . seqs)
  (cond ((null? seqs) '())

        ((null? (cdr seqs)) (car seqs))
        
        (else
         (apply append-seqs
                (cons (append-2-seqs (car seqs) (cadr seqs))
                      (cddr seqs))))))

(define (append-seq-alternatives . seq-alts)
  (let ((iseqs (map analyze seq-alts)))
    (make-inst-seq
     (apply union (map get-needs iseqs))
     (apply union (map get-mods iseqs))
     (apply append (map get-stmts iseqs)))))

(#%provide make-inst-seq
           get-seq-stmts
           preserve-append-seqs
           append-seqs
           append-seq-alternatives
           )
