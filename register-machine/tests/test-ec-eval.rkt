#lang sicp

(#%require "make-test-machine.rkt"
           "../controllers/ec-eval.rkt"
           "../controllers/ec-eval-operations.rkt"
            (rename racket/base
                    rkvoid? void?)
 )

(define init-cmds (append (list
                           ;'trace-on
                           ;'(stack (trace-on))
                           )
                          (map (lambda (reg)
                                 `(get-register (,reg) (trace-on)))
                               '(
                                 ;argl exp val proc unev env
                                 ))
                          ))

(define ec-eval-tester
  (apply make-test-machine
         (cons ec-eval
               (cons ec-eval-ops-assoc
                     init-cmds))))

(define (is-proc? x)
  (or (procedure? x)
      (primitive-procedure? x)
      (compound-procedure? x)
      (scheme-procedure? x)))

(define (exp-without-compiles exp)
  (if (compile? exp)
      (exp-to-compile exp)
      (if (pair? exp)
          (map exp-without-compiles exp)
          exp)))

(define (test-ec-eval exp)
  (let* ((scheme-output (eval (exp-without-compiles exp) (scheme-report-environment 5)))

         (test-args (cond ((rkvoid? scheme-output)
                           `((outputs (val ok))))

                          ((procedure? scheme-output)
                           `((outputs (val ,scheme-output))
                             (output-check-map ,is-proc?)))

                          (else `((outputs (val ,scheme-output)))))))

    (apply (ec-eval-tester 'test)
           (cons `(inputs (exp ,exp))
                 test-args))))
  

(for-each test-ec-eval

          '(
            765
            "clockwork"
            (quote a)
             -
            (= 2 5)
            (* 31 41)
            (if 2 4 5)
            (if (= 2 0) 12 (if (= 4 0) 4 7))
            (lambda (x) 1)
            
            (begin (define x 3)
                   (set! x 2)
                   x)

            (begin
              (define x 7)
              (if (= x 0)
                  (set! x 2)
                  (set! x 9)))

            (begin
              (define (square x)
                (* x x))
              (square 7))

            (begin
              (define (square x) (* x x))
              (define (cube x) (* x (square x)))
              (cube 14))

            (begin
              (define (factorial x)
                (if (= x 0)
                    1
                    (* x (factorial (- x 1)))))
              
              (factorial 3))

            (compile 3) 

            (compile (= 5 6))
            
            (compile (begin
                       (define (square x)
                         (* x x))
                       (square 7)))

            (compile (begin
                       (define (factorial x)
                         (if (= x 0)
                             1
                             (* x (factorial (- x 1)))))
              
                       (factorial 15)))

            (begin
              (compile (define (factorial x)
                         (if (= x 0)
                             1
                             (* x (factorial (- x 1))))))
              
              (factorial 8))

            (begin
              (define (factorial x)
                (if (= x 0)
                    1
                    (* x (factorial (- x 1)))))
              
              (compile (factorial 10))) 
           ))
