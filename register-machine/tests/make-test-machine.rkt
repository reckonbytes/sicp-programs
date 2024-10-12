#lang sicp

(#%require rackunit
           "../simulator/make-machine.rkt")

(define (make-test-machine controller-text init-ops . init-cmds)

  (let ((mach (apply make-machine
                     (cons controller-text
                           (cons init-ops init-cmds))))
        (sim-fn #f)
        (sim-input-regs #f)
        (sim-out->regs (lambda (sim-out)
                         (list 'outputs (list 'val sim-out))))
        (test-args #f))

    (mach 'repl-off)

    (define (try-sim inputs-arg)
      (let ((sim-inputs (map (lambda (reg)
                               (cadr (assoc reg (cdr inputs-arg))))
                             sim-input-regs)))
        (sim-out->regs (apply sim-fn sim-inputs))))      

    
    (define (set-inputs inputs-arg)
      (if (not inputs-arg)
          (display "\nInputs not given -- TEST-MACHINE.\n")
          (begin (display "\nSetting input regs\n")
                 (for-each (lambda (input)
                             (let* ((reg-name (car input))
                                    (val (cadr input))
                                    (reg ((mach 'get-register) reg-name)))
                               ((reg 'set) val)
                               (for-each display
                                         (list reg-name ":" val "\n"))))
                           (cdr inputs-arg)))))

    (define (check-outputs outputs-arg map-arg)
      (if (not outputs-arg)
          (error "Test outputs / simulated function not given -- TEST-MACHINE.")
          (let ((out-map (or (and map-arg (cadr map-arg)) (lambda (x) x))))
            (display "\nChecking output regs\n")
            (for-each (lambda (output) 
                        (let* ((reg-name (car output))
                               (expected-output (out-map (cadr output)))
                               (reg ((mach 'get-register) reg-name))
                               (reg-get (reg 'get))
                               (reg-output (out-map reg-get)))

                          (check-equal?
                           reg-output expected-output
                           (string-append "Output register "
                                          (symbol->string reg-name)
                                          " does not have the expected "
                                          (if map-arg "mapped " "")
                                          "value."))
                          (for-each display (list reg-name ":" reg-get "\n"))))

                      (cdr outputs-arg)))))

    (define (check-run exec-output)
      (display "\nSimulator run\n")
      (check-outputs
       (or (assoc 'outputs test-args)
           (and sim-fn (try-sim (assoc 'inputs test-args))))
       (assoc 'output-check-map test-args))
      ((mach 'stack) 'print-statistics)
      (set! test-args #f))

    (lambda (msg)
      (cond ((eq? msg 'machine) mach)

            ((eq? msg 'simulated-fn)
             (lambda (fn input-regs . output->regs)
               (set! sim-fn fn)
               (set! sim-input-regs input-regs)
               (if (null? output->regs)
                   (display "\nDefault output register: val\n")
                   (set! sim-out->regs (car output->regs)))))

            ((eq? msg 'test)
             (lambda (arg1 . other-args)
               (set! test-args (cons arg1 other-args))

               (let ((reset-arg (assoc 'reset test-args)))
                 (if (if reset-arg (cadr reset-arg) #f)
                     (mach 'reset)))
  
               (set-inputs (assoc 'inputs test-args))

               (check-run (mach 'start))))
            
            (else (error "Unknown message to test-machine" msg))))
      ))

(#%provide make-test-machine)
