(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

;;; append
;;; register-machine/test-cases/append_machine.ss
(controller
 (assign continue (label append-done))

 ;; append
 append
 (test (op null?) (reg x))              ;null? x
 (branch (label base-case))

 (save x)                               ;set up for the (append(cdr x) y)
 (save continue)
 (assign continue (label after-append-cdr))
 (assign x (op cdr) (reg x))
 (goto (label append))

 after-append-cdr                       ;retval stores the result of (append (cdr x) y)
 (restore continue)
 (restore x)
 (assign tmp (op car) (reg x))          ;tmp <- (car x)
 (assign retval                         ; retval = cons(tmp retval)
         (op cons) (reg tmp) (reg retval)) ;
 (goto (reg continue))                  ;return to caller

 base-case
 (assign retval (reg y))
 (goto (reg continue))                  ;return to caller
 ;; end append

 append-done
 ;; (perform (op print) (reg retval))
 )

;;; append!
;;; register-machine/test-cases/append_machine2.ss
(controller
 (assign continue (label append-done))
 (goto (label append!))

 ;; last-pair
 last-pair
 (assign tmp (op cdr) (reg x))
 (test (op null?) (reg tmp))            ;null? (cdr x)
 (branch (label last-pair-base))

 (save x)                               ;set up for last-pair(cdr x)
 (save continue)
 (assign continue (label after-last-pair))
 (assign x (op cdr) (reg x))
 (goto (label last-pair))

 after-last-pair             ;retval stores the result of last-pair(x)
 (restore continue)
 (restore x)
 (goto (reg continue))                  ;return to caller

 last-pair-base
 (assign retval (reg x))
 (goto (reg continue))                  ;return to caller
 ;; end last-pair

 ;; append!
 append!
 (save x)                            ;set up for calling (last-pair x)
 (save continue)
 (assign continue (label last-pair-return))
 (goto (label last-pair))

 last-pair-return                       ;returns from last-pair
 (restore continue)
 (restore x)
 (perform (op set-cdr!) (reg retval) (reg y))
 (assign retval (reg x))
 (goto (reg continue))                  ;return to caller
 ;; end append!

 append-done
 ;; (perform (op print) (reg retval))
 )


;;; append and append! with vector-ref and vector-set!
;;; register-machine/test-cases/append_vec_machine.ss
;;; register-machine/test-cases/append_vec_machine2.ss
