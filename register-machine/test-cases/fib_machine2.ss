;;; Standard version of fib that follow certain rules:
;;; 0, *retval* register is used to store the return value of a function
;;; 1. before calling a function, push all the parameters to the stack,
;;;    after a function returns, restore all paremeters.
;;; 2. before calling a function, push the return address which is stored
;;;    in *continue* to the stack,
;;;    before a function returns, restore the content of *continue* from the stack.
;;; 3. always push parameters first, then *continue*.
;;;
;; (define (fib n)
;;   (if (< n 2)
;;       n
;;       (+ (fib (- n 1))
;;          (fib (- n 2)))))
(controller
 (assign n (const 8))
 (assign continue (label fib-done))

 ;; fib
 fib
 (test (op <) (reg n) (const 2))
 (branch (label fib-base))
 (save n)                               ;set up for the fib(n-1)
 (save continue)                        ;by saving n and continue
 (assign continue (label after-fib-1))  ;
 (assign n (op -) (reg n) (const 1))    ;
 (goto (label fib))

 after-fib-1
 (restore continue)                     ;restore n and continue
 (restore n)                            ;after fib(n-1)
 (save n)                               ;set up for the fib(n-2)
 (save continue)                        ;by saving n and continue
 (assign continue (label after-fib-2))  ;
 (assign n (op -) (reg n) (const 2))    ;
 (save retval)                          ;save value of fib(n-1)
 (goto (label fib))

 after-fib-2
 (assign tmp (reg retval) )                    ;tmp <- fib(n-2)
 (restore retval)                          ;restore value of fib(n-1)
 (restore continue)                     ;restore n and continue
 (restore n)                            ;after fib(n-2)
 (assign retval (op +) (reg retval) (reg tmp)) ;retval <- fib(n-1) + fib(n-2)
 (goto (reg continue))                         ;return to caller

 fib-base
 (assign retval (reg n))
 (goto (reg continue))                  ;return to caller
 ;; end fib

 fib-done
 ;; (perform (op print) (reg retval))
 (perform (op print-stack-statistics))
 )
