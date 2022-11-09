;; ========== subroutine without stack ==========
;; gcd
(test ...)
(branch (label gcd-done))
(...)
(goto (label gcd))
;; gcd-done
(test (op = )(reg continue) (cons 0))
(branch (label after-gcd-1))
(goto (label after-gcd-2))
;; ========== end subroutine ==========
;; ========== call gcd1 ==========
(assign continue (const 0))
(goto (label gcd))
;; after-gcd-1
;; ...
;; ========== end call gcd1 ==========
;; ========== call gcd2 ==========
(assign continue (const 1))
(goto (label gcd))
;; after-gcd-2
;; ...
;; ========== end call gcd2 ==========
;; ========== end subroutine without stack ==========

;; ========== subroutine with stack ==========
;; gcd
(test ...)
(branch (label gcd-done))
(...)
(goto (label gcd))
;; gcd-done
(restore continue)                      ;restore continue from the stack
(goto (reg continue))
;; ========== end subroutine ==========
;; ========== call gcd1 ==========
(save (label after-gcd-1))              ;save continue instruction to the stack
(goto (label gcd))                      ;before call the gcd
;; after-gcd-1
;; ...
;; ========== end call gcd1 ==========
;; ========== call gcd2 ==========
(save (label after-gcd-2))
(goto (label gcd))
;; after-gcd-2
;; ...
;; ========== end call gcd2 ==========
;; ========== end subroutine without stack ==========


(define (factorial n)
  (if (= n 1) 
      1
      (* (factorial (- n 1)) n)))

(controller
 (assign continue (label fact-done))
 ;; fact-loop
 'fact-loop
 (test (op =) (reg n) (const 1))
 (branch (label fact-base))
 (save n)
 (save continue)
 (assign n (op -) (reg n) (cons 1))     ;n = n -1
 (assign continue (label after-fact))
 (goto (label fact-loop))
 ;; fact-base
 'fact-base
 (assign val (const 1))                 ;val = 1
 (goto (reg continue))
 ;; after-fact
 'after-fact
 (restore continue)
 (restore n)
 (assign val (op *) (reg val) (reg n))  ;val = (n-1)! * n
 (goto (reg continue))
 ;; fact-done
 'fact-done
 )

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(controller
 (assign continue (label fib-done))

 ;; fib-loop
 (test (op <) (reg n) (const 2))
 (branch (label fib-base))
 (save n)
 (save continue)
 (assign n (op -) (reg n) (const 1))    ;n = n - 1
 (assign continue (label after-fib-1))
 (goto (label fib-loop))
 ;; after-fib-n-1
 (restore continue)
 (restore n)

 ;; after-fib-n-2

 ;; fib-base
 (assign val (reg n))
 (goto (reg continue))
 ;; fib-done
 )


;;; 5.4
 (define (expt b n)
   (if (= n 0)
       1
       (* b (expt b (- n 1)))))

(controller
 (assign continue (label expt-done))
 ;; expt-loop
 (test (op =) (reg n) (const 0))
 (branch (label expt-base))
 (save n)
 (save continue)
 (assign n (op -) (reg n) (cons 1))     ;n = n -1
 (assign continue (label after-expt))
 (goto (label expt-loop))
 ;; expt-base
 (assign val (const 1))                 ;val = 1
 (goto (reg continue))
 ;; after-expt
 (restore continue)
 (restore n)
 (assign val (op *) (reg b) (reg val))   ;val = b * (expt b (- n 1))
 (goto (reg continue))
 ;; expt-done
 )


(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1)
                   (* b product))))
  (expt-iter n 1))

(controller
 (assign counter (op read))             ;counter = n
 (assign product (const 1))             ;product = 1
 ;; expt-loop
 (test (op =) (reg counter) (const 0))
 (branch (label expt-done))
 (assign counter (op -) (reg counter) (const 1)) ;counter = counter - 1
 (assign product (op *) (reg b) (reg product)) ;product = product * b
 (goto (label expt-loop))
 ;; expt-done
 )

;;; 5.5

(factorial 3)
;;; init stack:
()
;;; first fact-loop, stack:
(3 'fact-done)
;;; second fact-loop, stack:
(3 'fact-done 2 'after-fact)
;;; 3rd fact-loop, stack:
(3 'fact-done 2 'after-fact 1 'after-fact)
;;; first after-fact, stack:
(3 'fact-done 2 'after-fact)
;;; second after-fact, stack:
(3 'fact-done)
;;; 3rd after-fact, stack:
()

(fib 3)
;;; init stack:
()                                      ;n=3, cont='fib-done
;;; 1st fib-loop, fib(3), stack:
('fib-done 3)                           ;n=2, cont='fib-done
;;; 2nd fib-loop, fib(2), stack:
('fib-done 3 'afterfib-n-1 2)           ;n=1, cont='afterfib-n-1
;;; immediate-answer, fib(1), val=1, goto afterfib-n-1
('fib-done 3 'afterfib-n-1 2)           ;n=1, val=1
;;; afterfib-n-1, stack:
('fib-done 3 'afterfib-n-1 1)           ;n=0, val=1, cont='afterfib-n-2
;;; immediate-answer
('fib-done 3 'afterfib-n-1 1)           ;n=0, val=0, cont='afterfib-n-2
;;; afterfib-n-2, stack:



;;; n = 1 - 2
('fib-done 3 'afterfib-n-1)
('fib-done 3 'afterfib-n-1 1)           ;save Fib(n-1)


;;; 5.6

;; in afterfib-n-1, restore and save continue can be removed, since continue will be assigned in next instruction
;; (restore continue)
;; (...)
;; (save continue)
