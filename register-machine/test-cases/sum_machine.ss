;; (define (sum n)
;;   (if (= n 0)
;;       0
;;       (+ n (sum (- n 1)))))
;; (sum 10)
(controller
 (assign n (const 10))
 (assign continue (label done))
 loop
 (test (op =) (reg n) (const 0))
 (branch (label base-case))
 (save continue)                        ;set up to compute
 (assign continue (label after-sum))  ;sum(n-1)
 (save n)                               ;save old n
 (assign n (op -) (reg n) (const 1))    ;n = n - 1
 (goto (label loop))                    ;perform recursive call
 after-sum                            ;upon return, val contains sum(n-1)
 (restore n)
 (restore continue)
 (assign val (op +) (reg val) (reg n))
 (goto (reg continue))
 base-case
 (assign val (reg n))
 (goto (reg continue))
 done
 (perform (op print) (reg val))
 )
