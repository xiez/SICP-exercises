;; (define (fact n)
;;   (if (< n 2)
;;       n
;;       (* n
;;          (fact (- n 1)))))
(controller
 (assign n (const 10))
 (assign retval (const 1))
 (assign continue (label fact-done))

 fact-loop
 (test (op <) (reg n) (const 2))
 (branch (label base-case))
 (save n)
 (save continue)
 (assign n (op -) (reg n) (const 1))
 (assign continue (label after-fact))
 (goto (label fact-loop) )

 after-fact
 (restore continue)
 (restore n)
 (assign retval (op *) (reg n) (reg retval)) ;retval = n * fact(n - 1)
 (goto (reg continue))

 base-case
 (assign retval (reg n))
 (goto (reg continue) )

 fact-done
 (perform (op print-stack-statistics))
)
