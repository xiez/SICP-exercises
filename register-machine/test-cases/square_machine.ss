;; (define (square n)
;;   (* n n))
;; (square 4)
(controller
 (assign n (const 4))
 (assign continue (label done))
 square
 (assign n (op *) (reg n) (reg n))
 (goto (reg continue) )
 done
 (perform (op print) (reg n))
 )
