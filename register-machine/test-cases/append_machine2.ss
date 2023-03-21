;; (define (last-pair x)
;;   (if (null? (cdr x))
;;       x
;;       (last-pair (cdr x))))
;; (define (append! x y)
;;   (set-cdr! (last-pair x) y)
;;   x)

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

 after-last-pair                        ;retval stores the result of last-pair(x)
 (restore continue)
 (restore x)
 (goto (reg continue))                  ;return to caller

 last-pair-base
 (assign retval (reg x))
 (goto (reg continue))                  ;return to caller
 ;; end last-pair

 ;; append!
 append!
 (save x)                               ;set up for calling (last-pair x)
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
