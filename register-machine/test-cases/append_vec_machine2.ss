;; (define (last-pair x)
;;   (if (null? (cdr x))
;;       x
;;       (last-pair (cdr x))))
;; (define (append! x y)
;;   (set-cdr! (last-pair x) y)
;;   x)

(controller
 ;; init the-cars, the-cdrs, free
 (assign the-cars (op make-vector))
 (assign the-cdrs (op make-vector))
 (assign free (const 1))                ;free starts from address 1
 (assign continue (label append-done))

 ;; x = (list 4 3)
 (assign tmp1 (const 3))
 (assign tmp2 (const 0))                ;null pointer
 (perform (op vector-set!) (reg the-cars) (reg free) (reg tmp1))
 (perform (op vector-set!) (reg the-cdrs) (reg free) (reg tmp2))
 (assign x (reg free))
 (assign free (op +) (reg free) (const 1))

 (assign tmp1 (const 4))
 (assign tmp2 (reg x))
 (perform (op vector-set!) (reg the-cars) (reg free) (reg tmp1))
 (perform (op vector-set!) (reg the-cdrs) (reg free) (reg tmp2))
 (assign x (reg free))
 (assign free (op +) (reg free) (const 1))

 ;; y = (list 2 1)
 (assign tmp1 (const 1))
 (assign tmp2 (const 0))                ;null pointer
 (perform (op vector-set!) (reg the-cars) (reg free) (reg tmp1))
 (perform (op vector-set!) (reg the-cdrs) (reg free) (reg tmp2))
 (assign y (reg free))
 (assign free (op +) (reg free) (const 1))

 (assign tmp1 (const 2))
 (assign tmp2 (reg y))
 (perform (op vector-set!) (reg the-cars) (reg free) (reg tmp1))
 (perform (op vector-set!) (reg the-cdrs) (reg free) (reg tmp2))
 (assign y (reg free))
 (assign free (op +) (reg free) (const 1))
 ;; end init

 (assign continue (label append-done))
 (goto (label append!))

 ;; last-pair
 last-pair
 ;; (assign tmp (op cdr) (reg x))
 (assign tmp (op vector-ref) (reg the-cdrs) (reg x))
 (test (op null?) (reg tmp))            ;null? (cdr x)
 (branch (label last-pair-base))

 (save x)                               ;set up for last-pair(cdr x)
 (save continue)
 (assign continue (label after-last-pair))
 ;; (assign x (op cdr) (reg x))
 (assign x (op vector-ref) (reg the-cdrs) (reg x))
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
 ;; (perform (op set-cdr!) (reg retval) (reg y))
 (perform (op vector-set!) (reg the-cdrs) (reg retval) (reg y))
 (assign retval (reg x))
 (goto (reg continue))                  ;return to caller
 ;; end append!

 append-done
 ;; (perform (op print) (reg retval))
 )
