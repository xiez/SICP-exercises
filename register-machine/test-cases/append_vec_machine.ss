;; (define (append x y)
;;   (if (null? x)
;;       y
;;       (cons (car x) (append (cdr x) y))))
(controller
 ;; init the-cars, the-cdrs, free
 (assign the-cars (op make-vector))
 (assign the-cdrs (op make-vector))
 (assign free (const 1))                ;free starts from address 1
 (assign continue (label append-done))

 ;; x = (list 4 3)
 (assign tmp1 (const 3))
 (assign tmp2 (const 0))                ;NULL ptr
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
 (assign tmp2 (const 0))                ;NULL ptr
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

 ;; append
 append
 (test (op null?) (reg x))              ;null? x
 (branch (label base-case))

 (save x)                               ;set up for the (append(cdr x) y)
 (save continue)
 (assign continue (label after-append-cdr))
 ;; (assign x (op cdr) (reg x))
 (assign x (op vector-ref) (reg the-cdrs) (reg x))
 (goto (label append))

 after-append-cdr                       ;retval stores the result of (append (cdr x) y)
 (restore continue)
 (restore x)
 ;; (assign tmp (op car) (reg x))          ;tmp <- (car x)
 (assign tmp (op vector-ref) (reg the-cars) (reg x))
 ;; (assign retval                            ; retval = cons(tmp retval)
 ;;         (op cons) (reg tmp) (reg retval)) ;
 (perform (op vector-set!)
          (reg the-cars) (reg free) (reg tmp))
 (perform (op vector-set!)
          (reg the-cdrs) (reg free) (reg retval))
 (assign retval (reg free))
 (assign free (op +) (reg free) (const 1))

 (goto (reg continue))                  ;return to caller

 base-case
 (assign retval (reg y))
 (goto (reg continue))                  ;return to caller
 ;; end append

 append-done
 ;; (perform (op print) (reg retval))
 )
