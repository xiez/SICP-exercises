;; (define (append x y)
;;   (if (null? x)
;;       y
;;       (cons (car x) (append (cdr x) y))))
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
