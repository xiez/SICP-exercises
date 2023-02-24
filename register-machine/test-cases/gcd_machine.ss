;; (define (remainder a b)
;;   (if (< a b)
;;       a
;;       (remainder (- a b) b)))
;; (define (gcd a b)
;;   (if (= b 0)
;;       a
;;       (gcd b (remainder a b))))
;;; (gcd 42 6)
(controller
 (assign a (const 42))
 (assign b (const 6))
 (assign continue (label gcd-done))     ;set up to compute gcd(a b)
 (goto (label gcd-loop) )

 ;; remainder
 rem-loop
 (test (op <) (reg a) (reg b))
 (branch (label rem-base))
 (save a)                               ;set up for the recursive call
 (save b)                               ;by saving parameters and retaddr
 (save continue)                        ;to the stack
 (assign continue (label after-rem))    ;
 (assign a (op -) (reg a) (reg b))      ;a <- a - b
 (goto (label rem-loop))
 after-rem
 (restore continue)                     ;restore parameters and retaddr in
 (restore b)                            ;reversed order
 (restore a)
 (goto (reg continue))                  ;return to caller
 rem-base
 (assign retval (reg a))                ;result stored in retval
 (goto (reg continue))                  ;return to caller
 ;; end remainder

 ;; gcd
 gcd-loop
 (test (op =) (reg b) (const 0))
 (branch (label gcd-base) )
 (save a)                               ;save parameters and retaddr to the stack
 (save b)                               ;before calling remainder
 (save continue)
 (assign continue (label after-rem-ret))  ;set up to compute remainder(a b)
 (goto (label rem-loop))                  ;compute remainder
 after-rem-ret
 (restore continue)                     ;restore parameters and retaddr in
 (restore b)                            ;reversed order
 (restore a)
 (save a)                               ;set up to compute gcd(b remainder)
 (save b)                               ;
 (save continue)                        ;
 (assign continue (label after-gcd))
 (assign a (reg b))                     ;a <- b
 (assign b (reg retval))                ;b <- result of remainder
 (goto (label gcd-loop) )
 after-gcd
 (restore continue)
 (restore b)
 (restore a)
 (goto (reg continue))
 gcd-base
 (assign retval (reg a))
 (goto (reg continue))
 ;; end gcd

 gcd-done
 (perform (op print) (reg retval))
 )
