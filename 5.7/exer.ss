(define expt-machine-rec
  (make-machine
   '(val b n continue)
   (list (list '= =) (list '- -) (list '* *))
   '(expt-loop
       (test (op =) (reg n) (const 0))
       (branch (label expt-base))
       (save n)
       (save continue)
       (assign n (op -) (reg n) (cons 1))     ;n = n -1
       (assign continue (label after-expt))
       (goto (label expt-loop))
       ;; expt-base
       expt-base
       (assign val (const 1))                 ;val = 1
       (goto (reg continue))
       ;; after-expt
       after-expt
       (restore continue)
       (restore n)
       (assign val (op *) (reg b) (reg val))   ;val = b * (expt b (- n 1))
       (goto (reg continue))
     expt-done)))

;; (expt 2 10)
(set-register-contents! expt-machine-rec 'b 2)
(set-register-contents! expt-machine-rec 'n 10)
(set-register-contents! expt-machine-rec 'continue 'expt-done)
(start expt-machine-rec)
(get-register-contents expt-machine-rec 'val)

;; ----------------------------------------

(define expt-machine-iter
  (make-machine
   '(b counter product)
   (list (list '= =) (list '- -) (list '* *))
   '(expt-loop
       (test (op =) (reg counter) (const 0))
       (branch (label expt-done))
       (assign counter (op -) (reg counter) (const 1)) ;counter = counter - 1
       (assign product (op *) (reg b) (reg product)) ;product = product * b
       (goto (label expt-loop))
     expt-done)))

;; (expt 2 10)
(set-register-contents! expt-machine-rec 'counter 2)
(set-register-contents! expt-machine-rec 'counter 10)
(set-register-contents! expt-machine-rec 'product 1)
(start expt-machine-iter)
(get-register-contents expt-machine-iter 'product)
