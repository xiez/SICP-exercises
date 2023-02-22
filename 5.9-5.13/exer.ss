;; 5.9
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map
          (lambda (e)
            (if (label-exp? e)
                (error "Operations can be used with "
                       "registers and constants: ASSEMBLE" exp)
                (make-primitive-exp e machine labels)))
          (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

;; 5.10
;; TODO

;;; 5.11

;; 1
(let ((controller-text
       '(controller
         (assign n (const 20))
         (assign continue (label fib-done))
         fib-loop
         (test (op <) (reg n) (const 2))
         (branch (label immediate-answer))
         ;; set up to compute Fib(n − 1)
         (save continue)
         (assign continue (label afterfib-n-1))
         (save n)           ; save old value of n
         (assign n 
                 (op -)
                 (reg n)
                 (const 1)) ; clobber n to n-1
         (goto 
          (label fib-loop)) ; perform recursive call
         afterfib-n-1 ; upon return, val contains Fib(n − 1)
         (restore n)
         (restore continue)
         ;; set up to compute Fib(n − 2)
         (assign n (op -) (reg n) (const 2))
         (save continue)
         (assign continue (label afterfib-n-2))
         (save val)         ; save Fib(n − 1)
         (goto (label fib-loop))
         afterfib-n-2 ; upon return, val contains Fib(n − 2)
         (assign n 
                 (reg val)) ; n now contains Fib(n − 2)
         (restore val)      ; val now contains Fib(n − 1)
         (restore continue)
         (assign val        ; Fib(n − 1) + Fib(n − 2)
                 (op +) 
                 (reg val)
                 (reg n))
         (goto              ; return to caller,
          (reg continue))   ; answer is in val
         immediate-answer
         (assign val 
                 (reg n))   ; base case: Fib(n) = n
         (goto (reg continue))
         fib-done))
      (registers '(n val continue))
      (ops (list
            (list '< <)
            (list '- -)
            (list '+ +))))
  (define machine
    (make-machine registers ops controller-text))
  (machine 'start)

  (let ((res (get-register-contents
              machine
              'val)))
    (if (= 6765 res)
        (display "OK: fib-machine result is 6765 \n")
        (error "Error: fib-machine result is " res)))
  )

;; 2

