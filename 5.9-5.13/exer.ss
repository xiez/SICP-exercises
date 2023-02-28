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
'(controller
         (assign n (const 8))
         (assign continue (label fib-done))
         fib-loop
         (test (op <) (reg n) (const 2))
         (branch (label immediate-answer))
         ;; set up to compute Fib(n − 1)
         (save continue)
         (assign continue (label afterfib-n-1))
         (save n)                               ; save old value of n
         (assign n 
                 (op -)
                 (reg n)
                 (const 1))                     ; clobber n to n-1
         (goto 
          (label fib-loop))             ; perform recursive call
         afterfib-n-1                   ; upon return, val contains Fib(n − 1)
         (restore n)
         (restore continue)
         ;; set up to compute Fib(n − 2)
         (assign n (op -) (reg n) (const 2))
         (save continue)
         (assign continue (label afterfib-n-2))
         (save val)                             ; save Fib(n − 1)
         (goto (label fib-loop))
         afterfib-n-2                   ; upon return, val contains Fib(n − 2)
         ;; (assign n 
         ;;         (reg val))                     ; n now contains Fib(n − 2)
         ;; (restore val)                          ; val now contains Fib(n − 1)
         (restore n)                            ; <<< REPLACE WITH THIS ;n <- Fib(n - 1)
         (restore continue)
         (assign val                            ; Fib(n − 1) + Fib(n − 2)
                 (op +) 
                 (reg val)
                 (reg n))
         (goto                                  ; return to caller,
          (reg continue))                       ; answer is in val
         immediate-answer
         (assign val 
                 (reg n))                       ; base case: Fib(n) = n
         (goto (reg continue))
         fib-done)

;; 2
;;; save register name along with register contents
(define (make-save inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (push stack
            (cons
             (stack-inst-reg-name inst)
             (get-contents reg)))
      (advance-pc pc))))

;; make procedure for restore instruction
(define (make-restore inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (let ((stack-cont (pop stack)))
        (if (not (eq? (car stack-cont)
                      (stack-inst-reg-name inst)))
            (error "Invalid value to restore." (car stack-cont) (stack-inst-reg-name inst))
            (begin
              (set-contents! reg (cdr stack-cont))
              (advance-pc pc)))))))

;;; 3
;;; TODO

;;; 5.12
;;; TODO: extend the *assemble* procedure, add *extract-insts*, *extra-registers*, ...

;;; 5.13
;;; when assembler encounters <assign-op>, <reg-op> and <save-op>, extract the register name and allocate it
;;; if it does not exist.

