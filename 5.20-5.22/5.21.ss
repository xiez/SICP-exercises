(define tree (cons (cons 1 2) 3))

;;; 1. recursive
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else 
         (+ (count-leaves (car tree))
            (count-leaves (cdr tree))))))

;;; register-machine/test-cases/count_leaves_machine.ss
(controller
 (assign retval (const 0))
 (assign continue (label count-done))

 ;; count-leaves
 count-leaves
 (test (op null?) (reg tree))           ;null? tree
 (branch (label base-case-0))
 (assign tmp (op pair?) (reg tree))     ;(not (pair? tree))
 (test (op not) (reg tmp))              ;
 (branch (label base-case-1))

 (save tree)                    ;set up for the count-leaves(car tree)
 (save continue)                ;by saving tree and continue
 (assign continue (label after-count-car))
 (assign tree (op car) (reg tree))
 (goto (label count-leaves))

 after-count-car
 (restore continue)                     ;restore tree and continue
 (restore tree)                         ;after count-leaves(car tree)
 (save tree)
 (save continue)
 (assign continue (label after-count-cdr))
 (assign tree (op cdr) (reg tree))
 (save retval)
 (goto (label count-leaves))     ;save value of count-leaves(car tree)

 after-count-cdr
 (assign tmp (reg retval))        ;tmp <- count-leaves(cdr tree)
 (restore retval)                 ;retval <- count-leaves(car tree)
 (restore continue)               ;restore tree and continue
 (restore tree)                   ;after count-leaves(cdr tree)
 (assign retval (op +)            ;retval <- count-(car) + count-(cdr)
         (reg retval) (reg tmp))  ;
 (goto (reg continue))            ;return to caller

 base-case-0
 (assign retval (const 0))
 (goto (reg continue))

 base-case-1
 (assign retval (const 1))
 (goto (reg continue))
 ;; end count-leaves

 count-done
 ;; (perform (op print) (reg retval))
 )

;;; 2. iterative
(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else 
           (count-iter 
            (cdr tree)
            (count-iter (car tree) n)))))
  (count-iter tree 0))

;;; register-machine/test-cases/count_leaves_machine2.ss
(controller
 (assign n (const 0))
 (assign continue (label count-done))

 ;; count-iter
 count-iter
 (test (op null?) (reg tree))           ;null? tree
 (branch (label base-case-0))
 (assign tmp (op pair?) (reg tree))     ;(not (pair? tree))
 (test (op not) (reg tmp))              ;
 (branch (label base-case-1))

 (save tree)                            ;set up for the inner count(car tree)
 (save continue)
 (assign continue (label after-count-car))
 (assign tree (op car) (reg tree))
 (goto (label count-iter))

 after-count-car                        ;n stores the result of count(car tree)
 (restore continue)                     ;restore tree and continue
 (restore tree)                         ;after inner count(car tree)
 (save tree)                            ;set up for the outer count(cdr tree)
 (save continue)
 (assign continue (label after-count-cdr))
 (assign tree (op cdr) (reg tree))
 (goto (label count-iter))

 after-count-cdr
 (restore continue)
 (restore tree)
 (goto (reg continue))                  ;return to caller

 base-case-0
 (goto (reg continue))                  ;return to caller, with the result in n

 base-case-1
 (assign n (op +) (reg n) (const 1))    ;n = n + 1
 (goto (reg continue))
 ;; end count-iter

 count-done
 ;; (perform (op print) (reg n))
 )
