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
