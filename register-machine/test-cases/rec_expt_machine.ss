(controller
 (assign continue (label expt-done))
 (assign n (const 10))
 (assign b (const 2))
 expt-loop
 (test (op =) (reg n) (const 0))
 (branch (label expt-base))
 (save n)
 (save continue)
 (assign n (op -) (reg n) (const 1))     ;n = n -1
 (assign continue (label after-expt))
 (goto (label expt-loop))
 expt-base
 (assign val (const 1))                 ;val = 1
 (goto (reg continue))
 after-expt
 (restore continue)
 (restore n)
 (assign val (op *) (reg b) (reg val))   ;val = b * (expt b (- n 1))
 (goto (reg continue))
 expt-done
 )
