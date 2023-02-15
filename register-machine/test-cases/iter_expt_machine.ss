(controller
 (assign b (const 2))             ;base = 2
 (assign counter (const 10))             ;counter = n
 (assign product (const 1))             ;product = 1
 expt-loop
 (test (op =) (reg counter) (const 0))
 (branch (label expt-done))
 (assign counter (op -) (reg counter) (const 1)) ;counter = counter - 1
 (assign product (op *) (reg b) (reg product)) ;product = product * b
 (goto (label expt-loop))
 expt-done
 )
