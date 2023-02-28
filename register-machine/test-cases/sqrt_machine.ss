(sqrt-loop
 (assign x (op read))
 (assign guess (const 1.0))
 test-guess
 (test (op good-enough?) (reg guess) (reg x))
 (branch (label done))
 (assign guess (op improve) (reg guess) (reg x))
 (goto (label test-guess))
 done
 (perform (op print) (reg guess)))
