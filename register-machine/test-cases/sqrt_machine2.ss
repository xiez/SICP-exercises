(controller
 (assign x (op read))
 (assign guess (const 1.0))

 sqrt-loop
 (assign a (op square) (reg guess))     ;square is considered as a primitive op
 (assign a (op -) (reg a) (reg x))
 (assign a (op abs) (reg a))
 (test (op <) (reg a) (const 0.001))    ;if good-enough?, goto done with result in reg guess
 (branch (label done))
 (assign a (op /) (reg x) (reg guess))  ;otherwise, improve guess
 (assign guess (op average) (reg guess) (reg a))
 (goto (label sqrt-loop))

 done
 (perform (op print) (reg guess)))
