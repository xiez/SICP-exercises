(
 ;; ...

 ev-variable
 (assign val (op lookup-variable-value) (reg exp) (reg env))
 (test (op unbound-variable?) (reg val))
 (branch (label ev-variable-unbound))
 (assign val (op bound-variable-value) (reg val))
 (goto (reg continue))

 ;; ...
)
