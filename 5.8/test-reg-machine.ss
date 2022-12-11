;; ref exer 5.7
(define expt1
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
(expt1 'start)

(get-register-contents expt1 'pc)
(expt1 'operations)
((expt1 'stack) 'top)

(define controller-text
  '(expt-loop
    (test (op =) (reg counter) (const 0))
    (branch (label expt-done))
    (assign counter (op -) (reg counter) (const 1)) ;counter = counter - 1
    (assign product (op *) (reg b) (reg product)) ;product = product * b
    (goto (label expt-loop))
    expt-done))
(assemble controller-text expt1)

