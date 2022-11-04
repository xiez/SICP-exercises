(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))


;; register-machine language
(controller
 test-b
 (test (op >) (reg b) (reg c))
 ;if previous test result is true, goto label fact-done, otherwise,  continue with the next instruction
 (branch (label fact-done))
 (assign a (op *) (reg a) (reg b))
 (assign b (op +) (reg b) (const 1))
 (goto (label test-b))
 fact-done)


;; with read and display
(controller

 fact-loop
 (assign c (op read))
 (assign a 1)
 (assign b 1)

 test-b
 (test (op >) (reg b) (reg c))
 (branch (label fact-done))
 (assign a (op *) (reg a) (reg b))
 (assign b (op +) (reg b) (const 1))
 (goto (label test-b))

 fact-done
 (perform (op print) (reg a))
 (goto (label fact-loop)))
