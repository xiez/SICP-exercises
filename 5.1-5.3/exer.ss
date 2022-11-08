;;; 5.2
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


;;; 5.3
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;;; with good-enough? and improve as primitive operations
(controller

 sqrt-loop
 (assign x (op read))
 (assign guess (const 1.0))

 test-guess
 (test (op good-enough?) (reg guess) (reg x))
 (branch (label done))
 (assign guess (op improve) (reg guess) (reg x))
 (goto (label test-guess))

 done
 (perform (op print) (reg guess)))

;;; expand good-enough? and improve in terms of arithmetic operations
(controller

 sqrt-loop
 (assign x (op read))
 (assign guess (const 1.0))

 good-enough?
 (assign a (op square) (reg guess))     ;square is considered as a primitive op
 (assign a (op -) (reg a) (reg x))
 (assign a (op abs) (reg a))            ;same as abs
 (test (op <) (reg a) (const 0.001))
 (branch (label done))

 ;; improve guess
 (assign a (op /) (reg x) (reg guess))
 (assign guess (op average) (reg guess) (reg a)) ;same as average
 (goto (label good-enough?))

 done
 (perform (op print) (reg guess)))
