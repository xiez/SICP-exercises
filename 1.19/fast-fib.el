(defun fib (n)
  (defun double (n)
    (* 2 n))
  (defun even? (n)
    (= (mod n 2) 0))
  (defun square (n)
    (* n n))
  (defun double (n)
    (* 2 n))
  (defun fib-iter (a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))      ; compute p'
                     (+ (double (* p q)) (square q))      ; compute q'
                     (/ count 2)))
          (t (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))

(fib 80)
