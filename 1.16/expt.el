(defun expt (b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(expt 2 4)

(defun expt-iter (b n)
  (defun expt-iter-aux (b counter product)
    (if (= counter 0) product
        (expt-iter-aux b (- counter 1) (* b product))))
  (expt-iter-aux b n 1))

(expt-iter 2 4)

(defun fast-expt (b n)
  (defun square (n)
    (* n n))
  (defun even? (n)
    (= (mod n 2) 0))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (t (* b (fast-expt b (- n 1))))))

(fast-expt 2 10)

(defun fast-expt-iter (b n)
  (defun even? (n)
    (= (mod n 2) 0))
  (defun fast-expt-iter-aux (b counter a)
    (cond ((= counter 1) (* b a))
          ((even? counter) (fast-expt-iter-aux (square b) (/ counter 2) a))
          (t (fast-expt-iter-aux b (- counter 1) (* b a)))))
  (fast-expt-iter-aux b n 1))

(fast-expt-iter 2 10)
