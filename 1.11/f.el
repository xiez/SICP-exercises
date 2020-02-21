;; recursive
(defun f (n)
  (cond ((< n 3) n)
        (t (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

(f 3)
(f 4)
(f 10)

;; iterative
(defun f-iter (n)
  (defun f-iter-aux (a b c n)
    (cond ((= n 0) c)
          (t (f-iter-aux (+ a (* 2 b) (* 3 c))
                         a
                         b
                         (- n 1)))))
  (f-iter-aux 2 1 0 n))

(f-iter 10)
