(defun smallest-divisor (n)
  (defun find-divisor (n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (t (find-divisor n (+ test-divisor 1)))))
  (defun divides? (a b)
    (= (mod b a) 0))
  (defun square (n)
    (* n n))
  (find-divisor n 2))

(dolist (num '(199 1999 19999))
  (message
          "Smallest divisor of %d is %d." num (smallest-divisor num)))
