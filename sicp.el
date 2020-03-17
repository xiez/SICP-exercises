(defun remainder (a b)
  (if (>= a 0)
      (mod a (abs b))
    (- (mod (abs a) (abs b)))))

(defun gcd (a b)
  (if (= b 0)
      a
    (gcd b (remainder a b))))

(defun average (a b)
  (/ (float (+ a b)) 2))

(defun even? (x)
  (if (= 0 (mod x 2))
      t
    nil))

(defun odd? (x)
  (if (= 1 (mod x 2))
      t
    nil))

(provide 'remainder)
(provide 'gcd)
(provide 'average)
(provide 'even?)
(provide 'odd?)
