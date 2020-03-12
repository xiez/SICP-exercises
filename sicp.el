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

(provide 'remainder)
(provide 'gcd)
(provide 'average)
