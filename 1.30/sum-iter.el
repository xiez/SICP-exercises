(defun sum (term a next b)
  (defun iter (a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a ) result))))
  (iter a 0))

(defun sum-integers (a b)
  (defun identity (a) a)
  (sum 'identity a '1+ b))

(sum-integers 1 10)
