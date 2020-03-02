(defun accumulate (combiner null-value term a next b)
  (if (> a b)
      null-value
      (funcall combiner (funcall term a)
               (accumulate combiner null-value term (funcall next a) next b))))

(defun sum (term a next b)
  (accumulate '+ 0 term a next b))

(defun product (term a next b)
  (accumulate '* 1 term a next b))

;; testing
(defun sum-integers (a b)
  (sum 'identity a '1+ b))

(sum-integers 1 10)

(defun product-integers (a b)
  (product 'identity a '1+ b))

(product-integers 1 4)
