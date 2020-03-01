(setq lexical-binding t)

(defun product (term a next b)
  (if (> a b)
      1
      (* (funcall term a)
         (product term (funcall next a) next b))))

(defun factorial (n)
  (product 'identity 1 '1+ n))

(factorial 6)

(defun wallis-pi (n)
  (defun nom-term (a)
    (cond ((even? a) (+ 2 a))
          (t (+ 1 a))))
  (defun denom-term (a)
    (cond ((even? a) (+ 1 a))
          (t (+ 2 a))))

  (* 4
     (/ (float (product 'nom-term 1 '1+ n))
        (product 'denom-term 1 '1+ n))))
