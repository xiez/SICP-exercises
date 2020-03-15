(setq lexical-binding t)

(defun cons2 (x y)
  (lambda (m) (funcall m x y)))

(defun car2 (z)
  (funcall z (lambda (p q) p)))

(defun cdr2 (z)
  (funcall z (lambda (p q) q)))

;; ****************************************
(car2 (cons2 1 2))
(cdr2 (cons2 1 2))
