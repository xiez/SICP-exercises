(setq lexical-binding t)
(setq tolerance 0.00001)

(defun iterative-improve (good-enough? improve)
  (lambda (first-guess)
    (defun try (guess)
      (let ((next (funcall improve first-guess)))
        (if (good-enough? guess next)
            next
          (funcall 'try next))))
    (try first-guess)))

;; ****************************************

(defun my-sqrt (x)
  (defun average-damp (f)
    (defun average (a b)
      (/ (+ (float a) b) 2))
    (lambda (x) (average x (funcall f x))))
  
  (funcall (iterative-improve
            (lambda (v1 v2) (< (abs (- v1 v2)) tolerance))
            ;; (average-damp (lambda (y) (/ x y)))
            (lambda (y) (/ x y)))
           1.0))

(my-sqrt 4)
