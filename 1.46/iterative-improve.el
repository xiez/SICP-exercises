(setq lexical-binding t)

(defun iterative-improve (good-enough? improve)
  (lambda (first-guess)
    (defun try (guess)
      (let ((next (funcall improve guess)))
        (if (funcall good-enough? guess next)
            next
          (try next))))
    (try first-guess)))


(defun my-sqrt (x)
  (defun close-enough? (a b)
    (let ((radio (if (> a b) (/ a b) (/ b a))))
      (< radio 1.01)))
  
  (funcall (iterative-improve
            'close-enough?
            (lambda (y) (average y (/ x y))))
           1.0))

(my-sqrt 4)

;; ****************************************
(setq tolerance 0.00001)

(defun fixed-point (f first-guess)
  (defun close-enough? (v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (funcall (iterative-improve
            'close-enough?
            f)
           first-guess))

(defun my-sqrt2 (x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(my-sqrt2 4)
