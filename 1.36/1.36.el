(setq tolerance 0.00001)

(defun fixed-point (f first-guess)
  (defun close-enough? (v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (defun try (guess)
    (prin1 guess)
    (message "")
    (let ((next (funcall f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(defun xx (x)
  (/ (log 1000) (log x)))

(fixed-point 'xx 2.0)

;; average damping
(defun xx-avg-damping (x)
  (average (float x) (/ (log 1000) (log x))))

(fixed-point 'xx-avg-damping 2.0)
