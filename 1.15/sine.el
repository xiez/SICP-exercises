(defun cube (x) (* x x x))

(defun p (x)
  (- (* 3 x) (* 4 (cube x))))

(defun my-sine (angle)
  (if (not (> (abs angle) 0.1))
       angle
       (p (my-sine (/ angle 3.0)))))

(my-sine 12.5)
