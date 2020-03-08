(setq lexical-binding t)

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

(setq dx 0.00001)

(defun deriv (g)
  (lambda (x)
    (/ (- (funcall g (+ x dx)) (funcall g x))
       dx)))

(defun newton-transform (g)
  (lambda (x)
    (- x (/ (funcall g x) (funcall (deriv g) x)))))

(defun newtons-method (g guess)
  (fixed-point (newton-transform g) guess))

;; ****************************************
(defun cube (x)
  (* x x x))

(defun square (x)
  (* x x))

(defun cubic (a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(newtons-method (cubic 1 2 3) 1)
