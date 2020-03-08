(setq lexical-binding t)
(setq tolerance 0.00001)

(defun repeated (f n)
  (defun compose (f g)
    (lambda (x)
      (funcall f (funcall g x))))
  
  (if (= n 1)
      f
    (compose f (repeated f (- n 1)))
    ))

(defun fixed-point-of-transform (g transform guess)
  (defun fixed-point (f first-guess)
    (defun close-enough? (v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (defun try (guess)
      (let ((next (funcall f guess)))
        (if (close-enough? guess next)
            next
          (funcall 'try next))
        ))
    (try first-guess))
  (fixed-point (funcall transform g) guess))

(defun root (n x)
  (defun average-damp (f)
    (defun average (a b)
      (/ (+ (float a) b) 2))
    (lambda (x) (average x (funcall f x))))
  
  (if (= n 1)
      x
    (fixed-point-of-transform
     (lambda (y) (/ x (expt y (- n 1))))
     (repeated 'average-damp (floor (log n 2)))
     1.0)))

;; repeated 1 time
(root 2 9)
(root 3 27)

;; repeated 2 times
(root 4 16)
(root 5 16)
(root 6 16)
(root 7 16)

;; repeated 3 times
(root 8 16)
(root 15 16)

;; repeated 4 times
(root 16 16)
(root 31 16)

;; repeated 5 times
(root 32 16)

(root 100 16)
(expt 1.028110582901565 100)

