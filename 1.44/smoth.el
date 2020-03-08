(setq dx 0.1)

(defun repeated (f n)
  (defun compose (f g)
    (lambda (x)
      (funcall f (funcall g x))))
  
  (if (= n 1)
      f
    (compose f (repeated f (- n 1)))
    ))

(defun average (a b c)
  (/ (+ a b c) 3))

(defun smooth (f)
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(defun n-fold-smooth (f n)
  (funcall (repeated smooth) n))
