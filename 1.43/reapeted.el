(defun repeated (f n)
  (defun compose (f g)
    (lambda (x)
      (funcall f (funcall g x))))
  
  (if (= n 1)
      f
    (compose f (repeated f (- n 1)))
    ))
