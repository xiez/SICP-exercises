(setq lexical-binding t)

(defun double (f)
  (lambda (x)
    (funcall f (funcall f x))))

(funcall (double '1+) 1)
