(setq lexical-binding t)

(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

(funcall (compose 'square '1+) 6)
