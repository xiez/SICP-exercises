(defun for-each (proc lst)
  (if (null lst)
      t
    (progn
      (funcall proc (car lst))
      (for-each proc (cdr lst))
      )))

(for-each (lambda (x) (message "%s" x))
          (list 1 2 3))


(map (lambda (x) (* x 2))
     (list 2 3))
