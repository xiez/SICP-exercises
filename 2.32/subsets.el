(setq set (list 1 2 3))

(defun subsets (s)
  (if (null s)
      (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest ())
      )
      )
  )


(car set)
(cdr set)
