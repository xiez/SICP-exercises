(defun my-reverse (l)
  (if (= 1 (length l))
      l
    (append (my-reverse (cdr l))
          (list (car l)))))

(setq x (list (list 1 2) (list 3 4)))

(my-reverse x)

;; ****************************************

(defun deep-reverse lst
  )

