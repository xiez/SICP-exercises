(defun my-reverse (l)
  (if (= 1 (length l))
      l
    (append (my-reverse (cdr l))
          (list (car l)))))

(my-reverse (list 1 4 9 16 25))
