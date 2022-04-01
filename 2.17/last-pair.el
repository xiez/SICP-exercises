(defun last-pair (l)
  (if (= 1 (length l))
      l
    (last-pair (cdr l))))

(last-pair (list 23 72 149 34))
