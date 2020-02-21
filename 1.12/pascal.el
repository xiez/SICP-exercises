(defun pascal (row col)
  (cond ((= col 0) 1)
        ((= col row) 1)
        ((> col row) 0)
        (t (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))

(pascal 1 1)
(pascal 3 2)
(pascal 5 6)

