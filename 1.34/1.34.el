(let ((x 5))
  (+ (let ((x 3))
       (+ x (* x 10)))
     x))


(let ((x 2))
  (let ((x 3)
        (y (+ x 2)))
    (* x y)))


(defun f (g)
  (funcall g 2))

(defun square (n)
  (* n n))
(f 'square)

(f 'f)
