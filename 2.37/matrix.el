(setq m (list (list 1 2 3 4)
              (list 4 5 6 6)
              (list 6 7 8 9)
              (list 1 1 1 1)))
(setq v (list 1 1 1 1))

;; ****************************************
;; (defun dot-product (v w)
;;   (if (null v)
;;       0
;;     (+
;;      (* (car v) (car w))
;;      (dot-product (cdr v) (cdr w)))
;;     ))

(defun dot-product (v w)
  (accumulate
   '+
   0
   (accumulate-n
    '*
    1
    (list v w))))

(dot-product (list 2 2) (list 3 4))

;; ****************************************

(defun matrix-*-vector (m v)
  (map
   (lambda (x) (dot-product x v))
   m))

(matrix-*-vector m v)

;; ****************************************
(defun transpose (mat)
  (accumulate-n 'cons nil mat))

(transpose m)

;; ****************************************
(defun matrix-*-matrix (m n)
  (let ((cols (transpose n)))
    (message "%s %s" (car m) (car cols))

    (map
     (lambda (x) (matrix-*-vector cols x))
     m)))

(matrix-*-matrix m m)
