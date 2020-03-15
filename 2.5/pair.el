(setq lexical-binding t)

(defun cons3 (a b)
  (* (expt 2 a) (expt 3 b)))

(defun car3 (c)
  (defun iter (c count)
      (if (not (= (mod c 2) 0))
          count
        (iter (/ c 2) (+ count 1))))
  (iter c 0))

(defun cdr3 (c)
  (defun iter (c count)
    (if (not (= (mod c 3) 0))
        count
      (iter (/ c 3) (+ count 1))))
  (iter c 0))

;; ****************************************
(car3 (cons3 0 9))
(cdr3 (cons3 5 9))
