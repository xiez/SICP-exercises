(defun make-rat (n d)
  (defun gcd (a b)
    (if (= b 0)
        a
      (gcd b (mod a b))))
  
  (let ((g (gcd n d)))
    (cons (/ n g)
          (/ d g))))

(defun numer (x) (car x))
(defun denom (x) (cdr x))
(defun print-rat (x)
  (message "%d/%d" (numer x) (denom x))
  )

(setq one-half (make-rat 1 2))
(print-rat one-half)

(print-rat (make-rat 2 -3))
(numer (make-rat -2 3))
(denom (make-rat 2 -3))

;; ****************************************


