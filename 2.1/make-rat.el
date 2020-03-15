(add-to-list 'load-path "..")
(require 'remainder "sicp")
(require 'gcd "sicp")


(defun make-rat (n d)
  (let ((g (gcd n d)))
    (cond ((and (< n 0) (> d 0))
           (cons (/ (- n) g)
                 (/ (- d) g)))
          ((and (> n 0) (< d 0))
           (cons (/ (- n) g)
                 (/ (- d) g)))
          (t (cons (/ n g)
                 (/ d g))))))

(defun numer (x) (car x))
(defun denom (x) (cdr x))
(defun print-rat (x)
  (message "%d/%d" (numer x) (denom x)))

(setq one-half (make-rat 1 2))
(print-rat one-half)

(setq rat (make-rat -1 -2))
(print-rat rat)
(numer rat)
(denom rat)
