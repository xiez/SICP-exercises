(add-to-list 'load-path "..")
(require 'average "sicp")

(defun make-segment (p1 p2)
  (cons p1 p2))

(defun start-segment (s)
  (car s))

(defun end-segment (s)
  (cdr s))

(defun midpoint-segment (s)
  (make-point (average
               (x-point (start-segment s))
               (x-point (end-segment s)))
              (average
               (y-point (start-segment s))
               (y-point (end-segment s)))))

;; ----------------------------------------
(defun make-point (x y)
  (cons x y))

(defun x-point (p)
  (car p))

(defun y-point (p)
  (cdr p))

(defun print-point (p)
  (message "(%f,%f)" (x-point p) (y-point p)))

;; ****************************************

(setq p1 (make-point 0 1))
(print-point p1)

(setq p2 (make-point 1 0))
(print-point p2)

(print-point
 (midpoint-segment (make-segment p1 p2)))


