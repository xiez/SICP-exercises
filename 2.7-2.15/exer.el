(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; 2.7 ****************************************

(defun make-interval (lower upper)
  (cons lower upper))

(defun upper-bound (interval)
  (cdr interval))

(defun lower-bound (interval)
  (car interval))

;; 2.8 ****************************************
(defun sub-interval (x y)
  ;; x - y = x + (* -1 y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; 2.9 ****************************************
(defun width (interval)
  )

;; 2.10 ****************************************
(defun div-interval2 (x y)
  (if (and (>= (upper-bound y) 0) (<= (lower-bound y) 0))
      (error 'can not divide interval spans zero)
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

;; 2.11 ****************************************
(defun mul-interval2 (x y))

;; 2.12 ****************************************
(defun make-center-percent (center percent)
  (make-interval (* center (- 1 percent))
                 (* center (+ 1 percent))))

(defun center (interval)
  (/ (+ (lower-bound interval) (upper-bound interval)) 2))

(defun percent (interval)
  (- (/ (upper-bound interval) (center interval)) 1))

(center (make-center-percent 1.0 0.01))
(percent (make-center-percent 1.0 0.01))

;; 2.13 ****************************************
(defun par1 (r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defun par2 (r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(defun test()
  (let ((r1 (make-center-percent 1.0 0.01))
        (r2 (make-center-percent 2.0 0.05)))
    (message "%S" (par1 r1 r2))
    (message "%S" (par2 r1 r2))))
(test)

;; (0.6048231511254019 . 0.7339100346020762)
;; (0.6508650519031142 . 0.6819935691318327)

;; 2.14 ****************************************

(div-interval (make-center-percent 1.0 0.01)
              (make-center-percent 1.0 0.01))
;; (0.9801980198019802 . 1.0202020202020203) which is NOT equal to (1,1)

;; 2.15 ****************************************
;; According to the result of exer. 2.13, par2 does produce tigher error bounds which is a "better" program.

