(defun perimeter (rect)
  (* 2 (+ (length-rect rect)
          (width-rect rect))))

(defun area (rect)
  (* (length-rect rect)
     (width-rect rect)))

;; ----------------------------------------
(defun make-rect (len-seg wid-seg)
  (cons len-seg wid-seg))

(defun length-rect (rec)
  (let ((len-seg (car rec)))
        (- (x-point (end-segment len-seg))
           (x-point (start-segment len-seg)))))

(defun width-react (rec)
  (let ((wid-seg (cdr rec)))
    (- (y-point (end-segment len-seg))
       (y-point (start-segment len-seg)))))

;; ----------------------------------------
(defun make-rect (p1 p2)
  (cons p1 p2))

(defun length-rect (rec)
  (let ((p1 (car rec))
        (p2 (cdr rec)))

    (- (x-point p2) (x-point p1))))

(defun width-rect (rec)
  (let ((p1 (car rec))
        (p2 (cdr rec)))

    (- (y-point p2) (y-point p1))))
