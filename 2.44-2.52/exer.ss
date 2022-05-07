#lang sicp
(#%require sicp-pict)

(define eins einstein)
(paint eins)

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(paint (flipped-pars eins))

;;; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (conner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n -1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (conner (conner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right conner))))))

(define (square-limit painter n)
  (let ((quarter (conner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))


;;; higher-order operations

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;;; 2.45
(define  (split f-main f-small)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split f-main f-small) painter (- n 1))))
          (f-main painter (f-small smaller smaller))))))

(define right-split2 (split beside below))
(paint (right-split2 eins 4))

(define up-split2 (split below beside))
(paint  (up-split2 eins 4))

;;; 2.46

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (let ((x (+ (xcor-vect v1) (xcor-vect v2)))
        (y (+ (ycor-vect v1) (ycor-vect v2))))
    (make-vect x y)))

(define (sub-vect v1 v2)
  (let ((x (- (xcor-vect v1) (xcor-vect v2)))
        (y (- (ycor-vect v1) (ycor-vect v2))))
    (make-vect x y)))

(define (scale-vect s v)
  (let ((x (* s (xcor-vect v)))
        (y (* s (ycor-vect v))))
    (make-vect x y)))

;;; 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame fm)
  (car fm))

(define (edge1-frame fm)
  (cadr fm))

(define (edge2-frame fm)
  (caddr fm))

;; ---------
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame fm)
  (car fm))

(define (edge1-frame fm)
  (cadr fm))

(define (edge2-frame fm)
  (cddr fm))

;;; 2.48
(define (make-segment vstart vend)
  (cons vstart vend))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

;;; 2.49
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define outline-segs
  (list (make-segment (make-vect 0 0) (make-vect 1 0))
        (make-segment (make-vect 1 0) (make-vect 1 1))
        (make-segment (make-vect 1 1) (make-vect 0 1))
        (make-segment (make-vect 0 1) (make-vect 0 0))))

(define x-segs
  (list (make-segment (make-vect 0 0) (make-vect 1 1))
        (make-segment (make-vect 0 1) (make-vect 1 0))))

;;; 2.50
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2
