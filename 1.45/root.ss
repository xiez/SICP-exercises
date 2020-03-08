(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;; 2th-root
(define (square-root x)
  (fixed-point
   (lambda (y) (average y (/ x y)))
   1.0))

(define (square-root x)
  (fixed-point
   (average-damp (lambda (y) (/ x y)))
   1.0))

(define (square-root x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                         average-damp
                         1.0))

;; 3th-root
(define (cube-root x)
  (fixed-point
   (lambda (y) (average y (/ x y y)))
   1.0))

;; nth-root
(define (nth-root x n)
  (fixed-point
   (lambda (y) (average y (/ x (expt y (- n 1)))))
   1.0))

;; repeated dampenling
(define (compose f g)
  (lambda (x y) (f (g x) y)))

(define (repeated f n)
  (lambda (x)
    ((compose expt f) x n)))


(define (root x n)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (average-damp (average-damp))
                            1.0))

(root 2 2)
(fixed-point-of-transform (lambda (y) (/ 2 (expt y (- 2 1))))
                            (repeated average-damp 2)
                            1.0)
(fixed-point-of-transform (lambda (y) (/ 2 (expt y 1)))
                          (repeated average-damp 2)
                          1.0)
(fixed-point (((repeated average-damp 2))
              (lambda (y) (/ 2 (expt y 1)))) 1.0)


