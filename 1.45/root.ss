(define tolerance 0.00001)

(define (repeated f n)
  (define (compose f g)
    (lambda (x)
      (f (g x))))
  
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (fixed-point-of-transform g transform guess)
  (define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (define (try guess)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
          (try next))))
    (try first-guess))
  (fixed-point (transform g) guess))

(define (root n x)
  (define (average-damp f)
    (define (average a b)
      (/ (+ a b) 2))
    (lambda (x) (average x (f x))))
  
  (if (= n 1)
      x
    (fixed-point-of-transform
     (lambda (y) (/ x (expt y (- n 1))))
     (repeated average-damp (floor (log n 2)))
     1.0)))
