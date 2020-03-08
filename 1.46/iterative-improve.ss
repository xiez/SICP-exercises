(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve guess)))
        (if (good-enough? guess next)
            next
          (try next))))
    (try first-guess)))


(define (my-sqrt x)
  (define (close-enough? a b)
    (let ((radio (if (> a b) (/ a b) (/ b a))))
      (< radio 1.01)))
  
  ((iterative-improve
    close-enough?
    (lambda (y) (average y (/ x y))))
   1.0))

(my-sqrt 4)

;; ****************************************
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  ((iterative-improve
    close-enough?
    f)
   first-guess))

(define (my-sqrt2 x)
  (define (average-damp f)
    (define (average a b)
      (/ (+ a b) 2))
    (lambda (x) (average x (f x))))

  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(my-sqrt2 4)
