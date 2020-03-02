(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display "Trying ... ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (f y)
  (fixed-point (lambda (x) (/ (log y) (log x)))
               2))
(f 1000)

;; average damping
(define (average a b)
  (/ (+ a b) 2))

(define (f-avg-damping y)
  (fixed-point (lambda (x)
                 (average x
                          (/ (log y) (log x))))
               2))
(f-avg-damping 1000)





