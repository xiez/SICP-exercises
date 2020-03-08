(define dx 0.1)

(define average (a b c)
  (/ (+ a b c) 3))

(define (smooth f)
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (n-fold-smooth f n)
  (repeated smooth) n)
