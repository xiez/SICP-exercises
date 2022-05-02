#lang sicp

(define (count-pairs x)
  (let ((counted '()))

    (define (counted? x)
      (memq x counted))

    (define (set-counted! x)
      (set! counted (cons counted x)))

    (define (count x)
      (if (or (not (pair? x)) (counted? x))
        0
        (begin
          (set-counted! x)
          (+ (count (car x))
             (count (cdr x))
             1))))

    (count x)))

(define z1 (list 'a 'b 'c))             ;3
(count-pairs z1)

(define b (cons 'b '()))
(define a (cons 'a b))
(define z2 (cons b a))                
(count-pairs z2)


