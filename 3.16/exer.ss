#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define z1 (list 'a 'b 'c))             ;3

(define b (cons 'b '()))
(define a (cons 'a b))
(define z2 (cons b a))                  ;4

(define a2 (cons 'a '()))
(define b2 (cons a2 a2))
(define z3 (cons b2 b2))                ;7

(define c3 (cons 'c '()))
(set-cdr! c3 c3)
(define b3 (cons 'b c3))
(define z4 (cons 'a b3))                ;never returns

