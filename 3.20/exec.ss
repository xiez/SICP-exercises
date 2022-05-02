#lang sicp

(define (cons2 x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car2 z) (z 'car))

(define (cdr2 z) (z 'cdr))

(define (set-car2! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr2! z new-value)
  ((z 'set-cdr!) new-value)
  z)

(define x (cons2 1 2))
(define z (cons2 x x))
(set-car2! (cdr2 z) 17)

(car2 x)
