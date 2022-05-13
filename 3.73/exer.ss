#lang sicp

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

;;; exer 3.73
(define (RC R C dt)
  (define (proc stream initial-value )
    (add-streams
     (scale-stream stream R)
     (integral
      (scale-stream stream (/ 1 C))
      initial-value
      dt)))
  proc)

(define RC1 (RC 5 1 0.5))
(first-n (RC1 s 1) 10) 

;;; exer 3.74
(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))

;;; exer 3.75
