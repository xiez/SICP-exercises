#lang scheme

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 (display "run the proc")
                 (display proc)
                 result)
          result))))

(define-syntax delay
  (syntax-rules ()
    ((delay exp)
     (memo-proc (lambda () exp)))))

(define (force promise)
  (promise))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

(define (stream-car s)
  (car s))
(define (stream-cdr s)
  (force (cdr s)))
(define the-empty-stream '())
(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

;;; --------------------

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

;;; (stream-enumerate-interval 0 10) => (0 . (delay (stream-enumerate-interval low 10)) where low = 1
;;; (stream-map show (stream-enumerate-interval 0 10)) will print
;;; 0
;;; x will point to (0 . (delay (stream-map proc (stream-cdr s)))) where proc=show, and s = ...

(stream-ref x 5)
;;; 1
;;; 2
;;; 3
;;; 4
;;; 5
;;; x will point to (6 . delay (...))

(stream-ref x 7)
;;; 6
;;; 7


