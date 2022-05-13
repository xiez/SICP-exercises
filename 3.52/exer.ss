#lang scheme

;; (define (delay exp)
;;   (lambda () exp))

(define-syntax delay
  (syntax-rules ()
    ((delay exp)
     (lambda () exp))))

;; (define (delay exp)
;;   (memo-proc (lambda () exp)) )

(define (force promise)
  (promise))
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

(define (cons-stream a b)
  (cons a (delay b)))
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
(define sum 0)                          ;sum = 0

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20))) ;seq -> (cons 1 . (...)) sum = 1

(define y (stream-filter even? seq))    
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(stream-ref y 7)
(display-stream z)
