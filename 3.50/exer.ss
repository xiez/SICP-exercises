#lang sicp

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
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

;;; --------------------
;; (define (square n)
;;   (* n n))
;; (define (stream-map proc s)
;;   (if (stream-null? s)
;;       the-empty-stream
;;       (cons-stream (proc (stream-car s))
;;                    (stream-map proc (stream-cdr s)))))
;; (stream-map square
;;             (cons-stream 1
;;              (cons-stream 2
;;                           (cons-stream 3 '())))
;;             )



(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(stream-map +
            (cons-stream 1 (cons-stream 2 (cons-stream 3 '())))
            (cons-stream 4 (cons-stream 5 (cons-stream 6 '()))))
