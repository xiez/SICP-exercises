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

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

;; (display-stream
;;  (mul-streams
;;     (stream-enumerate-interval 1 10)
;;     (stream-enumerate-interval 1 10)))

(define ones
  (cons-stream 1
               ones))

(define (first-10 s)
  (define (iter s n)
    (if (< n 10)
        (begin
          (display (stream-car s))
          (display " ")
          (iter (stream-cdr s) (+ n 1)))

        (newline)))
  (iter s 0))

(define integers
  (cons-stream 1
               (add-streams ones integers)))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (stream-ref stream n)
  (if (= n 0) (stream-car stream)
        (stream-ref (stream-cdr stream) (- n 1))))
;;; ========================================

;;; exer 3.54
(define factorials
  (cons-stream 1
               (mul-streams integers factorials)))

;;; factorials is a pair whose car is 1 and cdr is a promise to multiply of integers starts from 1 and factorials start from 1
;;; Evaluating the cdr gives us 1 and a promise to multiply the integers starts from 2 and factorials starts from 1
;;; Evaluating the cdr gives us 2 and a promise to multiply the integers starts from 3 and factorials starts from 2
;;; Evaluating the cdr gives us 6 and a promise to multiply the integers starts from 4 and factorials starts from 6

;;; exer 3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums s) )))
(first-10 (partial-sums integers))

;;; exer 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))


(define odds (stream-filter odd? integers))
(define evens (stream-filter even? integers))
(first-10 (merge odds evens))
(first-10 (scale-stream integers 2))

(define S (cons-stream
           1
           (merge (scale-stream S 2)
                  (merge (scale-stream S 3)
                         (scale-stream S 5)))))

;;; exer 3.57
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams fibs (stream-cdr fibs)))))
(stream-ref fibs 2)                     ;one additon
(stream-ref fibs 3)                     ;two additons
;;; with memo-proc, n-1 additions are performed for nth fibonacci

;;; exer 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;;; (expand 1 7 10) =
(cons-stream
 (quotient (* 1 10) 7)                  ;1
 (expand (remainder (* 1 10) 7) 7 10))

;; (expand 3 7 10) =
(cons-stream
 (quotient (* 3 10) 7)                  ;4
 (expand (remainder (* 3 10) 7) 7 10))

;; (expand 2 7 10) =
(cons-stream
 (quotient (* 2 10) 7)                  ;2
 (expand (remainder (* 2 10) 7) 7 10))

;;; exer 3.59
(define (integrate-series s)
  (define coefficients
    (div-streams ones integers))
  (mul-streams s coefficients))

(first-10
 (integrate-series
 (stream-enumerate-interval 1 10)))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(define cosine-series
  (cons-stream 1
               (scale-stream (integrate-series sine-series) -1)))
(define sine-series
  (cons-stream 0
               (integrate-series cosine-series)))

