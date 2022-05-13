#lang sicp

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (newline)
                 (display "run proc -> ")
                 (display proc)
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

(define (first-n s n)
  (define (iter s counter)
    (if (< counter n)
        (begin
          (display (stream-car s))
          (display " ")
          (iter (stream-cdr s) (+ counter 1)))

        (newline)))
  (iter s 0))

(define ones
  (cons-stream 1
               ones))

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

(define integers-starts-from-2 (stream-cdr integers))

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums s) )))

;;; --------------------
(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)


(define (sqrt-stream-louis x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream-louis x))))


;;; 3.63
(define s (sqrt-stream 2))
(define s-louis (sqrt-stream-louis 2))

(stream-ref s 10)                       ;10 calls to procedure
(stream-ref s 11)                       ;1 call to procedure

(stream-ref s-louis 10)                       ;55 calls to procedure
(stream-ref s-louis 11)                       ;11 call to procedure

;;; no memo-proc
(stream-ref s 10)                       ;55 calls to procedure
(stream-ref s 11)                       ;66 call to procedure
(stream-ref s-louis 10)                       ;55 calls to procedure
(stream-ref s-louis 11)                       ;66 call to procedure

;;; 3.64
(define (stream-cadr s)
  (stream-car (stream-cdr s)))

(define (stream-limit s tolerance)
  (if (< (abs (- (stream-car s)
                 (stream-cadr s))) tolerance)
      (stream-cadr s)
      (stream-limit (stream-cdr s) tolerance)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.0000001)

;;; 3.65

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
  (partial-sums (ln2-summands 1)))

(first-n ln2-stream 20)

(define square (lambda (x) (* x x)))
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
(first-n (euler-transform ln2-stream) 20)
