#lang scheme

;;; put & get func
(define *op-table* (make-hash))
(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))
(define (get op type)
  (hash-ref *op-table* (list op type) false))

;;; tag func
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad taggad datum")))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum")))
;;; generic func
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
        (error
         "No method for these types -- APPLY-GENERIC %s"
         (list op type-tags))))))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)

  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))

  'install-rectangular-package-done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (equal-complex z1 z2)
    (and
     (= (real-part z1) (real-part z2))
     (= (imag-part z1) (imag-part z2))))
  (define (equal-zero? z)
    (and (= 0 (real-part z))
         (= 0 (imag-part z))))
  ;; interface to the rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (tag (equal-complex z1 z2))))
  (put '=zero? '(complex)
       (lambda (z) (tag (equal-zero? z))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))

  'install-complext-package-done)

;;; + - * /
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (tag (= x y))))
  (put '=zero? '(scheme-number)
       (lambda (x) (tag (= x 0))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

;;; add sub mul div
(define (add x y) (apply-generic 'add x y))


;;; test
(install-rectangular-package)
(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(add
 (make-complex-from-real-imag 3 4)
 (make-complex-from-real-imag 4 5))

;;; 2.78
(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(add
 (make-scheme-number 1)
 (make-scheme-number 2))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad taggad datum"))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum"))))

(add 1 2)

;;; 2.79
(install-scheme-number-package)
(install-rectangular-package)
(install-complex-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (equ? x y) (apply-generic 'equ? x y))

(equ?
 (make-scheme-number 1)
 (make-scheme-number 1)
 )

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(equ?
 (make-complex-from-real-imag 3 4)
 (make-complex-from-real-imag 3 4))

;;; 2.80
(define (=zero? x) (apply-generic '=zero? x))

(=zero? (make-scheme-number 0))
(=zero? (make-complex-from-real-imag 0 0))
