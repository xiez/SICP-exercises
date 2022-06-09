;; exer 4.27
(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

 ;; Give the missing values in the following sequence of interactions, and explain your answers.38

(define w (id (id 10)))
;;; L-Eval input:
count
;;; L-Eval value:
1
;; `id` is evaled once in `eval-definition` as a compound procedure

;;; L-Eval input:
w
;;; L-Eval value:
10
;; w is a thunk-obj, which is forced in the driver-loop.
;; so, count is called twise.

;;; L-Eval input:
count
;;; L-Eval value:
3

;; exer 4.28
;; if the operator of an expression is thunk, eval will failed, while actual-value works as expected.

;; consider following expressions,
(define f
  (lambda (x) x))
(define g
  (lambda (func) func))
((g f) 1)

;; exer 4.29
(define (fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (square x)
  (* x x))
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100
;;; L-Eval input:
count
;;; L-Eval value:
1 ; x will only be forced once
2 ; if no memoization

;; exer 4.30
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;; `proc` is forced as the operator os `(proc (car items))`
;; `items` is forced as the arguemnt to the `cdr`

(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

(p1 1)
;; original eval-sequence
;; => (1 2)
;; Cy's eval-sequence
;; => (1 2)

(p2 1)
;; original eval-sequence
;; => 1
;;   p is evaled as the operator of exp `(p (set! x (cons x '(2))))`, the argument is still a thunk, and not been passed to any primitive function, so it's not forced
;; Cy's eval-sequence
;; => (1 2)

;; I'm more in favor of Cy's approach.
