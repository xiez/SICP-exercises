(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

;;; exer 4.25
;;; in applicative-order language, (* n (factorial (- n 1))) will be evaluated infinitely.

;;; in normal-order language, it works as expected.

;;; exer 4.26
((unless? exp) (eval (unless->if exp) env))

(define (unless->if exp)
  (define (predicate exp)
    (cadr exp))
  (define (alternative exp)
    (caddr exp))
  (define (consequent exp)
    (cadddr exp))
  (make-if (predicate exp)
           (consequent exp)
           (alternative exp)))

