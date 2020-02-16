Exercise 1.4. Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:

(define (mystery a b)
  ((if (> b 0) + -) a b))
(define (mystery a b)
  ((if (> b 0) + -) a b))
Describe the behavior of the procedure by selecting which descriptions are equivalent. If you think that the procedure definition is invalid, choose the reasons why.

(+ a (if (> b 0) b (- b)))
(if (> b 0) (+ a b) (- a b))
Error: the leftmost subexpression is not a procedure
(+ a (abs b))
Error: the value of the leftmost subexpresion cannot depend on b
Error: the leftmost subexpression is not evaluated in the substitution model
