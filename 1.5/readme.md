Exercise 1.5. Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

(define (p) (p))
 
(define (test x y)
  (if (= x 0)
      0
      y))
Then he evaluates the expression

(test 0 (p))

What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer. (Assume that the evaluation rule for the special form if is the same whether the interpreter is using normal or applicative order: The predicate expression is evaluated first, and the result determines whether to evaluate the consequent or the alternative expression.)

In normal-order evaluation, (p) is not evaluated because predicate of if evaluates to true
In applicative-order evaluation, (p) will be evaluated, resulting in an infinite loop
In applicative-order evaluation, (p) is not evaluated because predicate of if evaluates to true
Error in both cases: (p) cannot be defined in terms of itself
In both cases, 0 is returned because (p) evaluates to itself, terminating the recursion
In normal-order evaluation, (p) will be evaluated, resulting in an infinite loop
