Exercise 1.34: Suppose we define the procedure

(define (f g)
  (g 2))

Then we have

(f square)
4
(f (lambda (z) (* z (+ z 1))))
6

What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.

The interpreter enters an infinite loop.
An error occurs, because 2 is not a procedure.
It evaluates to (2 2), which is 4.
The result is 2.
