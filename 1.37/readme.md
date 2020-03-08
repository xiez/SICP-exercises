Exercise 1.37. An infinite continued fraction is an expression of the form
f=N1D1+N2D2+N3D3+⋯
As an example, one can show that the infinite continued fraction expansion with the Ni and the Di all equal to 1 produces 1ϕ, where ϕ is the golden ratio (described in section 1.2.2). One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation — a so-called k-term finite continued fraction — has the form
f=N1D1+N2⋱+NkDk
Suppose that n and d are procedures of one argument (the term index i that return the Ni and Di of the terms of the continued fraction. Define a procedure cont-frac such that evaluating (cont-frac n d k) computes the value of the k-term finite continued fraction.

(define (cont-frac n d k)
  'your-code-here)
Check your procedure by approximating 1ϕ using

(define k 10)
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           k)
your-code-here
for successive values of k. How large must you make k in order to get an approximation that is accurate to 4 decimal places?

Try to write both an iterative and a recursive version of cont-frac.
