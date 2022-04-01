Exercise 2.37 .

Suppose we represent vectors v=(vi)
as sequences of numbers, and matrices m=(mij) as sequences of vectors (the rows of the matrix). For example, the matrix
⎡⎣⎢⎢146257368469⎤⎦⎥⎥

is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation, we can use sequence operations to concisely express the basic matrix and vector operations. These operations (which are described in any book on matrix algebra) are the following:

    (dot-product v w) returns the sum ∑iviwi

(matrix-*-vector m w) returns the vector t
, where ti=∑jmijvj
(matrix-*-matrix m n) returns the matrix p
, where pij=∑kmiknkj
(transpose m) returns the matrix n
, where nij=mji

We can define the dot product as17

(define (dot-product v w)

  'sorry-map-is-not-implemented-yet

  (accumulate + 0 (map * v w)))

Fill in the missing expressions in the following procedures for computing the other matrix operations. (The procedure accumulate-n is defined in Exercise 2-36.)

(define (matrix-*-vector m v)

  (map 'your-answer-here m))

 

(define (transpose mat)

  (accumulate-n 'your 'answer mat))

 

(define (matrix-*-matrix m n)

  (let ((cols (transpose n)))

    (map 'your-answer-here m)))
