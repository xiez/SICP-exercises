

Exercise 2.38: The accumulate procedure is also known as fold-right, because it combines the first element of the sequence with the result of combining all the elements to the right. There is also a fold-left, which is similar to fold-right, except that it combines elements working in the opposite direction:

(define (fold-left op initial sequence)

  (define (iter result rest)

    (if (null? rest)

        result

        (iter (op result (car rest))

              (cdr rest))))

  (iter initial sequence))

What are the values of

(fold-right / 1 (list 1 2 3))

'your-input-here

✗

(fold-left / 1 (list 1 2 3)) ;please give the inverse, we haven't implemented rational numbers yet!

'your-input-here

✗

(fold-right list nil (list 1 2 3))

'your-input-here

✗

(fold-left list nil (list 1 2 3))

'your-input-here

✗

Give a property that op should satisfy to guarantee that fold-right and fold-left will produce the same values for any sequence. 
