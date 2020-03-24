Exercise 2.33: Fill in the missing expressions to complete the following definitions of some basic list-manipulation operations as accumulations:

(define (map p sequence)
  (accumulate (lambda (x y) 'you-answer-here) nil sequence))
✗
(define (append seq1 seq2)
  (accumulate cons 'your-answer-here 'your-answer-here))
✗
(define (length sequence)
  (accumulate 'your-answer-here 0 sequence))
