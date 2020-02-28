(define (prime? n)
  (define (square n)
    (* n n))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;;
(define (search-for-primes n count)
  (define (iter n count)
    (cond ((< count 1) 0)
          ((timed-prime-test n) (search-for-primes (+ n 1) (- count 1)))
          (else (search-for-primes (+ n 1) count))))
  (iter n count))

(search-for-primes 1001 3)              ;20 microseconds
(search-for-primes 10001 3)             ;70
(search-for-primes 100001 3)            ;210
(search-for-primes 1000001 3)           ;590
