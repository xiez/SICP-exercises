(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; ****************************************

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
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

(search-for-primes 1001 3)              ;140 microseconds
(search-for-primes 10001 3)             ;180
(search-for-primes 100001 3)            ;200
(search-for-primes 1000001 3)           ;240
