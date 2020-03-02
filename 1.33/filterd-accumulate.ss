(define (filtered-accumulate combiner null-value term a next b filter?)
  (if (> a b)
      null-value
      (combiner (if (filter? a)
                    (term a)
                    null-value)
                (filtered-accumulate combiner null-value term (next a) next b filter?))))

(define (sum-odd-integers a b)
  (define (sum-odd term a next b)
    (filtered-accumulate + 0 term a next b odd?))
  (sum-odd identity a add1 b))

(sum-odd-integers 1 10)

(define (sum-integers a b)
  (define (sum term a next b)
    (filtered-accumulate + 0 term a next b identity))
  (sum identity a add1 b))

(sum-integers 1 10)


;; ****************************************
(define (square n)
  (* n n))

(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (next test-divisor)
    (if (= test-divisor 2)
        3
      (+ test-divisor 2)))
  (= n (smallest-divisor n)))

(define (sum-square-of-primes a b)
  (filtered-accumulate + 0 square a add1 b prime?))

(sum-square-of-primes 1 10)

;; ****************************************
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-relatively-primes n)
  (define (relatively-prime? i)
    (= (gcd i n) 1))

  (filtered-accumulate * 1 identity 1 add1 n relatively-prime?))

(product-relatively-primes 10)
