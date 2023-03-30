;;; 5.26

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; "EC-Eval>>>"(factorial 2)
;; (factorial 2)
;; total-pushes=99, total-pops=99, maximum-depth=10
;; 2
;; "EC-Eval>>>"(factorial 5)
;; (factorial 5)
;; total-pushes=204, total-pops=204, maximum-depth=10
;; 120
;; "EC-Eval>>>"(factorial 8)
;; (factorial 8)
;; total-pushes=309, total-pops=309, maximum-depth=10
;; 40320

2a + b = 99
5a + b = 204

a = 105 / 3 = 35
b = 29

8 * 35 + 29 = 280 + 29 = 309


;;; 5.27

(define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n)))

;; "EC-Eval>>>"(factorial 2)
;; (factorial 2)
;; total-pushes=48, total-pops=48, maximum-depth=13
;; 2
;; "EC-Eval>>>"(factorial 5)
;; (factorial 5)
;; total-pushes=144, total-pops=144, maximum-depth=28
;; 120
;; "EC-Eval>>>"(factorial 8)
;; (factorial 8)
;; total-pushes=240, total-pops=240, maximum-depth=43
;; 40320

;;; 5.28

;;; iterative
;; "EC-Eval>>>"(factorial 2)
;; (factorial 2)
;; total-pushes=107, total-pops=107, maximum-depth=20
;; 2
;; "EC-Eval>>>"(factorial 5)
;; (factorial 5)
;; total-pushes=218, total-pops=218, maximum-depth=29
;; 120
;; "EC-Eval>>>"(factorial 8)
;; (factorial 8)
;; total-pushes=329, total-pops=329, maximum-depth=38
;; 40320

;;; recursive
;; "EC-Eval>>>"(factorial 2)
;; (factorial 2)
;; total-pushes=52, total-pops=52, maximum-depth=19
;; 2
;; "EC-Eval>>>"(factorial 5)
;; (factorial 5)
;; total-pushes=154, total-pops=154, maximum-depth=43
;; 120
;; "EC-Eval>>>"(factorial 8)
;; (factorial 8)
;; total-pushes=256, total-pops=256, maximum-depth=67
;; 40320

;;; 5.29

(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

;; "EC-Eval>>>"(fib 2)
;; (fib 2)
;; total-pushes=72, total-pops=72, maximum-depth=13
;; 1
;; "EC-Eval>>>"(fib 5)
;; (fib 5)
;; total-pushes=408, total-pops=408, maximum-depth=28
;; 5
;; "EC-Eval>>>"(fib 6)
;; (fib 6)
;; total-pushes=688, total-pops=688, maximum-depth=33
;; 8
;; "EC-Eval>>>"(fib 7)
;; (fib 7)
;; total-pushes=1136, total-pops=1136, maximum-depth=38
;; 13

2a + b = 13
5a + b = 28

a = 15 / 3 = 5
b = 3

7 * 5 + 3 = 38

;;; let S(n) be the number of pushes used in Fib(n)
;;; S(n) = S(n-1) + S(n-2) + k

;;; S(5) = 408 = 240 + 128 + 40
;;; S(6) = 408 + 240 + 40 = 688
;;; k = 40

;;; S(n) = aFib(n+1)+b
;;; S(n+1) = aFib(n+2)+b
;;; S(n+1) - S(n) = a ( Fib(n+2) - Fib(n+1) ) = aFib(n)
;;; Since, S(n) = S(n-1) + S(n-2) + 40
;;; S(n) - S(n-1) = S(n-2) + 40
;;; aFib(n-1) = S(n-2) + 40
;;; S(n-2) = aFib(n-1) - 40
;;; S(n) = aFib(n+1) - 40

;;; Since, S(5) = aFib(6) - 40 = 408
;;; So, a = 448 / 8 = 56
;;; Test for S(6) = 56 * Fib(7) - 40 = 56 * 13 - 40 = 688


;;; without tail recursion
;; "EC-Eval>>>"(fib 2)
;; (fib 2)
;; total-pushes=78, total-pops=78, maximum-depth=19
;; 1
;; "EC-Eval>>>"(fib 5)
;; (fib 5)
;; total-pushes=438, total-pops=438, maximum-depth=43
;; 5
;; "EC-Eval>>>"(fib 6)
;; (fib 6)
;; total-pushes=738, total-pops=738, maximum-depth=51
;; 8
;; "EC-Eval>>>"(fib 7)
;; (fib 7)
;; total-pushes=1218, total-pops=1218, maximum-depth=59
;; 13
