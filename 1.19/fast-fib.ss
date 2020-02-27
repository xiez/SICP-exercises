;; ;; fib-iter
;; (1 0)
;; (1 1)
;; (2 1)
;; (3 2)
;; (5 3)
;; (8 5)

;; ;; fast-fib-iter
;; p = 0
;; q = 1
;; a = 1
;; b = 0
;; (1 0)
;; (1 1)
;; (2 1)
;; (3 2)


(define (fib n)
  (define (double n)
    (* 2 n))
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))      ; compute p'
                     (+ (double (* p q)) (square q))      ; compute q'
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))
