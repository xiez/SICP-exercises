;; recursive
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

;; iterative

;; ;; initial state
;; a <- f(2) = 2
;; b <- f(1) = 1
;; c <- f(0) = 0

;; ;; transform
;; a <- f(2) + 2f(1) + 3f(0) = a + 2b + 3c
;; b <- a
;; c <- b

(define (f-iter n)
  (define (f-iter-aux a b c count)
    (cond ((= count 0) c)
          (else (f-iter-aux (+ a (* 2 b) (* 3 c))
                            a
                            b
                            (- count 1)))))
  (f-iter-aux 2 1 0 n))



