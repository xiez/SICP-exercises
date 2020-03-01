(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-integers a b)
  (product identity a add1 b)
  )

(product-integers 1 4)

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (product-integers a b)
  (product-iter identity a add1 b)
  )

;; ****************************************

;; (* 1 2 3 4 .. n)
(define (factorial n)
  (product identity 1 add1 n))

(define (wallis-pi n)
  (define (nom-term n)
    (cond ((even? n) (+ 2 n))
          (else (+ 1 n))))
  (define (denom-term n)
    (cond ((even? n) (+ 1 n))
          (else (+ 2 n))))

  (* 4
     (/ (product nom-term 1 add1 n)
        (product denom-term 1 add1 n))))
