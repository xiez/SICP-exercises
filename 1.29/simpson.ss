(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-integers a b)
  (define (identity a) a)
  (define (inc n) (+ n 1))
  (sum identity a inc b))

(sum-integers 1 10)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube n)
  (* n n n))

(integral cube 0 1 0.0001)

;; ****************************************

(define multi-cube
  (lambda n
    (* 2 (apply cube n))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (inc n) (+ n 1))
  (define (evne? n) (= (remainder n 2) 0))

  (define (simpson-term k)
    (cond ((= k 0) (f (+ a (* k h))))
          ((= k n) (f (+ a (* k h))))
          ((even? k) (* 2 (f (+ a (* k h)))))
          (else (* 4 (f (+ a (* k h)))))))

  (* (/ h 3)
     (sum simpson-term a inc n)))

(simpson cube 0 1 100)
(simpson cube 0 1 1000)
