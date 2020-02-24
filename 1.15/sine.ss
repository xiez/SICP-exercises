(define (cube x) (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;; (sine 12.5)
;; (p (sine 4.16))
;; (p (p (sine 1.38)))
;; (p (p (p (sine 0.46))))
;; (p (p (p (p 0.15))))
;; (p (p (p (p (p (sine 0.05))))))

;; (p (p (p (p (p 0.05)))))
