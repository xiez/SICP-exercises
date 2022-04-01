(defun horner-eval (x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (progn
                  (message "%s" higher-terms)
                  (+ (* x higher-terms) this-coeff)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 2))

(horner-eval 2 (list 1 3 0 5 0 1))


;; 1 + 3x + 0x^2 + 5x^3 + 0x^4 + 1x^5 = 1+6+40+32 = 79
;; ((((1x + 0)x + 5)x + 0)x + 3)x + 1 = 79


(* 0.36 0.60 0.60)
