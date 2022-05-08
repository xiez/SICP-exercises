(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))


;;; P1 finished, then P2 finished, -> (* 100 100 100)

;;; P2 finished, then P1 finished, -> (* 1000 1000)
