(define (or-gate a1 a2 output)
  (let ((a3 (make-wire))
        (a4 (make-wire))
        (b (make-wire)))
    (inverter a1 a3)
    (inverter a2 a4)
    (and-gate a3 a4 b)
    (inverter b output)))

;;; or-gate dalay time = 2 * inverter-delay + and-gate-delay



