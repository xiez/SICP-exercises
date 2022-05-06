#lang sicp

(define or-gate-delay 1)

(define (or-gate a1 a2 output)
  (define (or-proc)
    (let ((new-val (logical-or (get-signal a1)
                               (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-val)))))
  (add-action! a1 or-proc)
  (add-action! a2 or-proc)
  'ok)

(define (logical-or a b)
  (cond ((or (= a 1) (= b 1)) 1)
        ((and (= a 0) (= b 0)) 0)
        (else (error "Invalid signal" a b))))


;;; missing part for the test
(define (call-each procs)
  (if (not (null? procs))
      (begin
        ((car procs))
        (call-each (cdr procs)))
      'done))

(define (make-wire)
  ;; A wire is a computation object represents as procedure.
  ;; It has three functions:
  ;; - `add-action`: bind a action procedure to the wire.
  ;; - `get-signal`: return the internal signal value.
  ;; - `set-signal!`: mutate the internal signal value and call the actions.
  (let ((signal-val 0) (actions '()))
    (define (add-action! proc)
      (set! actions (cons proc actions)))
    (define (get-signal)
      signal-val)
    (define (set-signal! new-val)
      (if (not (= new-val signal-val))
          (begin
            (set! signal-val new-val)
            (call-each actions)
            ))
      'done)
    (define (dispatch m)
      (cond ((eq? m 'add-action!) add-action!)
            ((eq? m 'get-signal) get-signal)
            ((eq? m 'set-signal!) set-signal!)
            (else (error "Invalid message" m))))

    dispatch))

(define (set-signal! w val)
  ((w 'set-signal!) val))
(define (get-signal w)
  ((w 'get-signal)) )
(define (add-action! w action-proc)
  ((w 'add-action!) action-proc))
(define (prob name w)
  (add-action! w (lambda ()
                   (newline)
                   (display "Name: ")
                   (display name)
                   (display ", current signal-val: ")
                   (display (get-signal w))
                   (newline))))

(define (after-delay time proc)
  ;; call the procedure with no delay time
  (display "After-delay time ")
  (display time)
  (display "... call proc ")
  (display proc)
  (newline)
  (proc))

;;; ==================== TEST ====================

;;; create wires and assemble them into the gate
(define a1 (make-wire))
(define a2 (make-wire))
(define b (make-wire))
(prob 'wire-b b)                        ;set a prob on the output wire
(or-gate a1 a2 b)

(set-signal! a1 1)
(= (get-signal b) 1)

(set-signal! a1 0)
(set-signal! a2 0)
(= (get-signal b) 0)


