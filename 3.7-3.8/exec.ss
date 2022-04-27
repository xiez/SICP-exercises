#lang scheme

;;; 3.7
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (current-balance) balance)
  (define (dispatch pwd m)
    (if (eq? pwd password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'current-balance) balance)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        (error "Incorrect password")))
  dispatch)


(define (make-joint account password new-password)
  (define (proxy pwd m)
    (if (eq? pwd new-password)
        (account password m)
        (error "Incorrect password")))
  proxy)

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'withdraw) 10)
(peter-acc 'open-sesame 'current-balance)
(paul-acc 'rosebud 'current-balance)

;;; 3.8
(define (make-monitored func)
  (let ((called? #f))
    (define (dispatch n)
      (if called? 0
        (begin (set! called? #t)
               (func n))))
    dispatch))

(define f (make-monitored (lambda (n) n)))

;; (define f
;;   (let ((called? #f))
;;     (lambda (x)
;;          (if called? 0
;;              (begin
;;                (set! called? #t)
;;                x)))))
