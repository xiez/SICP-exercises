#lang sicp

(define (self-evaluating? exp)
  (cond ((number? exp) #true)
        ((string? exp) #true)
        (else #false)))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating 
          exp target linkage))
        ;; ((quoted? exp) 
        ;;  (compile-quoted exp target linkage))
        ;; ((variable? exp)
        ;;  (compile-variable 
        ;;   exp target linkage))
        ;; ((assignment? exp)
        ;;  (compile-assignment
        ;;   exp target linkage))
        ;; ((definition? exp)
        ;;  (compile-definition
        ;;   exp target linkage))
        ;; ((if? exp)
        ;;  (compile-if exp target linkage))
        ;; ((lambda? exp)
        ;;  (compile-lambda exp target linkage))
        ;; ((begin? exp)
        ;;  (compile-sequence 
        ;;   (begin-actions exp) target linkage))
        ;; ((cond? exp) 
        ;;  (compile 
        ;;   (cond->if exp) target linkage))
        ;; ((application? exp)
        ;;  (compile-application 
        ;;   exp target linkage))
        (else
         (error "Unknown expression type: 
                 COMPILE" 
                exp))))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue)
                                    '()
                                    '((goto (reg continue)))))
        ((eq? linkage 'next)
         (make-instruction-sequence (empty-instruction-sequence)))
        (else
         (make-instruction-sequence '() '() `((goto (label ,linkage)))))))

;; ;;; test
;; (compile-linkage 'return)
;; (compile-linkage 'done)

(define (end-with-linkage linkage instruction-sequence)
  (preserving
   '(continue)
   instruction-sequence
   (compile-linkage linkage)))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and
             (needs-register? seq2 first-reg)
             (modifies-register? seq1 first-reg))
            (preserving (cdr regs) seq1 seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union 
      (registers-needed seq1)
      (list-difference 
       (registers-needed seq2)
       (registers-modified seq1)))
     (list-union
      (registers-modified seq1)
      (registers-modified seq2))
     (append (statements seq1)
             (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences 
         (car seqs)
         (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2)
         (list-union (cdr s1) s2))
        (else
         (cons (car s1)
               (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2)
         (list-difference (cdr s1) s2))
        (else 
         (cons (car s1)
               (list-difference (cdr s1)
                                s2)))))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target) `((assign ,target (const ,exp))))))


;;; test compile
(compile 42 'val 'done)
