(enumerate-interval 1 10)               ;(1 2 3 4 5 6 7 8)

(accumulate + 0 (enumerate-interval 1 10)) ;55
(accumulate append '() '((1) (2)))
(accumulate max 0 (map
                   (λ (x) (abs x))
                   (list 1 2 3 -3 -4 1))) ;4

(define positions-k1                    ;safe positions to place at column 1
  (filter
   (lambda (positions) (safe? 1 positions))
   (accumulate
    append
    '()
    (map
     (λ (rest-of-queens)
       (map
        (λ (new-row)
          (adjoin-position new-row 1 rest-of-queens))
        (enumerate-interval 1 8)))
     (list '()))
    )
   )
  )

(define positions-k2                    ;safe positions to place at column 2
  (filter
   (lambda (positions) (safe? 2 positions))
   (accumulate
    append
    '()
    (map
     (λ (rest-of-queens)
       (map
        (λ (new-row)
          (adjoin-position new-row 2 rest-of-queens))
        (enumerate-interval 1 8)))
     positions-k1)
    )
   )
  )


(define positions-k3                    ;safe positions to place at column 3
  (filter
   (lambda (positions) (safe? 3 positions))
   (accumulate
    append
    '()
    (map
     (λ (rest-of-queens)
       (map
        (λ (new-row)
          (adjoin-position new-row 3 rest-of-queens))
        (enumerate-interval 1 8)))
     positions-k2)
    )
   )
  )

(define positions-k4                    ;safe positions to place at column 3
  (filter
   (lambda (positions) (safe? 4 positions))
   (accumulate
    append
    '()
    (map
     (λ (rest-of-queens)
       (map
        (λ (new-row)
          (adjoin-position new-row 4 rest-of-queens))
        (enumerate-interval 1 8)))
     positions-k3)
    )
   )
  )



(accumulate append (list)
            (map
             (λ (i)
               (map (λ (j) (list i j))
                    (enumerate-interval 1 i)))
             (enumerate-interval 1 6)))


(accumulate append (list)
            (map
             (λ (i)
               (map (λ (j) (list i j))
                    (enumerate-interval 1 6)))
             (enumerate-interval 1 6)))

(define (nested n)
  (map
   (λ (i)
     (map
      (λ (j)
        (list i j))
      (enumerate-interval 1 i)))
   (enumerate-interval 1 n)))

(define (nested2 n)
  (accumulate append '()
              (map
               (λ (i)
                 (map
                  (λ (j)
                    (list i j))
                  (enumerate-interval 1 i)))
               (enumerate-interval 1 n))))

(define (nested3 n)
  (flatmap
   (λ (i)
     (map
      (λ (j)
        (list i j))
      (enumerate-interval 1 i)))
   (enumerate-interval 1 n)))

;;; ----------------------------------------
(adjoin-position 1 1 '())               ;((1 . 1))

(define board-size 8)
(map (lambda (new-row)
       (adjoin-position new-row 1 '()))
     (enumerate-interval 1 board-size))

(define rest-of-queens                  ;k-1 queens
  (list
   (cons 0 0)
   (cons 0 1)
   (cons 0 2)))

(flatmap
 (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
 rest-of-queens)
