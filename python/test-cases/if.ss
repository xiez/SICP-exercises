(=
 (if (null? 1)
    1
    2)
 2
 )

(=
 (if (null? (list))
    1
    2)
 1
 )

(= 2
   (if (null? 1)
       1
       (if (null? (list))
           2
           3))
   )


(= 3
   (if (null? 1)
       1
       (if (null? 1)
           2
           3))
   )
