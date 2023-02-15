(define controller-text
  '(start
    (goto (label here))
    here
    (assign a (const 3))
    (goto (label there))
    here2
    (assign a (const 4))
    (goto (label there))
    there))


(extract-labels controller-text
                (lambda (insts labels)
                  labels))

(define labels
  '((start ((goto (label here))) ((assign a (const 3))) ((goto (label there))) ((assign a (const 4))) ((goto (label there))))
    (here ((assign a (const 3))) ((goto (label there))) ((assign a (const 4))) ((goto (label there))))
    (here ((assign a (const 4))) ((goto (label there))))
    (there)))

;; exer 5.8
;; (assoc 'here labels) will be
;; (here ((assign a (const 3))) ((goto (label there))) ((assign a (const 4))) ((goto (label there))))
;; so the content of `a` will be 3
;; A fix can be found in the `extract-labels` procedure in machine.ss
