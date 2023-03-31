(
 ;; ...
 ev-cond
 (save continue)
 (assign unev (op cond-clauses) (reg exp))

 ev-cond-test-pred-loop
 (assign exp (op first-clause) (reg unev)) ;get the first clause
 (test (op cond-else-clause?) (reg exp)) ;if it's else clause, go eval else clause
 (branch (label ev-cond-else-clause))   ;otherwise, go eval predicate of the clause
 (save continue)
 (assign continue (label ev-cond-pred-clause-1))
 (save unev)
 (save exp)
 (assign exp (op cond-predicate) (reg exp))
 (goto (label eval-dispatch))

 ev-cond-pred-clause-1
 (restore exp)
 (restore unev)
 (restore continue)
 (test (op true?) (reg val))            ;if the eval result is true
 (branch (label ev-cond-pred-clause-2))         ;eval clause actions
 (assign unev (op rest-clauses) (reg unev)) ;otherwise, remove first clause
 (goto (label ev-cond-test-pred-loop))  ;go next loop

 ev-cond-pred-clause-2
 (assign unev (op cond-actions) (reg exp)) ;get clause actions
 (goto (label ev-sequence))             ;go eval actions sequencially

 ev-cond-else-clause
 (assign unev (op cond-actions) (reg exp)) ;get clause actions
                                        ; (TODO: 'else' should be the last clause)
 (goto (label ev-sequence))             ;go eval actions sequencially

 ;; ...
 )
