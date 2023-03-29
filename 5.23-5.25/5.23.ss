(
 ;; ...

 ;; eval let
 ev-let
 (assign exp (op let->combination) (reg exp))
 (goto (label eval-dispatch))
 ;; end eval let
 
 ;; eval cond
 ev-cond
 (assign exp (op cond->if) (reg exp))
 (goto (label eval-dispatch))
 ;; end eval cond

 ;; ...
)

