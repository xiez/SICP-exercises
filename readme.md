For better understanding of the examples in Structure and Interpretation of Computer Programs(SICP), as well as learning Emacs lisp, all exercises will be answered both in Scheme and Elisp.

SICP Interactive Version:

https://xuanji.appspot.com/isicp/index.html

Racket SICP collections:

https://docs.racket-lang.org/sicp-manual/

Emacs lisp evaluation:

https://www.gnu.org/software/emacs/manual/html_node/efaq/Evaluating-Emacs-Lisp-code.html


## 1.1 The Elements of Programming

### Exercises

1.3 - 1.8

## 1.2 Procedures and the Processes They Generate

The discussion in this section about the difference between recursion and iteration is a must read, and priceless.

### Iterative Process vs Recursive Process vs Recursive Procedure

Iterative process: the program variables provide a complete description of the state of the process at any point. If we stopped the computation between steps, all we would need to do to resume the computation is to supply the interpreter with the values of the three program variables.

Recursive process: there is some additional “hidden” information, maintained by the interpreter and not contained in the program variables, which indicates “where the process is” in negotiating the chain of deferred operations. The longer the chain, the more information must be maintained.

Recursive procedure: the syntactic fact that the procedure definition refers (either directly or indirectly) to the procedure itself.

### Recursive vs Iterative factorial

```
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
```

`factorial` is a linear recursive process (it has hidden information) and a recursive procedure (it calls itself directly). The call stack of procedure grows and shrinks.

`fact-iter` is a iterative process (no hidden information, `product,counter,max-count` contain the complete state) and a recursive procedure (it calls itself directly). The call stack of procedure does not grow and shrink with **tail-recursive** implementation.

For common languages (C, Python) that do not implement tail recursion, describe iterative process only by resorting to specail purpose “looping constructs” such as do, repeat, until, for, and while.

Scheme interpreter implements tail recursion, so the special iteration constructs are usefull only as syntactic sugar.
