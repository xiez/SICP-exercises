For better understanding of the examples in Structure and Interpretation of Computer Programs(SICP), as well as learning Emacs lisp, all exercises will be answered both in Scheme and Elisp.

SICP Interactive Version:

https://xuanji.appspot.com/isicp/index.html

Racket SICP collections:

https://docs.racket-lang.org/sicp-manual/

Emacs lisp evaluation:

https://www.gnu.org/software/emacs/manual/html_node/efaq/Evaluating-Emacs-Lisp-code.html

**NOTE 1**: Emacs do not have optimization of tail recursion according to this [SO answer](https://stackoverflow.com/questions/38493904/why-is-there-no-tail-recursion-optimization-in-emacs-lisp-not-but-like-other-sc), we need to increase the default `max-lisp-eval-depth`, otherwise we'll run into errors easily.

Add following lines to `.emacs`:

```
;; increase max-lisp-eval-depth from 500 to 100000
(setq max-specpdl-size         100000
      max-lisp-eval-depth      100000)
```

**NOTE 2**: Add `(setq lexical-binding t)` to the buffer to enable Elisp closure, see [lexical bidning](https://www.emacswiki.org/emacs/LexicalBinding).

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

## 1.3 Formulating Abstractions with Higher-Order Procedures

### Definition

Higher-order procedures are procedures that manipulate procedures in which accept procedures as arguments or return procedures as values.

### Why need Higher-Order procedures ?

Quote from the book:

> Yet even in numerical processing we will be severely limited in our ability to create abstractions if we are restricted to procedures whose parameters must be numbers. Often the same programming pattern will be used with a number of different procedures.

### Lambda

In order to eliminate the definition of trival procedures in higher-order procedure, `lambda` is introduced to define procedures on the fly.

By the way, the name of `lambda` is a bit obscure, it would be clearer and less intimidating to people learning Lisp if a name more obvious than `lambda`, such as `make-procedure`, were used.

### First-class elements

Quote from the book:

> In general, programming languages impose restrictions on the ways in which computational elements can be manipulated. Elements with the fewest restrictions are said to have first-class status. Some of the “rights and privileges” of first-class elements are:
> - They may be named by variables.
> - They may be passed as arguments to procedures.
> - They may be returned as the results of procedures.
> - They may be included in data structures.65

## 2.1  Introduction to Data Abstraction

Just like procedure abstraction, data abstraction is a methodology that enables us to isolate how a compound data is used from the details of how it is constructed.

As the example of rational numbers demonstrates,

![data abstraction barriers](https://xuanji.appspot.com/isicp/images/ch2-Z-G-6.gif)

lower layers expose only a few methods (`constructor & selectors` ) for the upper layers to use, how the lower layers are implemented is irrelevant to the upper layer.

### Data or Procedure ??

A valid but conter -intuitive implementation of `cons`:

```
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))

(define (cdr z) (z 1))
```

### What is mean by Data ?

> In general, we can think of data as defined by some collection of selectors and constructors, together with specified conditions that these procedures must fulfill in order to be a valid representation.

This definition does not distinguish whether Data is "real" data structure or not, as long as it satisfies the conditions.
