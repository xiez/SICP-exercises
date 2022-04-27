For a better understanding of the examples in Structure and Interpretation of Computer Programs(SICP), as well as learning Emacs lisp, all exercises will be answered both in Scheme and Elisp.

SICP Interactive Version:

https://xuanji.appspot.com/isicp/index.html

Racket SICP collections:

https://docs.racket-lang.org/sicp-manual/

Racket SICP Picture language (used in Chapter 2.2):

https://docs.racket-lang.org/sicp-manual/SICP_Picture_Language.html

Emacs lisp evaluation:

https://www.gnu.org/software/emacs/manual/html_node/efaq/Evaluating-Emacs-Lisp-code.html

**NOTE 1**: Emacs do not have the optimization of tail recursion according to this [SO answer](https://stackoverflow.com/questions/38493904/why-is-there-no-tail-recursion-optimization-in-emacs-lisp-not-but-like-other-sc), we need to increase the default `max-lisp-eval-depth`, otherwise we'll run into errors easily.

Add the following lines to `.emacs`:

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

The discussion in this section about the difference between recursion and iteration is a must-read, and priceless.

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

`fact-iter` is an iterative process (no hidden information, `product, counter and max-count` contain the complete state) and a recursive procedure (it calls itself directly). The call stack of procedure does not grow and shrink with **tail-recursive** implementation.

For common languages (C, Python) that do not implement tail recursion, describe iterative process only by resorting to special-purpose “looping constructs” such as do, repeat, until, for, and while.

The Scheme interpreter implements tail recursion, so the special iteration constructs are useful only as syntactic sugar.

## 1.3 Formulating Abstractions with Higher-Order Procedures

### Definition

Higher-order procedures are procedures that manipulate procedures that accept procedures as arguments or return procedures as values.

### Why need Higher-Order procedures?

Quote from the book:

> Yet even in numerical processing we will be severely limited in our ability to create abstractions if we are restricted to procedures whose parameters must be numbers. Often the same programming pattern will be used with several different procedures.

### Lambda

To eliminate the definition of trivial procedures in higher-order procedures, `lambda` is introduced to define procedures on the fly.

By the way, the name `lambda` is a bit obscure, it would be clearer and less intimidating to people learning Lisp if a name more obvious than `lambda`, such as `make-procedure`, were used.

### First-class elements

Quote from the book:

> In general, programming languages impose restrictions on the ways in which computational elements can be manipulated. Elements with the fewest restrictions are said to have first-class status. Some of the “rights and privileges” of first-class elements are:
> - They may be named by variables.
> - They may be passed as arguments to procedures.
> - They may be returned as the results of procedures.
> - They may be included in data structures.65

## 2.1  Introduction to Data Abstraction

Just like procedure abstraction, data abstraction is a methodology that enables us to isolate how compound data is used from the details of how it is constructed.

As the example of rational numbers demonstrates,

![data abstraction barriers](https://xuanji.appspot.com/isicp/images/ch2-Z-G-6.gif)

lower layers expose only a few methods (`constructor & selectors` ) for the upper layers to use, how the lower layers are implemented is irrelevant to the upper layer.

### Data or Procedure ??

A valid but counter-intuitive implementation of `cons`:

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

### What is meant by Data?

> In general, we can think of data as defined by some collection of selectors and constructors, together with specified conditions that these procedures must fulfill in order to be a valid representation.

This definition does not distinguish whether Data is a "real" data structure or not, as long as it satisfies the conditions.

## 2.2 Hierarchical Data and the Closure Property

The "closure property" of `cons`, meaning, the result of cons can be combined using cons again. It permits us to create "sequences" and "hierarchical" structures (List, Tree).

> The use of the word "closure" here comes from abstract algebra, where a set of elements is said to be closed under an operation if applying the operation to elements in the set produces an element that is again an element of the set. The Lisp community also (unfortunately) uses the word "closure" to describe a totally unrelated concept: A closure is an implementation technique for representing procedures with free variables. We do not use the word "closure" in this second sense in this book. 


The examples in this section demonstrate two powerful design principles, "conventional interfaces" and "stratified design".

### Conventional Interfaces

```
(define (salary-of-highest-paid-programmer records)
  (accumulate max
              0
              (map salary
                   (filter programmer? records))))
```

> Sequences, implemented here as lists, serve as a conventional interface that permits us to combine processing modules.

### Stratified Design

The picture language demonstrates the power of stratified design in a complex system. Just like data abstraction barriers in section 2.1, the picture language uses primitive painters `segments->painter`, combines those primitive painters to geometric combiners such as `beside` and `below`, then uses those combiners as primitives to work at a higher level, such as `square-of-four`.

> Stratified design helps make programs robust, that is, it makes it likely that small changes in a specification will require correspondingly small changes in the program.

## 2.3 Symbolic Data

Symbolic data has a quotation mark `'` at the beginning of the object, which is used to represent arbitrary symbols as data, e.g, `'+, '-, '*, '/`.

From the interpreter's point of view, everything in the Lisp is a list, quotation mark is just a single-character abbreviation for wrapping the next complete expression with a quote to form `(quote <expression>)`.

A quotation can also be placed to compound objects, e.g. `'(+ 1 2)`, that represents a list. Quote an empty list `'()` represents as `nil`.

## 2.4 Multiple Representations for Abstract Data

Abstraction barriers in section 2.1 are "horizontal", lower level implementations that can be replaced without affecting the upper level.

This section introduces "vertical" barriers, which are used to separate data that have multiple representations. 

As the example of the complex-arithmetic package demonstrates, a new representation can be added without affecting the usage of the upper level.

![multiple representations of data](https://xuanji.appspot.com/isicp/images/ch2-Z-G-54.gif)

Each representation of data has its own constructor and a "special tag" which is used to distinguish what type of data is.

For example, Ben's rectangular representation:

`(cons 'rectanular (cons 3 4))`

Alyssa's polar representation:

`(cons 'polar (cons 3 4))`

Generic interfaces such as `real-part, imag-part, magnitude, angle` will check the type of data, and dispatch the corresponding selector procedure.

```
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))
```

![multiple representations of data](https://xuanji.appspot.com/isicp/images/ch2-Z-G-62.gif)

### Data-Directed Programming

The dispatching on type strategy is a powerful strategy for obtaining modularity in system design. But it has two weaknesses:

1. Generic selector such as `real-part` has to know about all the different representations.

2. Individual representations may have name conflicts, so it needs to be named like `real-part-rectangular` and `real-part-polar`.

> The issue underlying both of these weaknesses is that the technique for implementing generic interfaces is not `additive`.

Data-directed programming uses an "operation-type" table to make generic interfaces additive. To add a new representation, we need only add new entries to the table and the interfaces remain intact.

![Table of operations for the complex-number system](https://xuanji.appspot.com/isicp/images/ch2-Z-G-63.gif)

The table is stored in a data structure similar to the hash table, with the `put` and `get` procedures.

```
(define (install-rectangular-package)
  ...
  (put 'real-part '(rectangular) real-part)
)

(define (install-polar-package)
  ...
  (put 'real-part '(polar) real-part)
)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
```

### Message passing

> An alternative implementation strategy is to decompose the table into columns and, instead of using "intelligent operations" that dispatch on data types, work with "intelligent data objects" that dispatch on operation names. 

```
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define (real-part z) (apply-generic 'real-part z))
```

This strategy is similar to the method call in object-oriented programming and will be detailed explained in detail in Chapter 3.

## 2.5 Systems with Generic Operations

Generic operations in section 2.4 mainly focused on different representations of the same type of data. In this section, generic operations can be further applied to different types of data.

![Generic arithmetic system](https://xuanji.appspot.com/isicp/images/ch2-Z-G-64.gif)

Consider the addition of complex number to an ordinary number `(add <complex-number> <scheme-number>)`.

One way to handle this is to design a procedure for each possible combination of types. For example,

```
;; to be included in the complex package
(define (add-complex-to-schemenum z x)
  (make-from-real-imag (+ (real-part z) x)
                       (imag-part z)))

(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))
```

This technique works, but it is cumbersome and does not scale if we have several types of data (e.g. rational number, real number, complex number...).

Since those types are not completely independent, scheme numbers can be converted to rational numbers which can be converted to real numbers and further to complex numbers. This process is called `coercion`.

We can define the following converter procedure:

```
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)
```

and put it to a hash table, then modify the `apply-genric` procedure to look up this table if no procedure is found in the previous operation-type table.

```
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))
```



