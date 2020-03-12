;; -*- mode: scheme; fill-column: 75; comment-column: 50; coding: utf-8 -*-

;; Chapter 1 of SICP

#| Exercise 1.1 Below is a sequence of expressions. What is the result
printed by the interpreter in response to each expression? Assume that the
sequence is to be evaluated in the order in which it is presented.
|#

> 10
10
> (+ 5 3 4)
12
> (- 9 1)
8
> (/ 6 2)
3
> (+ (* 2 4) (- 4 6))
6
> (define a 3)
a
> (define b (+ a 1))
b
> (+ a b (* a b))
19
> (if (and (> b a) (< b (* a b)))
      b
      a)
4
> (cond ((= a 4) 6)
	((= b 4) (+ 6 7 a))
	(else 25))
16
> (+ 2 (if (> b a) b a))
6
> (* (cond ((> a b) a)
	   ((< a b) b)
	   (else -1))
     (+ a 1))
16


#| Exercise 1.2
Translate the following expression into prefix form:

5 + 4 + (2 - (3 - (6 + 4/5)))
------------------------------
     3(6 - 2)(2 - 7)
|#

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))


#| Exercise 1.3 Define a procedure that takes three numbers as arguments
and returns the sum of the squares of the two larger numbers.
|#
(define (square x) (* x x))
(define (sum-of-squares a b) (+ (square a) (square b)))
(define (sum-squares-larger a b c)
  (cond ((and (> a c) (> b c)) (sum-of-squares a b))
	((and (> a b) (> c b)) (sum-of-squares a c))
	((and (> b a) (> c a)) (sum-of-squares b c)))
  )


#| Exercise 1.4 Observe that our model of evaluation allows for
combinations whose operators are compound expressions. Use this obseration
to describe the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
|#

If b is greater than 0, add a and b. Else subtract b from a.  This way if b
is positive, b is added to a. If b is negative, subtracting b cancels the
negative and you still end up adding b to a.


#| Exercise 1.5 Ben Bitdiddle has invented a test to determine whether the
interpreter he is faced with is using applicative-order evaluation or
normal-order evaluation. He defines the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

Then he evaluates the expression

(test 0 (p))

What behavior will Ben observe with an interpreter that uses
applicative-order evaluation? What behavior will he observe with an
interpreter that uses normal-order evaluation? Explain your answer. (Assume
that the evaluation rule for the special form if is the same whether the
interpreter is using normal or applicative order: The predicate expression
is evaluated first, and the result determines whether to evaluate the
consequent or the alternative expression.)
|#

Using applicative-order evaluation, Ben would observe an infinite loop
because applicative-order evaluation seeks to evaluate the subexpressions
first before applying the operator to the value of the other
subexpressions. Evaluating the arguments to `test`, the interpreter will
evaluate 0 for `x`, and then evaluate `p` for `y`. `p` is a function that
evaluates to itself, that is it calls the parameter, which happens to be
itself.

Using normal-order evaluation, Ben would observe 0 returned by the
interpreter. This is because the interpreter evaluates first expands the
expression and then evaluates the subexpressions as needed. When evaluating
`test`, the interpreter would substitute the `if` expression for `test`,
then evaluate the predicate. Because we pass in 0 for `x`, we return the
`consequent`, which is 0.
