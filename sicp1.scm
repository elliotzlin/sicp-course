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


#| Exercise 1.6 Alyssa P. Hacker doesn't see why `if` needs to be provided
as a special form. "Why can't I just define it as an ordinary process in
terms of cond?" she asks. Alyssa's friend Eva Lu Ator claims this can
indeed be done, and she defines a new version of `if`:

(define (new-if predicate
          then-clause
	  else-clause)
  (cond (predicate then-clause)
      (else else-clause))

Eva demonstrates the program for Alyssa:

(new-if (= 2 3) 0 5)
5

(new-if (= 1 1) 0 5)
0

Delighted, Alyssa uses `new-if` to rewrite the square-root program:

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

What happens when Alyssa attempts to use this to compute square roots?
Explain.
|#

Alyssa will spawn an infinite loop. The interpreter, which uses
applicative-order evalutation, will try to evaluate all the parameters of
`new-if`. This results in an infinite as the interpreter will try to
evaluate the `else-clause` parameter, which is another invocation of
`new-if`, and the process will repeat recursively.

Put another way, `new-if` is a procedure and `if` is a special form. Under
applicative-order evaluation all the sub-expressions of `new-if` are
evaluated before `new-if` is applied to the value of the operands, whereas
using `if`, only one of the consequent expressions is evaluated at a time.


#| Exercise 1.7 The `good-enough?` test used in computing square roots will
not be very effective for finding the square roots of very small
numbers. Also, in real computers, arithmetic operations are almost always
performed with limited precision. This makes our test inadequate for very
large numbers. Explain these statements, with examples showing how the test
fails for small and large numbers. An alternative strategy for implementing
`good-enough?` is to watch how `guess` changes from one iteration to the
next and to stop when the change is a very small fraction of the
guess. Design a square-root procedure that uses this kind of end test. Does
this work better for small and large numbers?
|#

The `good-enough` test is not adequate for finding the square roots of
very small numbers because we don't have enough precision. Essentially,
because we check if the square of our guess is within 0.001 of our number,
we are more likely to pass the test if the numbers are very small.

At the other end of the extreme, this test is inadequate for very large
numbers because of hardware limitations. The limited precision spoken of
refers to how we represent numbers at the level of bits, namely we use
floating point. Therefore, when multiplying very large numbers together, we
lose some precision.

;;; The more correct answer for large numbers is that for very large
;;; numbers, the machine precision is unable to represent small differences
;;; bewteen large numbers. The algorithm might never terminate as
;;; good-enough? will always return #f.
;;;
;;; An example for small numbers is (sqrt 0.0001), which yields 0.03230844
;;; instead of 0.01. For a large number, (sqrt 10000000000000), which
;;; should loop forever with the given implementation.

(define (sqrt-iter guess old-guess x)
  (if (new-good-enough? guess old-guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (new-good-enough? guess old-guess)
  (if (< (/ (abs (- guess old-guess)) old-guess) 0.001)))


This does work better for large and small numbers because calculating the
fraction of the change over the guess uses a relatively constant amount of
precision because we are calculating relative values.


#| Exercise 1.8 Newton's method for cube roots is based on the fact that if y is an
approximation of the cube root of x, then a better approximation is given
by the value

x/y^2 + 2y
__________
    3

Use this formula to implement a cube-rot procedure analogous to the
square-root procedure. (In 1.3.4 we will see how to implement Newton's
method in general as an abstraction of these square-root and cube-root
procedures.)
|#

(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root-iter guess old-guess x)
  (if (new-good-enough? guess old-guess)
      guess
      (cube-root-iter (improve-cube guess x) guess x)))


#| Exercise 1.9 Each of the following two procedures defines a method for
adding two positive integers in terms of the procedures `inc`, which
increments its argument by 1, and `dec`, which decrements its argument by
1.

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

Using the substitution model, illustrate the process generated by each
procedure in evaluating `(+ 4 5)`. Are these processes iterative or
recursive?
|#

First procedure:
(+ 4 5)
(inc (+ (dec 4) 5))
(inc (inc (+ (dec 3) 5)))
(inc (inc (inc (+ (dec 2) 5))))
(inc (inc (inc (inc (+ (dec 1) 5)))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6))))
(inc (inc 7)))
(inc 8))
9

Second procedure:
(+ 4 5)
(+ (dec 4) (inc 5))
(+ 3 6)
(+ (dec 3) (inc 6))
(+ 2 7)
(+ (dec 2) (inc 7))
(+ 1 8)
(+ (dec 1) (inc 8))
(+ 0 9)
9

The first process is recursive. The second is iterative.


#| Exercise 1.10 The following procedure computes a mathematical function
called Ackermann's function.

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

What are the values of the following expressions?
|#

> (A 1 10)
(A 1 10)
(A (- 1 1) (A 1 (- 10 1)))
(A 0 (A 1 9))
(A 0 (A (- 1 1) (A 1 (- 9 1))))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A (- 1 1) (A 1 (A (- 8 1))))))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
(A 0 512)
1024

> (A 2 4)
(A 2 4)
(A (- 2 1) (A 2 (- 4 1)))
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))
(A 0 (A 0 (A 1 (A 1 14))))
...
(A 0 (A 0 16384))
(A 0 32768)
65536

> (A 3 3)
(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 (A 1 2))
(A 2 (A 0 (A 1 1)))
(A 2 (A 0 2))
(A 2 4)
...
65536

#| Consider the following procedures, where `A` is the procedure defined
above:

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

Give concise mathematical definitions for the functions computed by the
procedures `f`, `g`, and `h` for positive integer values of `n`. For
example, `(k n)` computes 5n^2.
|#

f(n) = 2n

g(n) = 0 for n = 0, 2^n for n > 0

h(n) = 0 for n = 0, 2^(2^n) for n > 0


#| Exercise 1.11 A function f is defined by the rule that f(n) = n if n < 3
and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3. Write a procedure
that computes f by means of a recursive process. Write a procedure that
computes f by means of an iterative process.
|#

(define (f_recur n)
  (cond ((< n 3) n)
	(else (+ (f_recur (- n 1))
		 (* 2 (f_recur (- n 2)))
		 (* 3 (f_recur (- n 3)))))))

;; Table of values to visualize how the process unfolds.
;; n  f(n)
;; 0  0
;; 1  1
;; 2  2
;; 3  f(2) + 2f(1) + 3f(0) = 2 + 2*1 + 3*0 = 4
;; 4  f(3) + 2f(2) + 3f(1) = 4 + 2*2 + 3*1 = 11
;; 5  f(4) + 2f(3) + 3f(2) = 11 + 2*4 + 3*2 = 25
;; 6  f(5) + 2f(4) + 3f(3) = 25 + 2*11 + 3*4 = 59

(define (f_iter n)
  (define (f_iter_helper a b c counter max_count)
    (if (> counter max_count)
	a
	(if (< counter 3)
	    (cond ((= counter 0) (f_iter_helper 0 0 0 (+ counter 1) max_count))
		  ((= counter 1) (f_iter_helper 1 0 0 (+ counter 1) max_count))
		  ((= counter 2) (f_iter_helper 2 1 0 (+ counter 1) max_count)))
	    (f_iter_helper (+ a (* 2 b) (* 3 c) a b) (+ counter 1) max_count))))
  (f_iter_helper 0 0 0 0 n))

;; A better iterative function, perhaps.

(define (f_iter n)
  (define (f_iter_helper a b c count)
    (cond ((< count 0) count)
	  ((= count 0) c)
	  (else (f_iter_helper (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (f_iter_helper 2 1 0 n))
