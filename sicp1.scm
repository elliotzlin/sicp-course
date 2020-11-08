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


#| Exercise 1.12 The following pattern of numbers is called Pascal's
triangle.

    1
   1 1
  1 2 1
 1 3 3 1
1 4 6 4 1
   ...

The numbers at the edge of the triangle are all 1, and each number inside
the triangle is the sum of the two numbers above it. Write a procedure that
computes elements of Pascal's triangle by means of a recursive process.
|#

;; Footnote on p.42:
;; The elements of Pascal's triangle are called binomial coefficients
;; because the nth row consists of the coefficients of the terms in the
;; expansion (x + y)^n.

;; NOTES
;; index  value
;; 1      1      edge
;; 2      1      edge
;; 3      1      edge
;; 4      1      edge
;; 5      2
;; 6      1      edge
;; 7      1      edge
;; 8      3
;; 9      3
;; 10     1      edge
;; 11     1      edge
;; 12     4
;; 13     6
;; 14     4
;; 15     1      edge
;;
;; Not much help here; have my function take 2 parameters, row and col.

;; Computes the element at row r and column c of Pascal's triangle, where
;; r and c are 1-indexed and triangle is left-aligned.
(define (pascal r c)
  (cond ((or (> c r) (< r 1) (< c 1)) -1)  ; Invalid inputs, return -1.
	((or (= c 1) (= c r)) 1)  ; 'Edge' case, haha.
	(else (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c)))))


#| Exercise 1.13 Prove that Fib(n) is the closest integer to phi^n /
sqrt(5), where phi = (1 + sqrt(5)) / 2. Hint: Let psi = (1 - sqrt(5)) /
2. Use induction and the definition of the Fibonacci numbers (see section
1.2.2) to prove that Fib(n) = (phi^n - psi^n) / sqrt(5).
|#

According to the hint, we will first prove Fib(n) = (phi^n - psi^n) /
sqrt(5), where we define:

phi = (1 + sqrt(5)) / 2
psi = (1 - sqrt(5)) / 2

Following a proof by induction, we begin with the base case n = 0, 1.

Fib(0) = (phi^0 - psi^0) / sqrt(5) = (1 - 1) / sqrt(5) = 0

Fib(1) = (phi^1 - psi^1) / sqrt(5) = ((1 + sqrt(5))/2 - (1 - sqrt(5))/2) / sqrt(5)
                                   = sqrt(5) / sqrt(5)
                                   = 1

For the inductive step, we will show that for any n >= 1, if Fib(n - 1) and
Fib(n) holds, then Fib(n + 1) also holds, using the definition of the
Fibonacci sequence.

Fib(n + 1) = Fib(n) + Fib(n - 1)
           = (phi^n - psi^n) / sqrt(5) + (phi^(n - 1) - psi^(n - 1)) / sqrt(5)
           = (phi^n + phi^(n - 1) - (psi^n + psi^(n - 1))) / sqrt(5)
           = (phi^(n - 1) * (phi + 1) - psi^(n - 1) * (psi + 1)) / sqrt(5)
           = (phi^(n - 1) * ((1 + sqrt(5)) / 2 + 1) - psi^(n - 1) * ((1 - sqrt(5)) / 2 + 1)) / sqrt(5)
           = (phi^(n - 1) * ((3 + sqrt(5)) / 2) - psi^(n - 1) * ((3 - sqrt(5)) / 2)) / sqrt(5)
           = (phi^n * 2 / (1 + sqrt(5)) * (3 + sqrt(5)) / 2 - psi^n * 2 / (1 - sqrt(5)) * (3 - sqrt(5)) / 2) / sqrt(5)
           = (phi^n * (3 + sqrt(5)) / (1 + sqrt(5)) - psi^n * (3 - sqrt(5)) / (1 - sqrt(5))) / sqrt(5)
           = (phi^n * (3 + sqrt(5)) * (1 - sqrt(5)) / ((1 + sqrt(5))(1 - sqrt(5))) - psi^n * (3 - sqrt(5)) * (1 + sqrt(5)) / ((1 - sqrt(5))(1 + sqrt(5)))) / sqrt(5)
           = (phi^n * (3 - 2 * sqrt(5) - 5) / (-4) - psi^n * (3 + 2 * sqrt(5) - 5) / (-4)) / sqrt(5)
           = (phi^n * (2 + 2 * sqrt(5)) / 4 - psi^n * (2 - 2 * sqrt(5)) / 4) / sqrt(5)
           = (phi^n * (1 + sqrt(5)) / 2 - psi^n * (1 - sqrt(5)) / 2) / sqrt(5)
           = (phi^n * phi - psi^n * psi) / sqrt(5)
           = (phi^(n + 1) - psi^(n + 1)) / sqrt(5)
           = Fib(n + 1)

Having proved Fib(n) = (phi^n - psi^n) / sqrt(5), we observe that:

phi^n / sqrt(5) - Fib(n) = psi^n / sqrt(5)
                         = ((1 - sqrt(5)) / 2)^n / sqrt(5)

abs((1 - sqrt(5)) / 2) is roughly abs((1 - 2.23) / 2) is between 0 and
1. Therefore the distance between Fib(n) and phi^n / sqrt(5) is always less
than 1, thereby proving Fib(n) is the closest integer to phi^n /
sqrt(5). QED


#| Exercise 1.14 Draw the tree illustrating the process generated by the
`count-change` procedure of section 1.2.2 in making change for 11
cents. What are the orders of growth of the space and number of steps used
by this process as the amount to be changed increases?
|#

I swear I did the drawing myself, but I'm pasting an ASCII graph from
http://community.schemewiki.org/?sicp-ex-1.14

(count-change 11)
|
(cc 11 5)__
|          \
(cc 11 4)   (cc -39 5)
|       \___
|           \
(cc 11 3)   (cc -14 4)
|       \_______________________________________________________
|                                                               \
(cc 11 2)                                                      (cc 1 3)
|       \_________________________                              |     \__
|                                 \                             |        \
(cc 11 1)                        (cc 6 2)                      (cc 1 2) (cc -9 3)
|       \___                      |     \__                     |     \__
|           \                     |        \                    |        \
(cc 11 0)   (cc 10 1)            (cc 6 1) (cc 1 2)             (cc 1 1) (cc -4 2)
         __/ |                 __/ |       |     \__            |     \__
        /    |                /    |       |        \           |        \
(cc 10 0)   (cc 9 1)  (cc 6 0)   (cc 5 1) (cc 1 1) (cc -4 2)   (cc 1 0) (cc 0 1)
         __/ |                 __/ |       |     \__
        /    |                /    |       |        \
(cc 9 0)    (cc 8 1)  (cc 5 0)   (cc 4 1) (cc 1 0) (cc 0 1)
         __/ |                 __/ |
        /    |                /    |
(cc 8 0)    (cc 7 1)  (cc 4 0)   (cc 3 1)
         __/ |                 __/ |
        /    |                /    |
(cc 7 0)    (cc 6 1)  (cc 3 0)   (cc 2 1)
         __/ |                 __/ |
        /    |                /    |
(cc 6 0)    (cc 5 1)  (cc 2 0)   (cc 1 1)
         __/ |                 __/ |
        /    |                /    |
(cc 5 0)    (cc 4 1)  (cc 1 0)   (cc 0 1)
         __/ |
        /    |
(cc 4 0)    (cc 3 1)
         __/ |
        /    |
(cc 3 0)    (cc 2 1)
         __/ |
        /    |
(cc 2 0)    (cc 1 1)
         __/ |
        /    |
(cc 1 0)    (cc 0 1)

The space complexity will be proportional to the maximum depth of the
tree. For `count-change` this is theta(n + k) where n is the amount of
money to make change for and k is the number of denominations.

The time complexity will be proportional to the number of nodes in the
tree. Assuming there are 5 denominations in total, the call (cc n 1) will
produce (cc n 0), which returns 0, and (cc (- n 1) 1), which returns 1
if (- n 1) is 0. This will yield roughly n layers. This will produce a
total of 2n + 1 nodes.

With this in mind, let us consider (cc n 2). This function call spawns
a (cc n 1) subtree, which produces 2n + 1 nodes including itself,
and (cc (- n 5) 2). This second call will produce roughly ceil(n / 5)
number of (cc _ 2) rooted subtrees, the last of which should return 0 or
1. Each of these ceil(n / 5) subtrees also produces a (cc _ 1) subtree,
resulting in 2n + 1 nodes. Therefore, (cc n 2) produces roughly (2n + 1)
+ (ceil(n / 5) - 1)(2n + 1) + 1 nodes. Ignoring the constants, we can see
the leading term is n^2.

Therefore, if we extrapolate this to any arbitrary number of denominations
k, we can model the time complexity of this process as theta(n^k), where n
is the amount of money to be changed.

#| Exercise 1.15 The sine of an angle (specified in radians) can be
computed by making use of the approximation sin(x) ~ x if x is sufficiently
small, and the trigonometric identity

sin(r) = 3 * sin(r / 3) - 4 * sin^3(r / 3)

to reduce the size of the argument of `sin`. (For purposes of this exercise
an angle is considered "sufficiently small" if its magnitude is not greater
than 0.1 radians.) These ideas are incorporated in the following
procedures:

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

a. How many times is the procedure `p` applied when `(sine 12.15)` is
evaluated?

b. What is the order of growth in space and number of steps (as a function
of a) used by the proces generated by the `sine` procedure when `(sine a)`
is evaluated?
|#

a. Here's the evoluation of the process:
(sine 12.15)
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine 0.45))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine 0.05))))))
(p (p (p (p (p 0.05)))))

`p` is applied 5 times.

b. The space complexity for `(sine a)` is the number of layers in the
execution tree. At each step we divide `a` by 3 until. We do this n
times. So the inequality looks like:

a / 3^n = 0.1
10a = 3^n
n = log_{3}(10a)

Ignoring constants, the space complexity is theta(log(a)).

The time complexity (number of steps) is follows the same order of growth
as the space complexity.


#| Exercise 1.16 Design a procedure that evolves an iterative
exponentiation process that uses successive squaring and uses a logarithmic
number of steps, as does `fast-expt`. (Hint: Using the observation
that (b^(n/2))^2 = (b^2)^(n/2), keep, along with the exponent `n` and the
base `b`, an additional state variable `a`, and define the state
transformation in such a way that the product `ab^n` is unchanged from
state to state. At the beginning of the process `a` is taken to be 1, and
the answer is given by the value of `a` at the end of the process. In
general, the technique of defining an `invariant quantity` that remains
unchanged from state to state is a powerful way to think about the design
of iterative algorithms.)
|#

As per the hint, we will define our state variables and transformation with
an additional `a` variable.

if n is even:
b <- b^2
n <- n/2 - 1
a <- a * b^2

This represents b^n = (b^2)^(n / 2) = b^2 * (b^2)^((n / 2) - 1)

if n is odd:
b <- b
n <- n - 1
a <- a * b

This represents b^n = b * b^(n - 1)

(define (expt b n)
  (define (fast-expt-iter b n a)
    (cond ((= n 0) a)
	  ((even? n)
	   (fast-expt-iter (square b)
			   (- (/ n 2) 1)
			   (* a (square b))))
	  (else (fast-expt-iter b
				(- n 1)
				(* a b)))))
  (fast-expt-iter b n 1))

;; UPDATE: This is probably cleaner looking. I don't need to subtract 1,
;; but I'll keep my other solution as a record.
(define (expt b n)
  (define (fast-expt-iter b n a)
    (cond ((= n 0) a)
	  ((even? n)
	   (fast-expt-iter (square b)
			   (/ n 2)
			   a))
	  (else (fast-expt-iter b
				(- n 1)
				(* a b)))))
  (fast-expt-iter b n 1))


#| Exercise 1.17 The exponentiation algorithms in this section are based on
performing exponentiation by means of repeated multiplication. In a similar
way, one can perform integer multiplication by means of repeated
addition. The following multiplication procedure (in which it is assumed
that our language can only add, not multiply) is analogous to the expt
procedure:

(define (* a b)
  (if (= b 0)
      0
      (+ (* a (- b 1)))))

This algorithm takes a number of steps that is linear in b. Now suppose we
include, together with addition, operations `double`, which doubles an
integer, and `halve`, which divides an (even) integer by 2. Using these,
design a multiplication procedure analogous to fast-expt that uses a
logarithmic number of steps.
|#

(define (fast-mult a b)
  (cond ((= b 0) 0)
	((even? b) (double (fast-mult a (halve b))))
	(else (+ a (fast-mult a (- b 1))))))

;; Evolution of the recursive process
;; (fast-mult 5 9)
;; (+ 5 (fast-mult 5 8))
;; (+ 5 (double (fast-mult 5 4)))
;; (+ 5 (double (double (fast-mult 5 2))))
;; (+ 5 (double (double (double (fast-mult 5 1)))))
;; (+ 5 (double (double (double (+ 5 (fast-mult 5 0))))))
;; (+ 5 (double (double (double (+ 5 0)))))
;; (+ 5 (double (double (double 5))))
;; (+ 5 (double (double 10)))
;; (+ 5 (double 20))
;; (+ 5 40)
;; 45


#| Exercise 1.18 Using the results of exercises 1.16 and 1.17, devise a
procedure that generates an iterative process for multiplying two integers
in terms of adding, doubling, and halving and uses a logarithmic number of
steps.
|#

(define (fast-mult a b)
  (define (fast-mult-iter a b x)
    (cond ((= b 0) x)
	  ((even? b) (fast-mult-iter (double a) (- (halve b) 1) (+ x (double a))))
	  (else (fast-mult-iter a (- b 1) (+ x a)))))
  (fast-mult-iter a b 0))

;; Evolution of the iterative process
;; (fast-mult 5 9)
;; (fast-mult-iter 5 9 0)
;; (fast-mult-iter 5 8 5)
;; (fast-mult-iter 10 3 15)
;; (fast-mult-iter 10 2 25)
;; (fast-mult-iter 20 0 45)
;; 45

;; UPDATE: I don't think I needed to subtract 1, but as with the
;; fast-expt-iter function, I'll leave my work above for the record.
(define (fast-mult-iter a b x)
  (cond ((= b 0) x)
	((even? b) (fast-mult-iter (double a) (halve b) x))
	(else (fast-mult-iter a (- b 1) (+ x a)))))


#| Exercise 1.19 There is a clever algorithm for computing the Fibonacci
numbers in a logarithmic number of steps. Recall the transformation of the
state variables `a` and `b` in `fib-iter` process of section 1.2.2: `a <- a
+ b` and `b <- a`. Call this transformation `T`, and observe that applying
`T` over and over again `n` times, starting with 1 and 0, produces the pair
`Fib(n + 1)` and `Fib(n)`. In other words, the Fibonacci numbers are
produced by applying `T^n`, the nth power of the transformation `T`,
starting with the pair (1,0). Now consider `T` to be the special case of `p
= 0` and `q = 1` in a family of transformations `T_{pq}`, where `T_{pq}`
transforms the pair `(a,b)` according to `a <- bq + aq + ap` and `b <- bp +
aq`. Show that if we apply such a transformation `T_{pq}` twice, the effect
is the same as using a single transformation `T_{p'q'}` of the same form,
and compute `p'` and `q'` in terms of `p` and `q`. This gives us an
explicit way to square these transformations, and thus we can compute `T^n`
using successive squaring, as in the `fast-expt` procedure. Put this all
together to complete the following procedure, which runs in a logarithmic
number of steps:

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   <??>      ; compute p'
		   <??>      ; compute q'
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))
|#

;; Compute p'
(+ (square p) (square q))

;; Compute q'
(+ (* 2 p q) (square q))

;; Explanation: Use the transformation for b to compute p' and q'.
;; Given:
;;   a' = bq + aq + ap
;;   b' = bp + aq
;;
;; b'' = b'p + a'q
;;     = (bp + aq)p + (bq + aq + ap)q
;;     = bp^2 + apq + bq^2 + aq^2 + apq
;;     = bp^2 + bq^2 + 2apq + aq^2          ; Group terms
;;     = b(p^2 + q^2) + a(2pq + q^2)
;;     = bp' + aq'
;;
;; Therefore:
;;   p' = p^2 + q^2
;;   q' = 2pq + q^2

(define (fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
	  ((even? count)
	   (fib-iter a
		     b
		     (+ (square p) (square q))
		     (+ (* 2 p q) (square q))
		     (/ count 2)))
	  (else (fib-iter (+ (* b q) (* a q) (* a p))
			  (+ (* b p) (* a q))
			  p
			  q
			  (- count 1)))))
  (fib-iter 1 0 0 1 n))


#| Exercise 1.20 The process that a procedure generates is of course
dependent on the rules used by the interpreter. As an example, consider the
iterative gcd procedure given above. Suppose we were to interpret this
procedure using normal-order evaluation, as discussed in section
1.1.5. (The normal-order-evaluation rule for `if` is described in exercise
1.5.) Using the substitution method (for normal order), illustrate the
process generated in evaluating `(gcd 206 40)` and indicate the `remainder`
operations that are actually performed. How many `remainder` operations are
actually performed in the normal-order evaluation of `(gcd 206 40)`? In the
applicative-order evaluation?
|#

;; Using normal order evaluation
;; (gcd 206 40)
;; (if (= 40 0)
;;     206
;;     (gcd 40 (remainder 206 40)))
;; (if (= 40 0)
;;     206
;;     (if (= (remainder 206 40) 0)
;; 	   40
;; 	   (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))))
;; (if (= 40 0)
;;     206
;;     (if (= (remainder 206 40) 0)
;; 	   40
;; 	   (if (= (remainder 40 (remainder 206 40)) 0)
;; 	       (remainder 206 40)
;; 	       (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
;; (if (= 40 0)
;;     206
;;     (if (= (remainder 206 40) 0)
;; 	   40
;; 	   (if (= (remainder 40 (remainder 206 40)) 0)
;; 	       (remainder 206 40)
;; 	       (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
;; 	           (remainder 40 (remainder 206 40))
;; 		   (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))))
;; 2
;; There are 18 `remainder` operations performed.
;; N.B. We don't count the `remainder` operations in the part of the `if`
;; statement that isn't executed.


;; Using applicative order evaluation
;; (gcd 206 40)
;; (gcd 40 (remainder 206 40))
;; (gcd 40 6)
;; (gcd 6 (remainder 40 6))
;; (gcd 6 4)
;; (gcd 4 (remainder 6 4))
;; (gcd 4 2)
;; (gcd 2 (remainder 4 2))
;; (gcd 2 0)
;; 2
;;
;; There are 4 `remainder` operations performed.


#| Exercise 1.21 Use the `smallest-divisor` procedure to find the smallest
divisor of each of the following numbers: 199, 1999, 19999.
|#

;; 199
;; (smallest-divisor 199)
;; (find-divisor 199 2)
;; (find-divisor 199 3)
;; (find-divisor 199 4)
;; (find-divisor 199 5)
;; (find-divisor 199 6)
;; (find-divisor 199 7)
;; ...
;; (find-divisor 199 15)
;; 199

;; 1999
;; (smallest-divisor 1999)
;; (find-divisor 1999 2)
;; (find-divisor 1999 3)
;; (find-divisor 1999 4)
;; ...
;; (find-divisor 1999 45)
;; 1999

;; 19999
;; (smallest-divisor 19999)
;; (find-divisor 19999 2)
;; (find-divisor 19999 3)
;; (find-divisor 19999 4)
;; (find-divisor 19999 5)
;; (find-divisor 19999 6)
;; (find-divisor 19999 7)
;; 7


#| Exercise 1.22 Most Lisp implementations include a primitive called
`runtime` that returns an integer that specifies the amount of time the
system has been running (measured, for example, in microseconds). The
following `timed-prime-test` procedure, when called with an integer `n`,
prints `n` and checks to see if `n` is prime. If `n` is prime, the
procedure prints three asterisks followed by the amount of time used in
performing the test.

```
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
```

Using this procedure, write a procedure `search-for-primes` that checks the
primality of consecutive odd integers in a specified range. Use your
procedure to find the three smallest primes larger than 1000; larger than
10,000; larger than 100,000; larger than 1,000,000. Note the time needed to
test each prime. Since the testing algorithm has order of growth of
theta(sqrt(n)), you should expect that testing for primes around 10,000
should take a bout sqrt(10) times as long as testing for primes around
1000. Do your timing data bear this out? How well do the data for 100,000
and 1,000,000 support the sqrt(n) prediction? Is your result compatible
with the notion that programs on your machine run in time proportional to
the number of steps required for the computation?
|#

(define (search-for-primes lower upper)
  (define (search-prime-helper n)
    (cond ((< n upper) (timed-prime-test n) (search-prime-helper (+ n 2)))))
  (search-prime-helper (if (even? lower) (+ lower 1) lower)))

;; I didn't actually notice any difference in searching for prime numbers
;; larger than 1000, 10000, and 1000000. I think my machine (a 2013 laptop)
;; is too fast for me to actually notice any difference.

;; I did see results with values above 1e9.
(search-for-primes 1000000000 1000000023)     ; In the range 0.05s
(search-for-primes 10000000000 10000000063)   ; In the range 0.15-0.16s
(search-for-primes 100000000000 100000000059) ; In the range 0.47-0.48s

;; The timing data certainly bears out that increasing the size of the numbers
;; checked increases the runtime by sqrt(10), or about 3 seconds.


#| Exercise 1.23 The `smallest-divisor` procedure shown at the start of
this section does lots of needless testing: After it checks to see if the
number is divisible by 2 there is no point in checking to see if it is
divisible by any larger even numbers. This suggests that the values used
for `test-divisor` should not be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7,
9, .... To implement this change, define a procedure `next` that returns 3
if its input is equal to 2 and otherwise returns its input plus 2. Modify
the `smallest-divisor` procedure to use `(next test-divisor)` instead
of `(+ test-divisor 1)`. With `timed-prime-test` incorporating this
modified version of `smallest-divisor`, run the test for each of the 12
primes found in exercise 1.22. Since this modification halves the number of
test steps, you should expect it to run about twice as fast. Is this
expectation confirmed?  If not, what is the observed ratio of the speeds of
the two algorithms, and how do you explain the fact that it is different
from 2?
|#

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor ) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

;; Running the `search-for-primes` routines from 1.22.
(search-for-primes 1000000000 1000000023)     ; In the range 0.03-0.04s
(search-for-primes 10000000000 10000000063)   ; In the range 0.09-0.1s
(search-for-primes 100000000000 100000000059) ; In the range 0.3-0.32s

;; This algorithm doesn't run twice as fast, but it is faster. The ratio is
;; instead 3:5 for the faster one to the regular one. We did reduce the number
;; of divisors tested by half, but we also added an additional `if` call.


#| Exercise 1.24 Modify the `timed-prime-test` procedure of exercise 1.22
to use `fast-prime?` (the Fermat method), and test each of the 12 primes
you found in that exercise. Since the Fermat test has theta(log n) growth,
how would you expect the time to test primes near 1,000,000 to compare with
the time needed to test primes near 1000? Do your data bear this out? Can
you explain any discrepancy you find?
|#

;; The time needed to test each prime depends on the time performance of
;; the Fermat test, which has theta(log n) growth. Therefore, increasing n
;; by 10 times would yield the new runtime of log(10n), which is log(10) +
;; log(n), which indicates constant time. This can be explained by the fact
;; that we bound the number of steps the Fermat test takes by limiting the
;; number of attempts.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 1000000007)   ; 1.0000000000000009e-2
(timed-prime-test 1000000009)   ; 9.999999999999995e-3
(timed-prime-test 1000000021)   ; 2.0000000000000018e-2
(timed-prime-test 10000000019)  ; 9.999999999999981e-3
(timed-prime-test 10000000033)  ; 1.0000000000000009e-2
(timed-prime-test 10000000061)  ; 9.999999999999981e-3
(timed-prime-test 100000000003) ; 1.0000000000000009e-2
(timed-prime-test 100000000019) ; 1.0000000000000009e-2
(timed-prime-test 100000000057) ; 1.0000000000000009e-2
(timed-prime-test 1000000000039); 1.0000000000000009e-2
(timed-prime-test 1000000000061); 0.01999999999999999
(timed-prime-test 1000000000063); 2.0000000000000018e-2

;; The data bear this out.


#| Exercise 1.25 Alyssa P. Hacker complains that we went to a lot of extra
work in writing `expmod`. After all, she says, since we already know how to
compute exponentials, we could have simply written:

```
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))
```

Is she correct? Would this procedure serve as well for our fast prime
tester? Explain.
|#

;; Let us observe the execution of (expmod 2 5 5) using the textbook
;; method.
(expmod 2 5 5)
(remainder (* 2 (expmod 2 4 5)) 5)
(remainder (* 2 (remainder (square (expmod 2 2 5)) 5)) 5)
(remainder (* 2 (remainder (square (remainder (square (expmod 2 1 5)) 5)) 5)) 5)
(remainder (* 2 (remainder (square (remainder (square (remainder (* 2 (expmod 2 0 5)) 5)) 5)) 5)) 5)
(remainder (* 2 (remainder (square (remainder (square (remainder (* 2 1) 5)) 5)) 5)) 5)
(remainder (* 2 (remainder (square (remainder (square (remainder 2 5)) 5)) 5)) 5)
(remainder (* 2 (remainder (square (remainder (square 2) 5)) 5)) 5)
(remainder (* 2 (remainder (square (remainder 4 5)) 5)) 5)
(remainder (* 2 (remainder (square 4) 5)) 5)
(remainder (* 2 (remainder 16 5)) 5)
(remainder (* 2 1) 5)
(remainder 2 5)
2

;; Now let us observe the execution of this call using Alyssa P. Hacker's
;; way.
(expmod 2 5 5)
(remainder (fast-expt 2 5) 5)
(remainder (* 2 (fast-expt 2 4)) 5)
(remainder (* 2 (square (fast-expt 2 2))) 5)
(remainder (* 2 (square (square (fast-expt 2 1)))) 5)
(remainder (* 2 (square (square (* 2 (fast-expt 2 0))))) 5)
(remainder (* 2 (square (square (* 2 1)))) 5)
(remainder (* 2 (square (square 2))) 5)
(remainder (* 2 (square 4)) 5)
(remainder (* 2 16) 5)
(remainder 32 5)
2

;; One key difference is with the textbook implementation we never have to
;; compute `remainder` on values much larger than `m`. We don't have to
;; square successively bigger numbers. For small values of `m` this
;; probably doesn't make a huge difference, but once we start searching for
;; large prime numbers we could see a performance hit from having to divide
;; large numbers.


#| Exercise 1.26 Louis Reasoner is having great difficulty doing exercise
1.24. His `fast-prime?` test seems to run more slowly than his `prime?`
test. Louis calls his friend Eva Lu Ator over to help. When they examine
Louis's code, they find that he has rewritten the `expmod` procedure to use
an explicit multiplication, rather than calling `square`:

```
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
```

"I don't see what difference that could make," says Louis. "I do." says
Eva. "By writing the procedure like that, you have transformed the
theta(log n) process into a theta(n) process." Explain.
|#

;; Given that the Scheme interpreter uses applicative-order evaluation, which
;; evaluates the parameters to a function as needed, the interpreter will be
;; evaluating `(expmod base (/ exp 2) m)` twice every time `exp` is
;; even. Though we cut the exponent in half every time we hit an even
;; exponent, we double the number of nodes in our evaluation tree, effectively
;; negating that optimization.
;;
;; Another way to put it is the rewritten `expmod` turns a linear recursion
;; into a tree recursion.


#| Exercise 1.27 Demonstrate that the Carmichael numbers listed in footnote
47 really do fool the Fermat test. That is, write a procedure that takes an
integer `n` and tests whether `a^n` is congruent to `a` modulo n for every
`a < n`, and try your procedure on the given Carmichael numbers.
|#

;; Code for this exercise:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test-all n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (try-it-all iter)
    (cond ((= iter n) #t)
	  ((try-it iter) (try-it-all (+ iter 1)))
	  (else #f)))
  (try-it-all 1))

(fermat-test-all 561)  ; #t
(fermat-test-all 1105) ; #t
(fermat-test-all 1729) ; #t
(fermat-test-all 2465) ; #t
(fermat-test-all 6601) ; #t


#| Exercise 1.28 One variant of the Fermat test that cannot be fooled is
called the Miller-Rabin test (Miller 1976; Rabin 1980). This starts from an
alternate form of Fermat's Little Theorem, which states that if n is a
prime number and a is any positive integer less than n, then a raised to
the (n - 1)st power is congruent to 1 modulo n. To test the primality of a
number n by the Miller-Rabin test, we pick a random number a < n and raise
a to the (n - 1)st power modulo n using the `expmod` procedure. However,
whenever we perform the squaring step in `expmod`, we check to see if we
have discovered a "nontrivial square root of 1 modulo n," that is, a number
not equal to 1 or n - 1 whose square is equal to 1 modulo n. It is possible
to prove that if such a nontrivial square root of 1 exists, then n is not
prime. It is also possible to prove that if n is an odd number that is not
prime, then, for at least half the numbers a < n, computing a^(n - 1) in
this way will reveal a nontrivial square root of 1 modulo n. (This is why
the Miller-Rabin test cannot be fooled.) Modify the `expmod` procedure to
signal if it discovers a nontrivial square root of 1, and use this to
implement the Miller-Rabin test with a procedure analogous to
`fermat-test`. Check your procedure by testing various known primes and
non-primes. Hint: One convenient way to make `expmod` signal is to have it
return 0.
|#

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
	 (if (and (> base 1)
		  (< base (- m 1))
		  (= (remainder (square base) m) 1))
	     0  ; Discovered a nontrivial square root of 1 modulo m
	     (remainder (square (expmod base (/ exp 2) m))
			m)))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

;; Test on some known primes.
(fast-prime? 7 100)          ; #t
(fast-prime? 17 100)         ; #t
(fast-prime? 1000000007 100) ; #t

;; Test on the smallest few Carmichael numbers.

(fast-prime? 561 100)  ; #f
(fast-prime? 1105 100) ; #f
(fast-prime? 1729 100) ; #f
(fast-prime? 2465 100) ; #f
(fast-prime? 6601 100) ; #f


#| Exercise 1.29 Simpson's Rule is a more accurate method of numerical
integration than the method illustrated above. Using Simpson's Rule, the
integral of a function f between a and b is approximated as:

h / 3 [y_0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... + 2y_{n-2} + 4y_{n-1} + y_n]

where `h = (b - a)/n`, for some even integer `n`, and `y_k = f(a +
kh)`. (Increasing `n` increases the accuracy of the approximation.) Define
a procedure that takes as arguments `f`, `a`, `b`, and `n` and returns the
value of the integral, computed using Simpson's Rule. Use your procedure to
integrate `cube` between 0 and 1 (with n = 100 and n = 1000), and compare
the results to those of the `integral` procedure shown above.
|#

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (simpsons-integral f a b n)
  (define h (/ (- b a) n))
  (define (simpsons-term k)
    (define y (f (+ a (* k h))))
    (cond ((or (= k 0) (= k n)) y)
	  ((even? k) (* 2 y))
	  (else (* 4 y))))
  (* (/ h 3) (sum simpsons-term 0 inc n)))

(simpsons-integral cube 0 1 100)  ; 1/4
(simpsons-integral cube 0 1 1000) ; 1/4


#| Exercise 1.30 The sum procedure above generates a linear recursion. The
procedure can be rewritten so that the sum is performed iteratively. Show
how to do this by filling in the missing expressions in the following
definition:

```
(define (sum term a next b)
  (define (iter a result)
    (if <??>
        <??>
        (iter <??> <??>)))
  (iter <??> <??>))
```
|#

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

(sum cube 0 inc 10)  ; 3025


#| Exercise 1.31
a. The sum procedure is only the simplest of a vast number of similar
abstractions that can be captured as higher-order procedures. Write an
analogous procedure called `product` that returns the product of the values
of a function at points over a given range. Show how to define `factorial`
in terms of `product`. Also use product to compute approximations to `pi`
using the formula:

pi = 2 * 4 * 4 * 6 * 6 * 8 ...
--   -------------------------
4    3 * 3 * 5 * 5 * 7 * 7 ...

b. If your `product` procedure generates a recursive process, write one
that generates an iterative process. If it generates an iterative process,
write one that generates a recursive process.
|#

;; Part a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (factorial n)
  (define (inc x) (+ x 1))
  (define (identity x) x)
  (product identity 1 inc n))

(define (pi-product a b)
  (define (pi-term x)
    (define (pi-num n)
      (+ 2 (* 2 (floor (/ n 2)))))
    (define (pi-denom n)
      (+ 3 (* 2(floor (/ (- n 1) 2)))))
    (/ (* 1.0 (pi-num x)) (pi-denom x)))
  (define (inc x) (+ x 1))
  (product pi-term a inc b))

(* 4 (pi-product 1 4))   ; 3.4133333333333327
(* 4 (pi-product 1 10))  ; 3.275101041334807
(* 4 (pi-product 1 100)) ; 3.1570301764551645

;; Part b
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;; A much better `pi-term` read online. :facepalm:
(define (pi-term n)
  (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))


#| Exercise 1.32
a. Show that `sum` and `product` (exercise 1.31) are both special cases of
a still more general notion called `accumulate` that combines a collection
of terms, using some general accumulation function:

```
(accumulate combiner null-value term a next b)
```

`accumulate` takes as arguments the same term and range specifications as
`sum` and `product`, together with a `combiner` procedure (of two
arguments) that specifies how the current term is to be combined with the
accumulation of the preceding terms and a `null-value` that specifies what
base value to use when the terms run out. Write `accumulate` and show how
`sum` and `product` can both be defined as simple calls to `accumulate`.

b. If your `accumulate` procedure generates a recursive process, write one
that generates an iterative process. If it generates an iterative process,
write one that generates a recursive process.
|#

;; a. Recursive process
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

;; b. Iterative process
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

;; Define `sum` and `product` with `accumulate`
(define (sum term a next b) (accumulate + 0 term a next b))

(define (product term a next b) (accumulate * 1 term a next b))


#| Exercise 1.33 You can obtain an even more general version of
`accumulate` (exercise 1.32) by introducing the notion of a filter on the
terms to be combined. That is, combine only those terms derived from values
in the range that satisfy a specified condition. The resulting
`filtered-accumulate` abstraction takes the same arguments as accumulate,
together with an additional predicate of one argument that specifies the
filter. Write `filtered-accumulate` as a procedure. Show how to express the
following using `filtered-accumulate`:

a. the sum of the squares of the prime numbers in the interval `a` to
`b` (assuming that you have a `prime?` predicate already written)

b. the product of all the positive integers less than `n` that are
relatively prime to `n` (i.e., all positive integers `i < n` such that
`GCD(i,n) = 1`).
|#

;; Recursive version
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a)
		    (term a)
		    null-value)
		(filtered-accumulate filter combiner null-value term (next a) next b))))

;; Iterative version
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (if (filter a)
				     (term a)
				     null-value)
				 result))))
  (iter a null-value))

;; a. Sum of square primes

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (= n 1)
      #f
      (= n (smallest-divisor n))))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))

;; b. Product of positive integers less than `n` relatively prime to `n`.

(define (relative-prime? i n)
  (= (gcd i n) 1))

(define (product-relative-prime n)
  (accumulate relative-prime? * 1 identity 1 inc n))


#| Exercise 1.34 Suppose we define the procedure:
```
(define (f g)
  (g 2))
```
Then we have:
```
(f square)
4

(f (lambda (z) (* z (+ z 1))))
6
```
What happens if we (pervsersely) ask the interpreter to evaluate the
combination of `(f f)`? Explain.
|#

;; If we call `(f f)`, it will call `(f 2)`, which calls `(2 2)`, which will
;; error because `2` is not a callable function.
;;
;; Exact error: `The object 2 is not applicable.`


#| Exercise 1.35 Show that the golden ratio `phi` (section 1.2.2) is a
fixed point of the transformation `x |-> 1 + 1/x`, and use this fact to
compute by means of the `fixed-point` procedure.
|#

;; To show that the golden ratio is a fixed point of the transformation, we
;; plug it in to the function.
;;
;; f((1 + sqrt(5)) / 2) = 1 + 1 / ((1 + sqrt(5)) / 2)
;;                      = 1 + 2 / (1 + sqrt(5))
;;                      = (1 + sqrt(5)) / (1 + sqrt(5)) + 2 / (1 + sqrt(5))
;;                      = (3 + sqrt(5)) / (1 + sqrt(5))
;;                      = (3 + sqrt(5)) * (1 - sqrt(5)) / (1 + sqrt(5)) * (1 - sqrt(5))
;;                      = (3 - 2 * sqrt(5) - 5) / (1 - 5)
;;                      = (-2 - 2 * sqrt(5)) / (-4)
;;                      = (1 + sqrt(5)) / 2
;; Therefore f(phi) = phi. QED.




#| Exercise 1.36 Modify `fixed-point` so that it prints the sequence of
approximations it generates, using the newline and display primitives shown
in exercise 1.22. Then find a solution to `x^x = 1000` by finding a fixed
point of `x |-> log(1000)/log(x)`. (Use Scheme's primitive `log` procedure,
which computes natural logarithms.) Compare the number of steps this takes
with and without average damping. (Note that you cannot start `fixed-point`
with a guess of 1, as this would cause division by `log(1) = 0`.)\
|#

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display " *** ")
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Without average damping.
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
;; *** 2
;; *** 9.965784284662087
;; *** 3.004472209841214
;; *** 6.279195757507157
;; *** 3.759850702401539
;; *** 5.215843784925895
;; *** 4.182207192401397
;; *** 4.8277650983445906
;; *** 4.387593384662677
;; *** 4.671250085763899
;; *** 4.481403616895052
;; *** 4.6053657460929
;; *** 4.5230849678718865
;; *** 4.577114682047341
;; *** 4.541382480151454
;; *** 4.564903245230833
;; *** 4.549372679303342
;; *** 4.559606491913287
;; *** 4.552853875788271
;; *** 4.557305529748263
;; *** 4.554369064436181
;; *** 4.556305311532999
;; *** 4.555028263573554
;; *** 4.555870396702851
;; *** 4.555315001192079
;; *** 4.5556812635433275
;; *** 4.555439715736846
;; *** 4.555599009998291
;; *** 4.555493957531389
;; *** 4.555563237292884
;; *** 4.555517548417651
;; *** 4.555547679306398
;; *** 4.555527808516254
;; *** 4.555540912917957
;;;Value: 4.555532270803653
;; This took 35 steps.

;; With average damping.
(define (average x y)
  (/ (+ x y) 2))
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2)
;; *** 2
;; *** 5.9828921423310435
;; *** 4.922168721308343
;; *** 4.628224318195455
;; *** 4.568346513136242
;; *** 4.5577305909237005
;; *** 4.555909809045131
;; *** 4.555599411610624
;; *** 4.5555465521473675
;;;Value: 4.555537551999825
;; This took 10 steps.


#| Exercise 1.37
a. An infinite `continued fraction` is an expression of the form
```
f =         N_1
    _____________________
    D_1 +       N_2
          _______________
          D_2 +    N_3
                _________
                D_3 + ...
```
As an example, one can show that the infinite continued fraction expansion
with the `N_i` and the `D_i` all equal to 1 produces `1/phi`, where `phi`
is the golden ratio (described in section 1.2.2). One way to approximate an
infinite continued fraction is to truncate the expansion after a given
number of terms. Such a truncation -- a so-called `k-term finite continued
fraction` -- has the form
```
        N_1
__________________
D_1 +     N_2
     _____________
     .
      .
       . + N_K
           ___
           D_K
```
Suppose that `n` and `d` are procedures of one argument (the term index
`i`) that return the `N_i` and `D_i` of the terms of the continued
fraction. Define a procedure `cont-frac` such that evaluating `(cont-frac n
d k)` computes the value of the `k`-term finite continued fraction. Check
your procedure by approximating `1/phi` using
```
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           k)
```
for successive values of `k`. How large must you make `k` in order to get
an approximation that is accurate to 4 decimal places?

b. If your `cont-frac` procedure generates a recursive process, write one
that generates an iterative process. If it generates an iterative process,
write one that generates a recursive process.
|#

;; Note: 1 / phi = 2 / (1 + sqrt(5)) = 0.6180
;; a. Recursive process
(define (cont-frac n d k)
  (define (cont-frac-helper i)
    (if (= i k)
	(/ (n i) (d i))
	(/ (n i) (+ (d i) (cont-frac-helper (+ i 1))))))
  (cont-frac-helper 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)  ; .6180555555555556

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)  ; .6180257510729613
;; You need k - 12 to get the required precision.

;; b. Iterative process
(define (cont-frac n d k)
  (define (cont-frac-iter i result)
    (if (= i 0)
	result
	(cont-frac-iter (- i 1)
			(/ (n i)
			   (+ (d i) result)))))
  (cont-frac-iter k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)  ; .6180555555555556

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)  ; .6180257510729613


#| Exercise 1.38 In 1737, the Swiss mathematician Leonhard Euler published
a memoir `De Fractionibus Continuis`, which included a continued fraction
expansion for `e - 2`, where `e` is the base of the natural logarithms. In
this fraction, the `N_i` are all 1, and the `D_i` are successively 1, 2, 1,
1, 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your `cont-frac`
procedure from exercise 1.37 to approximate `e`, based on Euler's
expansion.
|#

(define (compute-e k)
  (define (n i) 1)
  (define (d i)
    (let ((q (remainder i 3)))
      (if (or (= q 0) (= q 1))
	  1
	  (* 2 (ceiling (/ i 3.0))))))
  (cont-frac n d k))

(+ 2 (compute-e 50))
; 2.7182818284590455


#| Exercise 1.39 A continued fraction representation of the tangent
function was published in 1770 by the German mathematician J.H. Lambert:
```
tan(r) =        r
         _______________
         1 -     r^2
             ___________
             3 -   r^2
                 _______
                 5 - ...
```
where `x` is in radians. Define a procedure `(tan-cf x k)` that computes an
approximation to the tangent function based on Lambert's formula. `K`
specifies the number of terms to compute, as in exercise 1.37.
|#

(define (tan-cf x k)
  (define (tan-cf-iter i result)
    (if (= i 0)
	result
	(tan-cf-iter (- i 1)
		     (/ (* x x)
			(* (- (- (* 2 i) 1) result) 1.0)))))
  (/ (tan-cf-iter k 0) x))


#| Exercise 1.40 Define a procedure `cubic` that can be used together with
the `newtons-method` procedure in expressions of the form
```
(newtons-method (cubic a b c) 1)
```
to approximate zeros of the cubic `x^3 + ax^2 + bx + c`.
|#

(define (cubic a b c)
  (lambda (x) (+ (* x x x)
		 (* a x x)
		 (* b x)
		 c)))


#| Exercise 1.41 Define a procedure `double` that takes a procedure of one
argument as argument and returns a procedure that applies the original
procedure twice. For example, if `inc` is a procedure that adds 1 to its
argument, then `(double inc)` should be a procedure that adds 2. What value
is returned by

```
(((double (double double)) inc) 5)
```
|#

(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5)
; 21


#| Exercise 1.42 Let `f` and `g` be two one-argument functions. The
*composition* `f after `g` is defined to be the function `x |->
f(g(x))`. Define a procedure `compose` that implements composition. For
example, if `inc` is a procedure that adds 1 to its argument,
```
((compose square inc) 6)
49
```
|#

(define (compose f g)
  (lambda (x) (f (g x))))


#| Exercise 1.43 If `f` is a numerical function and `n` is a positive
integer, then we can form the `nth` repeated application of `f`, which is
defined to be the function whose value at `x` is `f(f(...(f(x))...))`. For
example, if `f` is the function `x |-> x + 1`, then the `n`th repeated
application of `f` is the function `x |-> x + n`. If `f` is the operation
of squaring a number, then the `n`th repeated application of `f` is the
function that raises its argument to the `2^n`th power. Write a procedure
that takes as inputs a procedure that computes `f` and a positive integer
`n` and returns the procedure that computes the `n`th repeated application
of `f`. Your procedure should be able to be used as follows:
```
((repeated square 2) 5)
625
```
Hint: You may find it convenient to use `compose` from exercise 1.42.
|#

(define (repeated f n)
  (if (< n 2)
      f
      (compose f (repeated f (- n 1)))))

;; Not my solution, but one I saw online with O(log n) complexity.
(define (identity x) x)
(define (repeated f n)
  (cond ((= n 0) identity)
	((even? n) (repeated (compose f f) (/ n 2)))
	(else (compose f (repeated f (- n 1))))))


#| Exercise 1.44 The idea of smoothing a function is an important concept
in signal processing. If `f` is a function and `dx` is some small number,
then the smoothed version of `f` is the function whose value at a point `x`
is the average of `f(x - dx)`, `f(x)`, and `f(x + dx)`. Write a procedure
smooth that takes as input a procedure that computes `f` and returns a
procedure that computes the smoothed `f`. It is sometimes valuable to
repeatedly smooth a function (that is, smooth the smoothed function, and so
on) to obtained the `n-fold smoothed function`. Show how to generate the
`n`-fold smoothed function of any given function using `smooth` and
`repeated` from exercise 1.43.
|#

ANSWER


#| Exercise 1.45 We saw in section 1.3.3 that attempting to compute square
roots by naively finding a fixed point of `y |-> x/y` does not converge,
and that this can be fixed by average damping. The same method works for
finding cube roots as fixed points of the average-damped `y |->
x/y^2`. Unfortunately, the process does not work for fourth roots -- a
single average damp is not enough to make a fixed-point search for `y |->
x/y^3` converge. On the other hand, if we average damp twice (i.e., use the
average damp of the average damp of `y |-> x/y^3`) the fixed-point search
does converge. Do some experiments to determine how many average damps are
required to compute `n`th roots as a fixed-point search based upon repeated
average damping of `y |-> x/y^{n-1}. Use this to implement a simple
procedure for computing `n`th roots using `fixed-point`, `average-damp`,
and the `repeated` procedure of exercise 1.43. Assume that any arithmetic
operations you need are available as primitives.
|#

ANSWER


#| Exercise 1.46 Several of the numerical methods described in this chapter
are instances of an extremely general computational strategy known as
`iterative improvement`. Iterative improvement says that, to compute
something, we start with an initial guess for the answer, test if the guess
is good enough, and otherwise improve the guess and continue the process
using the improved guess as the new guess. Write a procedure
`iterative-improve` that takes two procedures as arguments: a method for
telling whether a guess is good enough and a method for improving a
guess. `Iterative-improve` should return as its value a procedure that
takes a guess as argument and keeps improving the guess until it is good
enough. Rewrite the `sqrt` procedure of section 1.1.7 and the `fixed-point`
procedure of section 1.3.3 in terms of `iterative-improve`.
|#

ANSWER
