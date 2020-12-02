;; -*- mode: scheme; fill-column: 75; comment-column: 50; coding: utf-8 -*-

;; Chapter 2 of SICP

#| Exercise 2.1 Define a better version of `make-rat` that handles both positive
and negative arguments. `Make-rat` should normalize the sign so that if the
rational number is positive, both the numerator and denominator are positive,
and if the rational number is negative, only the numerator is negative.
|#

(define (make-rat n d)
  (let ((g (abs (gcd n d)))
        (sign (if (> (* n d) 0) 1 -1)))
    (cons (/ (abs n) g) (/ (* sign (abs d)) g))))


#| Exercise 2.2 Consider the problem of representing line segments in a
plane. Each segment is represented as a pair of points: a starting point
and an ending point. Define a constructor `make-segment` and selectors
`start-segment` and `end-segment` that define the representation of
segments in terms of points. Furthermore, a point can be represented as a
pair of numbers: the x coordinate and the y coordinate. Accordingly,
specify a constructor `make-point` and selectors `x-point` and `y-point`
that define this representation. Finally, using your selectors and
constructors, define a procedure `midpoint-segment` that takes a line
segment as argument and returns its midpoint (the point whose coordinates
are the average of the coordinates of the endpoints). To try your
procedures, you'll need a way to print points:
```
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
```
|#

ANSWER


#| Exercise 2.3 Implement a representation for rectangles in a
plane. (Hint: You may want to make use of exercise 2.2.) In terms of your
constructors and selectors, create procedures that compute the perimeter
and the area of a given rectangle. Now implement a different representation
for rectangles. Can you design your system with suitable abstraction
barriers, so that the same perimeter and area procedures will work using
either representation?
|#

ANSWER
