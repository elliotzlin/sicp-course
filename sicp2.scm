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
