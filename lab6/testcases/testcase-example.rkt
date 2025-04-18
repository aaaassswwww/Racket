#lang racket

(define x 3)
(define y 5)
(define sum (+ (+ x 2) y))
(displayln sum)

(begin
  (display "x = ")
  (displayln x)
  (display "y = ")
  (displayln y)
  (display "(+ (+ x 2) y) = ")
  (displayln sum))