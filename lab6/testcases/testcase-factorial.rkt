#lang racket

(define factorial
  (lambda (n)
    (if (= n 0)
        1
        (* (factorial (- n 1)) n))))

(factorial 10)