#lang racket

(define a 100)
(define b 200)
(define fun (lambda (a b) (+ a (* b 2))))
(fun 5 3)
(fun 1 2)
(fun a b)

(define apply-test
  (lambda (f x y)
    (f x y)))
(apply-test (lambda (a b) (- a b)) 10 4)

(((lambda (x) (lambda (y) (+ x y))) 100) 50)

((lambda (x y)
   ((lambda (a b) (* (+ a b) (- a b))) x y))
 8 5)

(define delayed-add
  (lambda ()
    (lambda (x y)
      (+ x y))))
((delayed-add) 20 30)

(define print-something
  (lambda ()
    (display "body")
    (display "of")
    (display "lambda")
    (displayln "")))
(print-something)