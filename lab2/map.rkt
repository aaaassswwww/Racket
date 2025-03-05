#lang racket

; my-map :: (a -> b, [a]) -> [b]
; e.g. my-map(f, [x, y, z]) = [f(x), f(y), f(z)]
(define (my-map f xs)
  (if (pair? xs)
    (cons
      (f (car xs))
      (my-map f (cdr xs)))
    xs))

; get-col :: (int, [[a]]) -> [a]
; (get col x mat) represents finding the transpose of the
; x-th column (starting from 1) of a two-dimensional matrix mat
(define (get-col x mat)
  (define (get x)
    (lambda(list)
      (if (= x 1)
        (car list)
        ((get (- x 1)) 
          (cdr list)))))
  (my-map (get x) mat))

(provide my-map)
(provide get-col)