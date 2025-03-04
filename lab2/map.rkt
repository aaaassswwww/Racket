#lang racket

; my-map :: (a -> b, [a]) -> [b]
; e.g. my-map(f, [x, y, z]) = [f(x), f(y), f(z)]
(define (my-map f xs)
  'todo)

; get-col :: (int, [[a]]) -> [a]
; (get col x mat) represents finding the transpose of the
; x-th column (starting from 1) of a two-dimensional matrix mat
(define (get-col x mat)
  'todo)

(provide my-map)
(provide get-col)