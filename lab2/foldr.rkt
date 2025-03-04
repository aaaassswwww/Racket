#lang racket

; my-foldr :: ((a, b) -> b, b, [a]) -> b
; e.g. my-foldr(f, init, [x, y, z]) = f(x, f(y, f(z, init)))
(define (my-foldr f init xs)
  'todo)

; group-adjacent :: [int] -> [[int]]
;
; (group-adjacent xs) will group adjacent identical elements
; in the list and return the grouped list.
; Grouping should maintain the original order of elements.
(define (group-adjacent xs)
  'todo)

(provide my-foldr)
(provide group-adjacent)