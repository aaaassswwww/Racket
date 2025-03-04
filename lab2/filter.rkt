#lang racket

; my-filter :: (a -> Bool, [a]) -> [a]
(define (my-filter f xs)
  'todo)

; intersect :: ([int], [int]) -> [int]
;
; (intersect lst1 lst2) will result in a new list,
; where each element in the new list appears in both
; `lst1` and `lst2` in the same order as in `lst1`
;
; Assumption: There are no duplicate elements in both
; `lst1` and `lst2`
(define (intersect lst1 lst2)
  'todo)

(provide my-filter)
(provide intersect)