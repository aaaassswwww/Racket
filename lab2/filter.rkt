#lang racket

; my-filter :: (a -> Bool, [a]) -> [a]
(define (my-filter f xs)
  (if (pair? xs)
    (if(f (car xs)) 
      (cons (car xs)
            (my-filter f (cdr xs)))
      (my-filter f (cdr xs)))
    xs))

; intersect :: ([int], [int]) -> [int]
;
; (intersect lst1 lst2) will result in a new list,
; where each element in the new list appears in both
; `lst1` and `lst2` in the same order as in `lst1`
;
; Assumption: There are no duplicate elements in both
; `lst1` and `lst2`
(define (intersect lst1 lst2)
  (if (pair? lst1)
    (if (pair? (my-filter (true? (car lst1)) lst2))
      (cons
        (car (my-filter (true? (car lst1)) lst2))
        (intersect (cdr lst1) lst2))
      (intersect (cdr lst1) lst2))
    '())) 

(define (true? x)
  (lambda (y)
    (equal? x y)))

(provide my-filter)
(provide intersect)