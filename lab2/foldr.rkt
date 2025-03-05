#lang racket

; my-foldr :: ((a, b) -> b, b, [a]) -> b
; e.g. my-foldr(f, init, [x, y, z]) = f(x, f(y, f(z, init)))
(define (my-foldr f init xs)
  (if (null? xs)
    init
    (f (car xs)
      (my-foldr f init (cdr xs)))
  ))
; group-adjacent :: [int] -> [[int]]
;
; (group-adjacent xs) will group adjacent identical elements
; in the list and return the grouped list.
; Grouping should maintain the original order of elements.
(define (group-adjacent xs)
  (define (add-to-groups x groups)
    (if (null? groups)
      (list (list x))
      (let ([first-group (car groups)])
        (if (equal? x (car first-group))
          (cons (cons x first-group) (cdr groups))
          (cons (list x) groups)))))
  
  (my-foldr add-to-groups '() xs))

(provide my-foldr)
(provide group-adjacent)