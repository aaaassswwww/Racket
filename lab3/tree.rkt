#lang racket

; tree-1:
; 8
; ├── 91
; │   └── 77
; │       └── 64
; ├── 38
; ├── 65
; │   ├── 13
; │   │   └── 66
; │   └── 61
; └── 23
; equals to '(8 (91 (77 (64))) (38) (65 (13 (66)) (61)) (23)))
(define tree-1 (list 8
                     (list 91
                           (list 77
                                 (list 64)))
                     (list 38)
                     (list 65
                           (list 13
                                 (list 66))
                           (list 61))
                     (list 23)))

; fold-tree :: (a tree, (a, b list) -> b) -> b
(define (fold-tree tree f)
  (define (fold f)
    (lambda (x) 
      (fold-tree x f)))
  (f (car tree) (map (fold f) (cdr tree))))

; sum-tree :: Number tree -> Number
(define (sum-tree tree)
  (define (f x ys)
    (if (null? ys)
      x
      (foldr + x ys)))
  (fold-tree tree f))

; dfs :: a tree -> a list
(define (dfs tree)
  (define (f x ys)
    (if (null? ys)
      (list x)
      (cons x (foldr append '() ys))))
  (fold-tree tree f))


; height :: a tree -> Number
(define (height tree)
  (define (f x ys)
    (if (null? ys)
      1
      (+ (foldr 
            (lambda (a b) 
              (if (> a b) a b))
            0
            ys) 
          1)))
  (fold-tree tree f))

(provide tree-1)
(provide fold-tree)
(provide sum-tree)
(provide dfs)
(provide height)
