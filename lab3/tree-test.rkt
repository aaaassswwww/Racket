#lang racket

(require "unittest.rkt")
(require "tree.rkt")

; 2
; ├── 10
; │   └── 7
; │       └── 9
; │           └── 4
; ├── 10
; │   ├── 1
; │   └── 8
; └── 10
;     └── 10
(define tree-2 '(2 (10 (7 (9 (4)))) (10 (1) (8)) (10 (10))))

; 6
; ├── 3
; │   ├── 5
; │   ├── 3
; │   │   └── 6
; │   └── 1
; ├── 5
; │   └── 7
; │       └── 3
; └── 2
(define tree-3 '(6 (3 (5) (3 (6)) (1)) (5 (7 (3))) (2)))

; 10
; ├── 9
; ├── 9
; ├── 10
; ├── 6
; ├── 6
; ├── 6
; ├── 3
; ├── 3
; └── 9
(define tree-4 '(10 (9) (9) (10) (6) (6) (6) (3) (3) (9)))

; 2
; └── 2
;     └── 1
;         └── 5
;             └── 2
;                 └── 6
;                     └── 3
;                         └── 8
;                             └── 10
;                                 └── 9
(define tree-5 '(2 (2 (1 (5 (2 (6 (3 (8 (10 (9)))))))))))

(define tree-6 '(1))

(define ctx (make-test-context))

(test-case ctx
           (assert-equal (sum-tree tree-1) 506)
           (assert-equal (sum-tree tree-2) 71)
           (assert-equal (sum-tree tree-3) 41)
           (assert-equal (sum-tree tree-4) 71)
           (assert-equal (sum-tree tree-5) 48)
           (assert-equal (sum-tree tree-6) 1)

           (assert-equal (dfs tree-1) '(8 91 77 64 38 65 13 66 61 23))
           (assert-equal (dfs tree-2) '(2 10 7 9 4 10 1 8 10 10))
           (assert-equal (dfs tree-3) '(6 3 5 3 6 1 5 7 3 2))
           (assert-equal (dfs tree-4) '(10 9 9 10 6 6 6 3 3 9))
           (assert-equal (dfs tree-5) '(2 2 1 5 2 6 3 8 10 9))
           (assert-equal (dfs tree-6) '(1))

           (assert-equal (height tree-1) 4)
           (assert-equal (height tree-2) 5)
           (assert-equal (height tree-3) 4)
           (assert-equal (height tree-4) 2)
           (assert-equal (height tree-5) 10)
           (assert-equal (height tree-6) 1)
           )

(run-tests ctx)