#lang racket

(require "unittest.rkt")
(require "filter.rkt")

(define list1to5 (list 1 2 3 4 5))
(define empty-list (list))
(define mixed-list (list -3 -2 0 1 4))
(define dupes-list (list 2 2 3 4 2))

(define (negative? x) (< x 0))
(define (zero? x) (= x 0))
(define (equal3? x) (= x 3))
(define (always-true? x) #t)
(define (always-false? x) #f)
(define (>2&<5? x) (and (> x 2) (< x 5)))

(define ctx (make-test-context))

(test-case ctx
           ; 基础谓词
           (assert-equal (my-filter negative? mixed-list) (list -3 -2))
           (assert-equal (my-filter zero? mixed-list) (list 0))
           ; 精确匹配
           (assert-equal (my-filter equal3? (list 1 3 3 4)) (list 3 3))
           ; 全量过滤
           (assert-equal (my-filter always-true? list1to5) list1to5)
           (assert-equal (my-filter always-false? list1to5) empty-list)
           ; 复合条件
           (assert-equal (my-filter >2&<5? list1to5) (list 3 4))
           ; 单元素列表
           (assert-equal (my-filter even? (list 2)) (list 2))
           (assert-equal (my-filter even? (list 3)) empty-list)
           ; 重复元素
           (assert-equal (my-filter even? dupes-list) (list 2 2 4 2))
           ; 全匹配/不匹配
           (assert-equal (my-filter negative? (list 1 2 3)) empty-list)
           (assert-equal (my-filter positive? (list -1 -2 -3)) empty-list)

           ; 基础交集，保持顺序
           (assert-equal (intersect '(1 2 3) '(2 3 4)) '(2 3))
           ; 顺序与lst1一致，即使lst2顺序不同
           (assert-equal (intersect '(3 1 2) '(1 2 3)) '(3 1 2))
           ; lst1为空
           (assert-equal (intersect '() '(1 2 3)) '())
           ; lst2为空
           (assert-equal (intersect '(1 2 3) '()) '())
           ; 无交集元素
           (assert-equal (intersect '(1 2 3) '(4 5 6)) '())
           ; 完全交集，顺序按lst1
           (assert-equal (intersect '(1 2 3) '(3 2 1)) '(1 2 3))
           ; 单个元素存在
           (assert-equal (intersect '(1) '(1)) '(1))
           ; 单个元素不存在
           (assert-equal (intersect '(1) '(2)) '())
           )

(run-tests ctx)