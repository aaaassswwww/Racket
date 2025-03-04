#lang racket

(require "unittest.rkt")
(require "map.rkt")

(define (square x) (* x x))
(define (add2 x) (+ x 2))
(define (mul5 x) (* x 5))
(define (is-even? x) (= 0 (modulo x 2)))
(define (string->length str) (string-length str))
(define (nested-square x) (square (+ x 1)))
(define list1to5 (list 1 2 3 4 5))
(define mixed-list (list 3 -2 0 10))
(define str-list (list "hi" "test" "scheme"))
(define singleton-list (list 7))
(define nested-list (list (list 1 2) (list 3 4)))
(define mat1 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(define ctx (make-test-context))

(test-case ctx
           ; 基础数值运算
           (assert-equal (my-map square list1to5) (list 1 4 9 16 25))
           (assert-equal (my-map add2 list1to5) (list 3 4 5 6 7))
           (assert-equal (my-map mul5 list1to5) (list 5 10 15 20 25))
           ; 类型转换测试
           (assert-equal (my-map is-even? list1to5)
                         (list #f #t #f #t #f))
           ; 字符串处理
           (assert-equal (my-map string->length str-list)
                         (list 2 4 6))
           ; 负数/零处理
           (assert-equal (my-map square mixed-list)
                         (list 9 4 0 100))
           ; 单元素列表
           (assert-equal (my-map add2 singleton-list)
                         (list 9))
           ; 嵌套函数调用
           (assert-equal (my-map nested-square (list 1 2 3))
                         (list 4 9 16))
           ; 列表结构保持验证
           (assert-equal (my-map car nested-list)
                         (list 1 3))
           (assert-equal (my-map cdr nested-list)
                         (list (list 2) (list 4)))

           ; 取矩阵第 x 列
           (assert-equal (get-col 1 mat1) (list 1 4 7))
           (assert-equal (get-col 2 mat1) (list 2 5 8))
           (assert-equal (get-col 3 mat1) (list 3 6 9))
           )

(run-tests ctx)