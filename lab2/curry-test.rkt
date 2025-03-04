#lang racket

(require "unittest.rkt")
(require "curry.rkt")

(define ctx (make-test-context))

(define add1 ((my-curry +) 1))
(define one-sub ((my-curry -) 1))
(define (power x y) (expt x y)) ; 定义一个幂运算函数
(define pow2 ((my-curry power) 2))
(define add10 ((my-curry +) 10))
(define mul3 ((my-curry *) 3))
(define prepend1 ((my-curry cons) 1))
(define greet ((my-curry string-append) "Hello, "))
(define ten-div ((my-curry /) 10))

(test-case ctx
           (assert-equal (add1 (one-sub 10)) -8) ; 1 + (1 - 10) = -8
           (assert-equal (one-sub (add1 5)) -5) ; (1 - (1 + 5)) = -5
           (assert-equal (pow2 3) 8) ; 2^3 = 8
           (assert-equal (pow2 5) 32) ; 2^5 = 32
           (assert-equal (mul3 (add10 5)) 45) ; (5 + 10) * 3 = 45
           (assert-equal (add10 (mul3 5)) 25) ; (5 * 3) + 10 = 25
           (assert-equal (prepend1 '(2 3 4)) '(1 2 3 4)) ; 将 1 添加到列表前面
           (assert-equal (prepend1 '()) '(1)) ; 将 1 添加到空列表
           (assert-equal (greet "FDU") "Hello, FDU") ; 连接字符串
           (assert-equal (greet "Racket") "Hello, Racket") ; 连接字符串
           (assert-equal (ten-div 10) 1) ; 10 / 10 = 1
           (assert-equal (ten-div 2) 5) ; 10 / 2 = 5
           )

(run-tests ctx)