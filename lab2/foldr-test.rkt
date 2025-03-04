#lang racket

(require "unittest.rkt")
(require "foldr.rkt")

(define list1to5 (list 1 2 3 4 5))

(define ctx (make-test-context))

(test-case ctx
           (assert-equal (my-foldr + 0 list1to5) 15)
           (assert-equal (my-foldr * 1 list1to5) 120)
           (assert-equal (my-foldr + 0 (list)) 0)

           ; 空列表返回初始值
           (assert-equal (my-foldr + 0 '()) 0)

           ; 单元素列表，直接应用函数
           (assert-equal (my-foldr (lambda (x acc) (+ x acc)) 0 '(5)) 5)

           ; 多元素加法：1 + (2 + (3 + 0)) = 6
           (assert-equal (my-foldr + 0 '(1 2 3)) 6)

           ; 多元素乘法：2 * (3 * (4 * 1)) = 24
           (assert-equal (my-foldr * 1 '(2 3 4)) 24)

           ; 字符串拼接：按右结合顺序连接
           (assert-equal (my-foldr string-append "" '("a" "b" "c")) "abc")

           ; 使用 `cons` 保持原列表顺序（等价于 `append` 效果）
           (assert-equal (my-foldr cons '() '(1 2 3)) '(1 2 3))

           ; 反转列表：通过追加到累积结果末尾
           (assert-equal (my-foldr (lambda (x acc) (append acc (list x))) '() '(1 2 3)) '(3 2 1))

           ; 存在性检查：是否存在元素 >5（列表含7时返回#t）
           (assert-equal (my-foldr (lambda (x acc) (or (> x 5) acc)) #f '(3 7 2)) #t)

           ; 类型转换：符号转字符串并按原顺序保留
           (assert-equal (my-foldr (lambda (x acc) (cons (symbol->string x) acc)) '() '(a b c)) '("a" "b" "c"))

           ; 阶乘计算：4! = 24（列表含1-4）
           (assert-equal (my-foldr * 1 '(1 2 3 4)) 24)

           ; 混合类型操作：数值和布尔值组合
           (assert-equal (my-foldr (lambda (x acc) (and (number? x) acc)) #t '(5 "a" 3)) #f)

           ; 基础测试
           (assert-equal (group-adjacent '()) '())                    ; 空列表
           (assert-equal (group-adjacent '(1)) '((1)))                 ; 单元素
           (assert-equal (group-adjacent '(1 1 1)) '((1 1 1)))         ; 全相同元素

           ; 混合分组测试
           (assert-equal (group-adjacent '(1 1 2 3 3 3 1))
                         '((1 1) (2) (3 3 3) (1)))
           (assert-equal (group-adjacent '(1 1 2 3 3))
                         '((1 1) (2) (3 3)))
           (assert-equal (group-adjacent '(1 2 1))
                         '((1) (2) (1)))
           )

(run-tests ctx)