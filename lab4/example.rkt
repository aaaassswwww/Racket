#lang racket

(struct Point (x y)) ; 声明一个结构体类型 Point，有两个域 x 和 y

; 这相当于你自动获得了一个 constructor、一个 predicate 和若干 accessor
(define p_a (Point 1 2))      ; constructor
(println (Point? p_a))        ; #t
(println (Point? (cons 1 2))) ; #f
(println (Point-x p_a))       ; 1
(println (Point-y p_a))       ; 2

(struct Line (p1 p2))
(struct Circle (p r))
(define line_1 (Line p_a (Point 3 4)))
(define circle_1 (Circle p_a 2))

; 使用模式匹配访问数据
; (match val [cond1 e1] [cond2 e2] ...) 的语义是：
; 1. 若 val 满足模式 cond1 则整个表达式值为 e1
; 2. 若 val 满足模式 cond2 则整个表达式值为 e2
; ...
; 匹配是从上往下顺序进行的，模式可以嵌套，下划线表示通配符
; (? pred pat ...) 表示当 (pred val) 为 #t 且 val 满足模式 pat 时才匹配
;
; 注：[] 和 () 是可以替换的，这里混合使用只是为了看得清楚
(define (my-print s)
  (match s
    [(Point 0 0) (printf "origin\n")]
    [(Point 2 _) (printf "a point with x=2\n")]
    [(Point _ _) (printf "a point\n")]
    [7 (printf "a lucky number 7\n")]
    [(? number? x) (printf "just a number ~a\n" x)]
    [(Line (Point x1 y1) (Point x2 y2))
     (printf "a line from (~a,~a) to (~a,~a)\n" x1 y1 x2 y2)]
    [(Circle (Point x y) r)
     (printf "a circle with a radius of ~a and a center at (~a,~a)\n" r x y)]
    [_ (printf "i don't know how to print\n")]))

(my-print 12)               ; just a number 12
(my-print 7)                ; a lucky number 7
(my-print (Point 0 0))      ; origin
(my-print (Point 2 3))      ; a point with x=2
(my-print (Line-p2 line_1)) ; a point
(my-print line_1)           ; a line from (1,2) to (3,4)
(my-print circle_1)         ; a circle with a radius of 2 and a center at (1,2)
(my-print (list 1 2 3))     ; i don't know how to print
