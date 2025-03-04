#lang racket

(require "unittest.rkt")
(require "compose.rkt")

(define add2 (lambda (x) (+ x 2)))
(define mul5 (lambda (x) (* x 5)))
(define add2-then-mul5 (compose mul5 add2))
(define mul5-then-add2 (compose add2 mul5))

(define add1 (lambda (x) (+ x 1)))
(define sub3 (lambda (x) (- x 3)))
(define mul3 (lambda (x) (* x 3)))
(define square (lambda (x) (* x x)))

(define add1-then-mul3 (compose mul3 add1))
(define sub3-then-square (compose square sub3))
(define square-then-sub3 (compose sub3 square))

(define double (lambda (x) (* x 2)))
(define zero-check (compose double double))

(define add10 (lambda (x) (+ x 10)))
(define mul-neg2 (lambda (x) (* x -2)))
(define add10-then-mul-neg2 (compose mul-neg2 add10))

(define stringify (lambda (x) (number->string x)))
(define add-excl (lambda (s) (string-append s "!")))
(define process-num (compose add-excl stringify))

(define is-even? (lambda (x) (= (modulo x 2) 0)))
(define logical-not (lambda (b) (not b)))
(define check-odd (compose logical-not is-even?))

(define ctx (make-test-context))

(test-case ctx
           (assert-equal (add2-then-mul5 7) 45)
           (assert-equal (mul5-then-add2 7) 37)
           (assert-equal (add1-then-mul3 0) 3)   ; (0+1)*3 = 3
           (assert-equal (sub3-then-square 4) 1)  ; (4-3)^2 = 1
           (assert-equal (square-then-sub3 4) 13) ; (4^2)-3 = 13
           (assert-equal (zero-check 0) 0)        ; 0*2*2 = 0
           (assert-equal (add10-then-mul-neg2 -5) -10) ; (-5+10)*-2 = -10
           (assert-equal (process-num 5) "5!")     ; "5" -> "5!"
           (assert-equal (check-odd 5) #t)         ; 5是奇数，取反后为#t
           )

(run-tests ctx)