#lang racket

(require "unittest.rkt")
(require "password.rkt")

(define ctx (make-test-context))

(test-case ctx
           (assert-equal (validate "short" (MinimumLength 8)) #f)
           (assert-equal (validate "veryLongPassword" (MinimumLength 8)) #t)
           (assert-equal (validate "password" (ContainsSome "0123456789")) #f)
           (assert-equal (validate "p4ssword" (ContainsSome "0123456789")) #t)
           (assert-equal (validate "password" (DoesNotContain "0123456789")) #t)
           (assert-equal (validate "p4ssword" (DoesNotContain "0123456789")) #f)
           (assert-equal (validate "p4ssword" (And (ContainsSome "1234") (MinimumLength 5))) #t)
           (assert-equal (validate "p4ss" (And (ContainsSome "1234") (MinimumLength 5))) #f)
           (assert-equal (validate "p4ss" (Or (ContainsSome "1234") (MinimumLength 5))) #t)

           ;; there are hidden testcases on the Online Judge
           )

(run-tests ctx)
