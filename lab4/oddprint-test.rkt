#lang racket

(require "unittest.rkt")
(require "oddprint.rkt")

(define ctx (make-test-context))

(test-case ctx
           (assert-equal (oddprint '()) "empty")
           (assert-equal (oddprint '(7)) "one-lucky")
           (assert-equal (oddprint '((((1))))) "one")
           (assert-equal (oddprint '(1 (7 2))) "two-lucky")
           (assert-equal (oddprint '(1 (2 7))) "two")
           (assert-equal (oddprint '(7 (1 2))) "two")
           (assert-equal (oddprint '(1 4 3)) "three-unlucky")
           (assert-equal (oddprint '(1 (4) 3)) "oh")
           (assert-equal (oddprint '(1 2 3)) "oh")

           ;; there are hidden testcases on the Online Judge
           )

(run-tests ctx)
