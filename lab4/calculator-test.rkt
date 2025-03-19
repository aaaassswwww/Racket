#lang racket

(require "unittest.rkt")
(require "calculator.rkt")

(define ctx (make-test-context))

(test-case ctx
           (assert-equal (calculate (b-expr 1 'Add 3)) 4)
           (assert-equal (calculate (b-expr (b-expr 1 'Add 3) 'Sub 4)) 0)
           (assert-equal (calculate (u-expr 'Neg (b-expr (b-expr 1 'Add 3) 'Sub
                                                         (b-expr 2 'Mul (u-expr 'Neg 4))))) -12)

           ;; there are hidden testcases on the Online Judge
           )

(run-tests ctx)
