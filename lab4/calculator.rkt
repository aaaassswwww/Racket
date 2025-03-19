#lang racket

; represents an expression consisting of a unary operator
; op can be:
; 1. Neg -
(struct u-expr (op oprand))

; represents an expression consisting of a binary operator
; op can be:
; 1. Add +
; 2. Sub -
; 3. Mul *
(struct b-expr (lhs op rhs))

; calculate :: expr -> number
; (calculate expr) evaluates the expression and obtains a number
;
; Examples:
; (calculate (b-expr 1 'Add 3)) => (1+3) => 4
; (calculate (b-expr (b-expr 1 'Add 3) 'Sub 4)) => ((1+3)-4) => 0
; (calculate (u-expr 'Neg (b-expr (b-expr 1 'Add 3)
;                                 'Sub
;                                 (b-expr 2 'Mul (u-expr 'Neg 4))))) => (-((1+3)-(2*(-4)))) => -12
(define (calculate expr)
  (match expr
    ; todo
    [_ (error "unknow expression ~a" expr)]))

(provide calculate u-expr b-expr)