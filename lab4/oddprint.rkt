#lang racket

; oddprint :: list -> string
; The rules are described in the PDF document
;
; Examples:
; (oddprint '()) => "empty"
; (oddprint '(7)) => "one-lucky"
; (oddprint '((((1))))) => "one"
; (oddprint '(1 (7 2))) => "two-lucky"
; (oddprint '(1 (7))) => "two-lucky"
; (oddprint '(1 (2 7))) => "two"
; (oddprint '(7 (1 2))) => "two"
; (oddprint '(1 4 3)) => "three-unlucky"
; (oddprint '(1 (4) 3)) => "oh"
; (oddprint '(1 2 3)) => "oh"
(define (oddprint l)
  (match l
    ['() "empty"]
    ['(7) "one-lucky"]
    [(list _) "one"]
    [(list _ (list 7 _ ...)) "two-lucky"]
    [(list _ _) "two"]
    [(? (lambda (x) (and (= (length x) 3) (member 4 x)))) "three-unlucky"]
    [_ "oh"]
    ))


(provide oddprint)
