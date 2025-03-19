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
    ; todo
    ))

(provide oddprint)