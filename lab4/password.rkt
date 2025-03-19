#lang racket

; Some functions that may be useful to you:
;
; 1. string-length :: string -> int
;    Obtain the length of a string
;
; 2. string->list :: string -> char list
;    Convert a string to a character list

; |password| >= len
(struct MinimumLength (len))

; contains at least one of given characters
(struct ContainsSome (chars))

; does not contain any of the given characters
(struct DoesNotContain (chars))

; and'ing two requirements
(struct And (r1 r2))

; or'ing
(struct Or (r1 r2))

; validate :: (string, Rules) -> boolean
; (validate pw rule) checks if the password is valid under the given rules
;
; Examples:
; (validate "short" (MinimumLength 8)) => #f
; (validate "p4ssword" (And (ContainsSome "1234") (MinimumLength 5))) => #t
; (validate "p4ssword" (And (ContainsSome "1235") (MinimumLength 5))) => #f
; (validate "password" (DoesNotContain "0123456789")) => #t
(define (validate pw rule)
  (match rule
    ; todo
    [_ (error "unknown rule ~a" rule)]))

(provide validate MinimumLength ContainsSome DoesNotContain And Or)