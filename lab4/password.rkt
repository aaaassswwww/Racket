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
    [(MinimumLength len) (MinimumLength-judge pw len)]
    [(ContainsSome chars) (ContainsSome-judge (string->list pw) (string->list chars))]
    [(DoesNotContain chars) (DoesNotContain-judge (string->list pw) (string->list chars))]
    [(And r1 r2) (and (validate pw r1) (validate pw r2))]
    [(Or r1 r2) (or (validate pw r1) (validate pw r2))]
    [_ (error "unknown rule ~a" rule)]))

(define (MinimumLength-judge pw len)
  (>= (string-length pw) len))

(define (ContainsSome-judge pw chars)
  (for/or ([c pw])
    (and (member c chars) #t)))

(define (DoesNotContain-judge pw chars)
  (not (ContainsSome-judge pw chars)))

;;这一部分是导致测试不通过的原因；推测是因为r1中还有可能包括and or；所以需要使用递归
;;明明caculator想到要用递归，怎么这个就没想到呢，真是脑子抽了
;; (define (judge rule)
;;   (if (MinimumLength? rule)
;;     (lambda (pw) (MinimumLength-judge pw (MinimumLength-len rule)))
;;     (if (ContainsSome? rule)
;;       (lambda (pw) (ContainsSome-judge (string->list pw) (string->list (ContainsSome-chars rule))))
;;       (lambda (pw) (DoesNotContain-judge (string->list pw) (string->list (DoesNotContain-chars rule) ))))))

(provide validate MinimumLength ContainsSome DoesNotContain And Or)
