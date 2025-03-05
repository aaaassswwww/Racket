#lang racket

; my-curry :: ((a, b) -> c) -> a -> b -> c
(define (my-curry f)
  (lambda (a) 
      (lambda(b) (f a b))))

(provide my-curry)