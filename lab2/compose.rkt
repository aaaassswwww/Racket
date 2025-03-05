#lang racket

; my-compose :: (b -> c, a -> b) -> (a -> c)
(define (my-compose f g)
  (lambda (a) 
    (f (g a))))

(provide my-compose)