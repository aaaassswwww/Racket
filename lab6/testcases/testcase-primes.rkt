#lang racket

(define prime?-helper
  (lambda (x i)
    (if (> (* i i) x)
        1
        (if (= (remainder x i) 0)
            0
            (prime?-helper x (+ i 1))))))

; assume x>=2
(define prime? (lambda (x) (prime?-helper x 2)))

(define count-primes-in (lambda (n)
                          (if (= n 1)
                              0
                              (+ (count-primes-in (- n 1))
                                 (prime? n)))))

(count-primes-in 6011)