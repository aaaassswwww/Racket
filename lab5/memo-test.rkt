#lang racket

(require "unittest.rkt")
(require "memo.rkt")

(define mod 19260817)

; fib :: Int -> Int
; Calculate the n-th term of the Fibonacci sequence (A000045) mod 19260817
(define (fib n)
  (if (< n 2)
      (remainder n mod)
      (remainder (+ (fib (- n 1)) (fib (- n 2))) mod)))

; narayana :: Int -> Int
; Calculate the n-th term of the Narayana's cows sequence (A000930) mod 19260817
(define (narayana n)
  (if (< n 3)
      1
      (remainder (+ (narayana (- n 1)) (narayana (- n 3))) mod)))

; pascal :: (Int, Int) -> Int
; Calculate the element in row n and column n of the Pascal's triangle (A007318) mod 19260817
(define (pascal n m)
  (if (or (= m 0) (= m n))
      1
      (remainder (+ (pascal (- n 1) (- m 1))
                    (pascal (- n 1) m))
                 mod)))

; stirling :: (Int, Int) -> Int
; Calculate the element in row n and column n of the triangle of Stirling numbers of the second kind (A008277) mod 19260817
(define (stirling n m)
  (cond ((and (= n 0) (= m 0)) 1)
        ((or (= n 0) (= m 0)) 0)
        (else (remainder (+ (* m (stirling (- n 1) m))
                            (stirling (- n 1) (- m 1)))
                         mod))))

(define (forward-2-to-1 f)
  (lambda (n m) (f (list n m))))

(define (forward-1-to-2 f)
  (lambda (nm)
    (define n (car nm))
    (define m (cadr nm))
    (f n m)))

(define (record-called-times f limit)
  (define counter 0)
  (lambda (arg)
    (if (> counter limit)
        (error "time limit exceed")
        (let ([x (f arg)])
          (set! counter (+ counter 1))
          x))))

(define limit 100000) ; 1e5
(set! fib (record-called-times (memoize fib) limit))
(set! narayana (record-called-times (memoize narayana) limit))
(set! pascal (forward-2-to-1 (record-called-times (memoize (forward-1-to-2 pascal))
                                                  limit)))
(set! stirling (forward-2-to-1 (record-called-times (memoize (forward-1-to-2 stirling))
                                                    limit)))

(define ctx (make-test-context))

(test-case ctx
           ; fib pre-test
           (assert-equal (bundle-test (list
                                       (subtask (fib 10) 55)
                                       (subtask (fib 10000) 2596925)
                                       )) #t)

           ; narayana pre-test
           (assert-equal (bundle-test (list
                                       (subtask (narayana 10) 28)
                                       (subtask (narayana 10000) 18255303)
                                       )) #t)

           ; pascal pre-test
           (assert-equal (pascal 10 6) 210)

           ; stirling pre-test
           (assert-equal (stirling 6 3) 90)

           ;;; there are hidden testcases on the Online Judge
           ; fib system-test
           ; narayana system-test
           ; pascal system-test small
           ; pascal system-test big
           ; stirling system-test small
           ; stirling system-test big
           )

(run-tests ctx)
