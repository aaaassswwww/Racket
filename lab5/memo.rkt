#lang racket

(require racket/mpair)
(define mcaar (compose mcar mcar))
(define mcdar (compose mcdr mcar))

(struct table (data) #:mutable)

; There are some usage examples related to table,
; which will not be included in the test cases.
;
; (define t (make-table))
; (table-has-key? t 'x) ; #f
; (table-put! t 'x 123)
; (table-has-key? t 'x) ; #t
; (table-get t 'x) ; 123
; (table-put! t 'x 234)
; (table-put! t 3 5)
; (table-get t 'x) ; 234
; (table-get t 3) ; 5

; make-table :: () -> table
(define (make-table)
  (table (mlist)))

; table-put! :: (table, a, b) -> ()
; Add a (key, value) pair to the table
; You can use (set-table-data! t new-data) to update data field of t
(define (table-put! table key value)
  (set-table-data! table (mcons (mcons key value) (table-data table))))

; table-has-key? :: (table, a) -> Bool
; Return whether there is a corresponding key in the table
(define (table-has-key? table key)
  (let loop ([lst (table-data table)])
    (if (null? lst)
      #f
      (let ([pair (mcar lst)])
        (if (equal? (mcar pair) key)
          #t
          (loop (mcdr lst)))))))

; table-get :: (table, a) -> b
; Get the value corresponding to the first key in the table
; It's necessary to ensure that the key exists before calling this procedure
(define (table-get table key)
  (let loop ([lst (table-data table)])
    (if (null? lst)
      (error)
      (let ([pair (mcar lst)])
        (if (equal? (mcar pair) key)
          (mcdr pair)
          (loop (mcdr lst)))))))


; memoize :: (a -> b) -> (a -> b)
; Accept a function and return a function that can cache computed results
;
; Examples:
;
; (define mod 19260817)
;
; ; fib :: Int -> Int
; ; Calculate the n-th term of the Fibonacci sequence (A000045) mod 19260817
; (define (fib n)
;   (if (< n 2)
;       (remainder n mod)
;       (remainder (+ (fib (- n 1)) (fib (- n 2))) mod)))   
;
; (fib 100) ; very slow QAQ
;
; (set! fib (memoize fib))
;
; (fib 100) ; very fast OvO
;
;由于传入的函数在定义时绑定了环境，即使替换了全局的函数定义后，记忆化函数内部仍然使用的是未经记忆化改变的函数，这意味只要在原始
;定义中使用了递归的函数，都不可能通过简单的全局替换来实现记忆化？？？？？？？
;原本的写法没有问题，主要是table-put的实现中一直没有通过set-table-data来更新table的值域，导致table始终为空，因此一直跳过查找缓存的
;过程。
;原来问题的解答，实际上在set！之后，传入的闭包里面在没有命中缓存的时候调用的是原来的函数，但是在原来函数内部的递归部分调用的是全局
;的f，所以这一部分调用的就是记忆化后的函数了，并不会产生之前想象中的问题，大概是我不停逼问导致了ai幻觉。
(define (memoize f)
  (let ([table (make-table)])
    (lambda (k) 
          (if (table-has-key? table k)
            (table-get table k)
            (let ([result (f k)])
              (table-put! table k result)
              result)))))


(provide memoize)
