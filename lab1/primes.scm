#lang scheme

(define (count-primes-in n)
  (let loop ((i 2) (count 0))
    (if (> i n)
        count
        (if (is-prime i)
            (loop (+ i 1) (+ count 1))
            (loop (+ i 1) count)))))


(define (is-prime n)
  (if (< n 2)
      #f
      (let loop ((i 2))
        (if (> (* i i) n)
            #t
            (if (= (modulo n i) 0)
                #f
                (loop (+ i 1)))))))

; Tests

(define (test n expected)
    (let ((result (count-primes-in n)))
        (if (= result expected)
            (display (format "Ok, expect ~a got ~a.\n" expected result))
            (error (format "Expect ~a got ~a.\n" expected result)))))

(test 2 1)
(test 3 2)
(test 4 2)
(test 5 3)
(test 6 3)
(test 7 4)
(test 8 4)
(test 9 4)
(test 10 4)
(test 6011 785)
(test 114514 10830)
(test 321299 27709)
(test 377386 32097)
(test 675606 54733)
(test 765249 61378)