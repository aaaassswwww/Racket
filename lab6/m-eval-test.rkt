#lang racket

(require "unittest.rkt")
(require "m-eval.rkt")

(define ctx (make-test-context))

(struct mcase (input output))
(define init-env (generate-init-env))
(define (reset-env) (set! init-env (generate-init-env)))
(define (run case)
  (reset-env)
  (map (lambda (code) (m-eval init-env code)) case))
(define VOID (void))

(define-syntax (assert-ok stx)
  (syntax-case stx ()
    [(_ case)
     #'(let ([c case])
         (assert-equal (run (mcase-input c)) (mcase-output c)))]))

(define testcase-example
  (mcase

   '((define x 3)
     (define y 5)
     (define sum (+ (+ x 2) y))
     (displayln sum)

     (begin
       (display "x = ")
       (displayln x)
       (display "y = ")
       (displayln y)
       (display "(+ (+ x 2) y) = ")
       (displayln sum)))

   (list 'DEFINE 'DEFINE 'DEFINE VOID VOID)))

(define testcase-if
  (mcase

   '((if true
         "t"
         "f")

     (if false
         "t"
         "f")

     (if (= 1 1)
         "t"
         "f")

     (if (= 1 2)
         "t"
         "f")

     (if (list 1 2)
         "t"
         "f")

     (newline)

     (define x -5)
     (if (> x 0)
         "Positive"
         "Non-positive")

     (define score 85)
     (if (>= score 90)
         "A"
         (if (>= score 80)
             "B"
             (if (>= score 70)
                 "C"
                 (if (>= score 60)
                     "D"
                     "F"))))

     (define num 50)
     (if (> num 0)
         (if (< num 100)
             "Within 0-100"
             "Too high")
         "Too low")

     (if (= (remainder num 2) 0)
         "Even"
         "Odd")

     (define year 2000)
     (if (= (remainder year 4) 0)
         (if (= (remainder year 100) 0)
             (if (= (remainder year 400) 0)
                 "Leap year"
                 "Not a leap year")
             "Leap year")
         "Not a leap year"))

   (list "t" "f" "t" "f" "t" VOID 'DEFINE "Non-positive" 'DEFINE "B" 'DEFINE "Within 0-100" "Even" 'DEFINE "Leap year")))

(define testcase-cond
  (mcase

   '((cond
       [true
        (display "there")
        (display "is")
        (display "a")
        (display "sequence")
        (display "of")
        (display "actions")
        '!]
       [else "no action"])

     (define score 85)
     (cond
       [(>= score 90) "A"]
       [(>= score 80) "B"]
       [(>= score 70) "C"]
       [(>= score 60) "D"]
       [else "F"])

     (define temperature 25)
     (cond
       [(> temperature 30) "Hot"]
       [(>= temperature 20) "Comfortable"]
       [else "Cold"])

     (define num -5)
     (cond
       [(> num 0) "Positive"]
       [(< num 0) "Negative"]
       [else "Zero"])

     (define hour 15)
     (cond
       [(< hour 12) "Good morning"]
       [(< hour 18) "Good afternoon"]
       [else "Good evening"]))

   (list '! 'DEFINE "B" 'DEFINE "Comfortable" 'DEFINE "Negative" 'DEFINE "Good afternoon")))

(define testcase-lambda
  (mcase

   '((define a 100)
     (define b 200)
     (define fun (lambda (a b) (+ a (* b 2))))
     (fun 5 3)
     (fun 1 2)
     (fun a b)

     (define apply-test
       (lambda (f x y)
         (f x y)))
     (apply-test (lambda (a b) (- a b)) 10 4)

     (((lambda (x) (lambda (y) (+ x y))) 100) 50)

     ((lambda (x y)
        ((lambda (a b) (* (+ a b) (- a b))) x y))
      8 5)

     (define delayed-add
       (lambda ()
         (lambda (x y)
           (+ x y))))
     ((delayed-add) 20 30)

     (define print-something
       (lambda ()
         (display "body")
         (display "of")
         (display "lambda")
         (displayln "")))
     (print-something))

   (list 'DEFINE 'DEFINE 'DEFINE 11 5 500 'DEFINE 6 150 39 'DEFINE 50 'DEFINE VOID)))

(define testcase-factorial
  (mcase

   '((define factorial
       (lambda (n)
         (if (= n 0)
             1
             (* (factorial (- n 1)) n))))

     (factorial 10))

   (list 'DEFINE 3628800)))

(define testcase-primes
  (mcase

   '((define prime?-helper
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

     (count-primes-in 6011))

   (list 'DEFINE 'DEFINE 'DEFINE 785)))

(test-case ctx
           ;;; release
           (assert-ok testcase-if)
           (assert-ok testcase-cond)
           (assert-ok testcase-lambda)
           (assert-ok testcase-factorial)
           (assert-ok testcase-primes)

           ;;; there are hidden testcases on the Online Judge
           ; if system-test
           ; cond system-test
           ; lambda system-test
           ; factorial system-test
           ; primes system-test
           )

(run-tests ctx)
