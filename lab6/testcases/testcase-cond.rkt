#lang racket

(cond
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
  [else "Good evening"])