#lang racket

(if true
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
    "Not a leap year")