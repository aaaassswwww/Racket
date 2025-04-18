#lang racket

(require "unittest.rkt")
(require "tree-ans.rkt")

; 2
; ├── 10
; │   └── 7
; │       └── 9
; │           └── 4
; ├── 10
; │   ├── 1
; │   └── 8
; └── 10
;     └── 10
(define tree-2 '(2 (10 (7 (9 (4)))) (10 (1) (8)) (10 (10))))

; 6
; ├── 3
; │   ├── 5
; │   ├── 3
; │   │   └── 6
; │   └── 1
; ├── 5
; │   └── 7
; │       └── 3
; └── 2
(define tree-3 '(6 (3 (5) (3 (6)) (1)) (5 (7 (3))) (2)))

; 10
; ├── 9
; ├── 9
; ├── 10
; ├── 6
; ├── 6
; ├── 6
; ├── 3
; ├── 3
; └── 9
(define tree-4 '(10 (9) (9) (10) (6) (6) (6) (3) (3) (9)))

; 2
; └── 2
;     └── 1
;         └── 5
;             └── 2
;                 └── 6
;                     └── 3
;                         └── 8
;                             └── 10
;                                 └── 9
(define tree-5 '(2 (2 (1 (5 (2 (6 (3 (8 (10 (9)))))))))))

(define tree-6 '(1))

; system tests
(define tree-s1 '(43 (77 (75) (1) (34) (49)) (55 (48 (48)) (39) (5) (25 (53))) (23 (22) (37) (69 (63)) (63)) (69 (26 (45)) (81 (42)) (58) (34) (21 (24))) (8 (22 (47)) (27)) (13 (62)) (79 (9) (74)) (3 (48) (19)) (26 (54 (79)) (89 (63)) (46) (33) (25) (23)) (92 (11 (49)) (13) (30) (29) (69 (47)) (82) (63)) (4 (25) (60) (15) (8)) (30 (43 (44)) (68) (75)) (99) (85 (84)) (49 (87)) (74) (97 (20) (13)) (79 (59)) (29 (36) (49)) (30) (67 (93 (53)) (66) (26)) (83 (76)) (100 (75)) (12 (10)) (47) (13) (64) (50) (62)))
(define tree-s2 '(86 (21 (83) (40)) (80 (74) (6 (20 (50))) (18 (49) (13)) (91 (19) (60))) (28 (26 (57 (74)) (36 (55))) (21) (83 (45) (89) (79)) (44)) (1 (93 (34 (60 (6 (6)))) (56) (30)) (20 (37)) (28 (79)) (46 (79 (97) (81) (90)) (10 (54)) (28)) (65 (23) (57) (89 (98))) (57 (60) (40))) (55 (79 (63)) (76 (35 (96)))) (79 (5) (100 (98) (69)) (36 (50 (47)) (5 (26))) (66) (50)) (56 (8) (92 (12) (48)) (48)) (96 (91) (96)) (17 (37)) (30 (44 (81))) (93) (73 (19 (17) (59)) (20) (76)) (66 (88 (25))) (68) (33) (68)))
(define tree-s3 '(71 (5) (52) (11) (5) (3) (93) (49) (30) (29) (20) (67) (75) (31) (44) (83) (86) (70) (75) (100) (37) (62) (2) (75) (19) (56) (11) (96) (7) (31) (70) (24) (91) (38) (44) (39) (100) (81) (64) (11) (99) (49) (30) (34) (71) (31) (8) (8) (42) (96) (43) (42) (48) (71) (22) (36) (43) (38) (6) (40) (98) (11) (47) (49) (4) (90) (62) (57) (77) (53) (72) (24) (69) (50) (27) (39) (87) (47) (15) (92) (75) (71) (58) (53) (19) (45) (91) (78) (33) (63) (43) (36) (5) (84) (54) (41) (21) (40) (34) (8)))
(define tree-s4 '(41 (24 (91 (34 (84 (85 (22 (13 (40 (29 (8 (86) (48 (42 (34 (35) (74 (34 (82 (72 (28 (54 (98 (93 (61 (76) (36 (85 (85 (14 (91 (3 (6 (78 (55 (59 (43 (40 (62) (60 (70 (13 (89 (17 (84 (99 (27 (51 (90 (93 (68 (27 (19 (14 (95 (39 (44 (19 (23 (45 (33 (61 (30 (39 (89 (69 (73 (17 (94 (47) (69 (45))) (98)))) (89))))) (7) (42 (43))) (24 (87)))) (48) (21)) (79 (46)))))) (51))))))))))) (11)) (89))))) (27))))) (55) (62)))))))) (62)) (62 (76))))))) (89))))))))))))) (94 (50) (20 (72)))))) (55 (49))) (34)))
(define tree-s5 '(17))

(define (fdupag-fold-tree tree f)
  (define (fold-tree-with-f t) (fdupag-fold-tree t f))
  (if (null? tree)
      ('())
      (f (car tree)
         (map fold-tree-with-f (cdr tree)))))

(define (fdupag-sum-tree tree)
  (define (f x ys)
    (+ x (foldr + 0 ys)))
  (fdupag-fold-tree tree f))

(define (fdupag-dfs tree)
  (define (f x ys)
    (cons x (foldr append '() ys)))
  (fdupag-fold-tree tree f))

(define (fdupag-height tree)
  (define (f _ ys)
    (+ 1 (foldr max 0 ys)))
  (fdupag-fold-tree tree f))

(define (fdupag-sum-tree-2 tree)
  (define (f x ys)
    (+ x (foldr + 0 ys)))
  (fold-tree tree f))

(define (fdupag-dfs-2 tree)
  (define (f x ys)
    (cons x (foldr append '() ys)))
  (fold-tree tree f))

(define (fdupag-height-2 tree)
  (define (f _ ys)
    (+ 1 (foldr max 0 ys)))
  (fold-tree tree f))

(define ctx (make-test-context))

(test-case ctx
           (assert-equal (sum-tree tree-s1) (fdupag-sum-tree-2 tree-s1))
           (assert-equal (dfs tree-s2) (fdupag-dfs-2 tree-s2))
           (assert-equal (height tree-s3) (fdupag-height-2 tree-s3))

           (assert-equal (sum-tree tree-1) (fdupag-sum-tree tree-1))
           (assert-equal (sum-tree tree-2) (fdupag-sum-tree tree-2))
           (assert-equal (sum-tree tree-3) (fdupag-sum-tree tree-3))
           (assert-equal (sum-tree tree-4) (fdupag-sum-tree tree-4))
           (assert-equal (sum-tree tree-5) (fdupag-sum-tree tree-5))
           (assert-equal (sum-tree tree-6) (fdupag-sum-tree tree-6))
           (assert-equal (sum-tree tree-s1) (fdupag-sum-tree tree-s1))
           (assert-equal (sum-tree tree-s2) (fdupag-sum-tree tree-s2))
           (assert-equal (sum-tree tree-s3) (fdupag-sum-tree tree-s3))
           (assert-equal (sum-tree tree-s4) (fdupag-sum-tree tree-s4))
           (assert-equal (sum-tree tree-s5) (fdupag-sum-tree tree-s5))

           (assert-equal (dfs tree-1) (fdupag-dfs tree-1))
           (assert-equal (dfs tree-2) (fdupag-dfs tree-2))
           (assert-equal (dfs tree-3) (fdupag-dfs tree-3))
           (assert-equal (dfs tree-4) (fdupag-dfs tree-4))
           (assert-equal (dfs tree-5) (fdupag-dfs tree-5))
           (assert-equal (dfs tree-6) (fdupag-dfs tree-6))
           (assert-equal (dfs tree-s1) (fdupag-dfs tree-s1))
           (assert-equal (dfs tree-s2) (fdupag-dfs tree-s2))
           (assert-equal (dfs tree-s3) (fdupag-dfs tree-s3))
           (assert-equal (dfs tree-s4) (fdupag-dfs tree-s4))
           (assert-equal (dfs tree-s5) (fdupag-dfs tree-s5))

           (assert-equal (height tree-1) (fdupag-height tree-1))
           (assert-equal (height tree-2) (fdupag-height tree-2))
           (assert-equal (height tree-3) (fdupag-height tree-3))
           (assert-equal (height tree-4) (fdupag-height tree-4))
           (assert-equal (height tree-5) (fdupag-height tree-5))
           (assert-equal (height tree-6) (fdupag-height tree-6))
           (assert-equal (height tree-s1) (fdupag-height tree-s1))
           (assert-equal (height tree-s2) (fdupag-height tree-s2))
           (assert-equal (height tree-s3) (fdupag-height tree-s3))
           (assert-equal (height tree-s4) (fdupag-height tree-s4))
           (assert-equal (height tree-s5) (fdupag-height tree-s5))
           )

(run-tests ctx)