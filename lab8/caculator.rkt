#lang racket

(struct m-env (table) #:mutable)
(define (empty-env) (m-env (hash)))

(define (set-env! env1 env2)
  (set-m-env-table! env1 (m-env-table env2)))

(define (env-extend env var val)
  (m-env (hash-set (m-env-table env) var val)))

(define (env-extend-multi env vars vals)
  (if (empty? vars)
      env
      (env-extend-multi (env-extend env (car vars) (car vals))
                        (cdr cars)
                        (cdr vals))))

(define (env-lookup env var)
  (define tab (m-env-table env))
  (if (hash-has-key? tab var)
      (hash-ref tab var)
      (error "Unkown binding -- ENV-LOOKUP" var)))



(define variable? symbol?)

; self-evaluated
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

; booleans
(define booleans (list (cons 'true true)
                       (cons 'false false)))
(define (false? x) (equal? x 'false))
(define (true? x) (equal? x 'true))

; primitive procedures
(struct primitive-procedure (proc))
(define primitive-procedures
  (list (cons 'car car)
        (cons 'cdr cdr)
        (cons 'cons cons)
        (cons 'null? null?)
        (cons 'list list)
        (cons '+ +)
        (cons '- -)
        (cons '* *)
        (cons '/ /)
        (cons '= =)
        (cons '> >)
        (cons '>= >=)
        (cons '< <)
        (cons '<= <=)
        (cons 'not not)
        (cons 'eq? eq?)
        (cons 'equal? equal?)
        (cons 'eqv eqv?)
        (cons 'remainder remainder)
        (cons 'display display)
        (cons 'displayln displayln)
        (cons 'newline newline)
        (cons 'error error)
        (cons 'format format)
        (cons 'exit exit)
        ;; you can add more primitive procedures here
        ))

; (user-defined) procedure
; (lambda (<para_1> <para_2> ...) <body>)

(struct user-defined-procedure (parameters body env))
(define (lambda-procedure? procedure)
  (equal? (car procedure) 'lambda))
(define (lambda-parameters procedure)
  (user-defined-procedure-parameters (cdr procedure)))
(define (lambda-body procedure)
  (user-defined-procedure-body (cdr procedure)))
(define (lambda-env procedure)
  (user-defined-procedure-env (cdr procedure)))

(define (eval-lambda procedure arguments)
  ;; (display (lambda-body procedure))
  ;; (display arguments)
  (m-eval (env-extend-multi (lambda-env procedure) (lambda-parameters procedure) arguments) (lambda-body procedure)))



; application
; (<procedure> <arg_1> <arg_2> ... <arg_n>)
;
; Interpret user-defined procedures and primitive procedures differently
(define application? pair?)
(define operator car)
(define operands cdr)
(define (list-of-values env exps)
  (map (lambda (exp) (m-eval env exp)) exps))
(define (m-apply env procedure arguments)
  (cond [(primitive-procedure? procedure)
         ;; (display "1")
         (apply (primitive-procedure-proc procedure) arguments)]
        [(lambda-procedure? procedure) (eval-lambda procedure arguments)] ; todo (lambda)
        [else (error "Unkown procedure type -- APPLY" procedure)]))

; begin
; (begin <act_1> <act_2> ... <act_n>)
(define (make-begin exps)
  (cons 'begin exps))
(define (last-exp? seq) (null? (cdr seq)))
(define first-exp car)
(define rest-exps cdr)
(define (eval-sequence env exps)
  (if (last-exp? exps)
      (m-eval env (first-exp exps))
      (begin (m-eval env (first-exp exps))
             (eval-sequence env (rest-exps exps)))))
(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))

; define
; (define <var> <expr>)
;
; For simplicity, we do not consider syntactic sugar
; like (define (<fun> <args...>) <fun-body>) here
(define (eval-define env var expr)
  (set-env! env (env-extend env
                            var
                            (m-eval env expr)))
  'DEFINE)

; if
; (if <cond-expr> <then-expr> <else-expr>)
; todo (if)

;思路非常简单，就是计算条件，真就计算前一条表达式，非就计算后一条表达式
(define (eval-if env cond-expr then-expr else-expr)
  (if (eval-condition env cond-expr)
      (m-eval env then-expr)
      (m-eval env else-expr)))

(define (eval-condition env cond-expr)
  (match cond-expr
    [(? false?) #f]
    [(? true?) #t]
    [(? self-evaluating?) #t]
    [(? variable?) (eval-condition env (env-lookup env cond-expr))]
    [(? application?) (m-eval env cond-expr)]
    [_ #f]))

(define (make-if cond-expr then-expr else-expr) (list 'if cond-expr then-expr else-expr))

; cond
; (cond [<predicate> <actions>]
;       [<predicate> <actions>]
;       ...
;       [else <actions>])
;
; For simplicity, you can assume the `else` clause is
; always the last clause
;
; HINT: You can interpret `cond` by expanding it to nested `if`

; todo (cond)

;对于SICP课本上的实现方法的复现，实际上就是将cond转换为if的嵌套
(define (cond-else-clause? clause)
  (equal? (car clause) 'else))

(define (cond->if exp) (expand-clauses exp))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ([first (car clauses)]
            [rest (cdr clauses)])
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cdr first))
                (error "ELSE clauses isn't last:cond->if"
                       clauses))
            (make-if (car first)
                     (sequence->exp (cdr first))
                     (expand-clauses rest))))))
                

; ==================================================
; meta-circular evaluator

; the initial environment
(define (generate-init-env)
  (define pp-env (for/fold ([current-env (empty-env)])
                           ([pair primitive-procedures])
                   (env-extend current-env
                               (car pair)
                               (primitive-procedure (cdr pair)))))
  (for/fold ([current-env pp-env])
            ([pair booleans])
    (env-extend current-env
                (car pair)
                (cdr pair))))

(define (m-eval env exp)
  (match exp
    ; primitives
    [(? self-evaluating?) exp]
    [(? variable?) (env-lookup env exp)]

    ; special forms
    [(list 'quote expr) expr]
    [(cons 'begin actions) (eval-sequence env actions)]
    [(list 'define var expr) (eval-define env var expr)]

    [(list 'if cond-expr then-expr else-expr) (eval-if env cond-expr then-expr else-expr)] ; todo (if)
    [(cons 'cond clauses) (m-eval env (cond->if clauses))] ; todo (cond)
    [(cons 'lambda (cons parameters body)) 
     (cons 'lambda (user-defined-procedure parameters (sequence->exp body) env))] ; todo (lambda)
    ; application
    [(? application?) 
     ;; (display exp) 
     (m-apply env (m-eval env (operator exp))
                               (list-of-values env (operands exp)))]
    [_ (error "Unkown expression type -- EVAL" exp)]))

(provide generate-init-env m-eval)
