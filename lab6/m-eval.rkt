#lang racket

; ==================================================
; environment

; e.g.
; (define env1 (env-extend (env-extend (empty-env) 'x 3) 'y 4))
; (env-lookup env1 'x) ; 3
; (env-lookup env1 'y) ; 4
; (define env2 (env-extend env1 'x 5))
; (env-lookup env2 'x) ; 5
; (env-lookup env2 'y) ; 4
; (env-lookup env1 'x) ; 3
; (env-lookup env1 'y) ; 4

(struct m-env (table) #:mutable)
(define (empty-env) (m-env (hash)))

(define (set-env! env1 env2)
  (set-m-env-table! env1 (m-env-table env2)))

; Bind the value `val` to the variable `var`
; Note that this operation is functional and
; does not modify the original `env`.
(define (env-extend env var val)
  (m-env (hash-set (m-env-table env) var val)))

(define (env-extend-multi env vars vals)
  (if (empty? vars)
      env
      (env-extend-multi (env-extend env (car vars) (car vals))
                        (cdr vars)
                        (cdr vals))))

; Find the value of variable x in env.
; If it cannot be found, an exception will be thrown
(define (env-lookup env var)
  (define tab (m-env-table env))
  (if (hash-has-key? tab var)
      (hash-ref tab var)
      (error "Unkown binding -- ENV-LOOKUP" var)))

;; (define (substitute env var)
;;     (if (hash-has-key? tab var)
;;         (hash-ref tab var)
;;         var))
;;
;; (define (substitute-multiple env vars)
;;   (map (lambda (var) (substitute env var)) vars))

; ==================================================
; evaluator structures

; variable
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

; todo (lambda)
;自定义函数的解释完成过程是最困难的，一开始的思路是在函数被调用需要计算的时候让env扩展带上被传入的参数，这样在body呗传入m-eval
;时env中就会找到对应的值，同时全局env也不会被赋值导致副作用以至于后续的计算造成冲突。
;但是这种做法无法覆盖lambda中嵌套lambda的，情况（即返回值为一个函数的情况，因为原来的做法必须一次计算完成，
;嵌套递归的返回值必须是一个计算完成的值；而新的情况的返回值只是一个函数，如果返回的话，本来他所配套的呗扩展的env不会跟着返回，
;因此又回到了全局env，这是一个非常大的问题。
;初步解决思路是通过替换，即在计算对应返回函数的时候，直接把返回函数对印的变量替换为对应值，这样返回的时候就不用担心全局环境没
;有对应的变量值，但是替换的时机非常别扭，由于递归栈过深，很难操作。
;后续想到可以通过将自定义函数和env绑定的方法解决这个问题，即直接构造一个自定义函数的结构体，其中除了parameters和body还存放对应
;env，这样函数会直接储存定义时的env，对于返回值为函数的情况，函数返回的时候事实上就是这个函数被定义的时候，所以会储存对应的env
;这样在调用返回的函数就使用的时扩展后的env了。
;实际上就是一个env模型的简单实现，在求值时直接在这个解决方法非常符合直觉，且实现非常简单高效
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
