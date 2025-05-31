#lang racket

(require "util.rkt")

(struct env (str))

(define (initial-env) (env ""))

(define (handle-clear _)
  (initial-env))

(define (handle-append e args)
  (unless (and (= (length args) 1) (string? (car args)))
    (error "append needs 1 str"))
  (env (string-append (env-str e) (car args))))

(define (handle-repeat e args)
  (unless (and (= (length args) 1) (exact-positive-integer? (car args)))
    (error "repeat needs 1 postive integer"))
  (env (string-append* (make-list (car args) (env-str e)))))

(define (handle-reverse e)
  (env (list->string (reverse (string->list (env-str e))))))

(define (handle-print e)
  (displayln (env-str e))
  e)

(define (eval-expr e expr)
  (match expr
    [(list 'clear)        (handle-clear e)]
    [(list 'append arg)   (handle-append e (list arg))]
    [(list 'repeat n)     (handle-repeat e (list n))]
    [(list 'reverse)      (handle-reverse e)]
    [(list 'print)        (handle-print e)]
    [_ (error (format "error command: ~a" expr))]))

(define (eval-ast ast)
  (foldl (lambda (expr env) (eval-expr env expr))
         (initial-env)
         ast))

(printf "S-Exp read:\n")
(pretty-print ast) ; just for debug, you can delete these lines
(printf "\n")

(void (eval-ast ast))