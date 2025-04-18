#lang racket

(provide test-context make-test-context test-case assert-equal run-tests bundle-test subtask)

(struct test-context (pass-count total-tests) #:mutable)

(define (make-test-context)
  (test-context 0 0))

(define-syntax (test-case stx)
  (syntax-case stx ()
    [(_ ctx body ...)
     #`(begin
         #,@(for/list ([expr (syntax->list #'(body ...))])
              #`(begin
                  (set-test-context-total-tests! ctx (add1 (test-context-total-tests ctx)))
                  (with-handlers ([exn:fail? (lambda (e)
                                               (printf "~a\n" (exn-message e)))])
                    #,expr
                    (set-test-context-pass-count! ctx (add1 (test-context-pass-count ctx)))))))]))

(define-syntax (assert-equal stx)
  (syntax-case stx ()
    [(_ actual expected)
     #'(let ([ex expected]
             [ac actual])
         (unless (equal? ex ac)
           (error (format "Assertion failed: ~a\nExpected: ~s\nActual: ~s"
                          '(assert-equal expected actual) ex ac))))]))

(struct subtask (act exp))

(define (bundle-test checks)
  (define (check cs)
    (match cs
      [(list) #t]
      [(cons (subtask x y) lst)
       (begin
         (if (equal? x y)
             (check lst)
             (error (format "Assertion failed: ~a\nExpected: ~s\nActual: ~s"
                            '(assert-equal expected actual) y x)))
         )]))
  (check checks))

(define (run-tests ctx)
  (printf "\nscore=~a/~a\n"
          (test-context-pass-count ctx)
          (test-context-total-tests ctx)))