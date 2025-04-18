#lang racket

(require "m-eval.rkt")

(define init-env (generate-init-env))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define (should-display? string)
  (match string
    ['DEFINE #f]
    [(== (void)) #f]
    [_ #t]))
(define (driver-loop) (repl #f))

(define (repl port)
  (if port #f (prompt-for-input input-prompt))
  (let ((input (if port (read port) (read))))
    (cond ((eof-object? input)   'meval-done)
          ((eq? input '**quit**) 'meval-done)
          (else
           (let ((output (m-eval init-env input)))
             (if (should-display? output)
                 (if port
                     (pretty-display output)
                     (begin
                       (announce-output output-prompt)
                       (pretty-display output)))
                 #f)
             (repl port))))))

(define (m-eval-file file-name)
  (let ((stream (open-input-file file-name)))
    (read-line stream) ;; strip off "#lang racket" line
    (repl stream)))    ;; feed the rest of the definitions into m-eval

(define (main)
  (let ((args (vector->list (current-command-line-arguments))))
    (define (run-files files)
      (cond ((null? files) 'done)
            (else (display  (format "> evaluating ~a ...\n" (car files)))
                  (m-eval-file (car files))
                  (newline)
                  (run-files (cdr files)))))
    (if (null? args)
        (driver-loop)
        (run-files args))
    (newline)))

(main)