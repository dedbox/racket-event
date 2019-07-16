#lang racket/base

(require event
         racket/sandbox
         scribble/examples
         scribble/manual)

(provide (all-defined-out))

(random-seed 7)

(define (rtech . args)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

(define (gtech . args)
  (apply tech #:doc '(lib "scribblings/guide/guide.scrbl") args))

(define event-evaluator
  (call-with-trusted-sandbox-configuration
   (λ ()
     (parameterize ([sandbox-output       'string]
                    [sandbox-error-output 'string])
       (make-base-eval #:lang 'algebraic/racket/base '(void)))))) 

(define-syntax-rule (example expr ...)
  (examples #:eval event-evaluator #:label #f expr ...))
