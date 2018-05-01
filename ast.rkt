#lang racket/base

(provide
 (for-syntax event-literals
             event-literal-ids))

(require
 (for-syntax racket/base
             syntax/parse))

(define-syntax-rule (define-literals (literals literal-ids) id ...)
  (begin
    (provide id ...)
    (define-syntax (id stx)
      (raise-syntax-error #f "cannot be evaluated" stx))
    ...
    (begin-for-syntax
     (define-literal-set literals (id ...))
     (define literal-ids (list #'id ...)))))

(define-literals (event-literals event-literal-ids)
  #%event-esc
  #%event-pure
  #%event-app
  #%event-begin
  #%event-begin0
  #%event-let-values
  #%event-lambda
  #%event-case-lambda
  #%event-if
  #%event-curry)
