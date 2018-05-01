#lang racket/base

(require
 event/monad
 event/async-monad
 event/expand
 event/reduce
 event/final
 racket/pretty
 (for-syntax racket/base
             racket/pretty
             syntax/parse))

(provide
 event event-do event-print event-debug
 (all-from-out event/monad
               event/async-monad))

(define-for-syntax (make-event stx-0 debugging?)
  (define stx-1 (syntax-parse stx-0 [e:event0 #'e.expand]))
  (define stx-2 (syntax-parse stx-1 [e:event1 #'e.reduce]))
  (define stx-3 (syntax-parse stx-2 [e:event2 #'e.final]))
  (if debugging? (values stx-0 stx-1 stx-2 stx-3) stx-3))

(define-syntax (event stx)
  (syntax-parse stx
    [(_ e ...+) (make-event #'(begin e ...) #f)]))

(define-syntax (event-do stx)
  (syntax-parse stx
    [(_ e ...+) #`(sync #,(make-event #'(begin e ...) #f))]))

(define-syntax (event-print stx)
  (syntax-parse stx
    [(_ e ...+)
     (pretty-write (syntax->datum (make-event #'(begin e ...) #f)))
     #'(values)]))

(define-syntax (event-debug stx)
  (syntax-parse stx
    [(_ e ...+)
     (define-values (stx-0 stx-1 stx-2 stx-3) (make-event #'(begin e ...) #t))
     (displayln "-- INPUT --\n") (pretty-write (syntax->datum stx-0))
     (displayln "\n-- EXPAND --\n") (pretty-write (syntax->datum stx-1))
     (displayln "\n-- REDUCE --\n") (pretty-write (syntax->datum stx-2))
     (displayln "\n-- FINAL --\n") (pretty-write (syntax->datum stx-3))
     (displayln "\n-- RESULT --\n") #`(pretty-write (sync #,stx-3))]))
