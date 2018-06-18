#lang racket/base

(require
 event/concurrent
 event/racket/async-pair
 event/racket/pair
 event/racket/void
 event/sequential
 racket/contract/base
 racket/function
 racket/list
 (for-syntax racket/base
             syntax/parse))

(provide
 (all-from-out event/racket/async-pair
               event/racket/pair
               event/racket/void)
 event-let event-let* event-let*-values event-cond
 async-let)

;;; Syntactic Forms

(define-syntax (event-let stx)
  (syntax-parse stx
    [(_ ([x V] ...) E ...+) #'(bind V ... (λ (x ...) (seq E ...)))]))

(define-syntax (event-let* stx)
  (syntax-parse stx
    [(_ () E ...+) #'(seq E ...)]
    [(_ ([x V] bs ...) E ...+) #'(bind V (λ (x) (event-let* (bs ...) E ...)))]))

(define-syntax (event-let*-values stx)
  (syntax-parse stx
    [(_ () E ...+) #'(seq E ...)]
    [(_ ([xs Vs] bs ...) E ...+)
     #'(bind Vs (λ xs (event-let*-values (bs ...) E ...)))]))

(define-syntax (event-cond stx)
  (syntax-parse stx
    #:literals (=> else)
    [(_) #'(pure (void))]
    [(_ [else E ...+]) #'(seq E ...)]
    [(_ [T => F] clause ...)
     #'(bind T (λ (v) (if v (app F (pure v)) (event-cond clause ...))))]
    [(_ [T E ...+] clause ...)
     #'(test T (seq E ...) (event-cond clause ...))]
    [(_ [E]) #'E]))

;; Concurrent

(define-syntax (async-let stx)
  (syntax-parse stx
    [(_ ([x:id V] ...) E ...+)
     #'(async-bind V ... (λ (x ...) (seq E ...)))]))

;;; Unit Tests

(module+ test
  (require rackunit)

  ;; Sequential

  (test-case "event-let"
    (check = (sync (event-let ([x (pure 1)] [y (pure 2)]) (pure (+ x y))))
           3))

  (test-case "event-let*"
    (check
     = (sync (event-let* ([x (pure 1)] [y (pure (+ x 2))]) (pure (+ x y))))
     4))

  (test-case "event-let*-values"
    (check
     = (sync (event-let*-values ([(x y) (pure (values 1 2))]) (pure (+ x y))))
     3))

  (test-case "event-cond"
    (check-pred void? (sync (event-cond)))
    (check = (sync (event-cond [else (pure 1) (pure 2)])) 2)
    (check = (sync (event-cond [(pure #t) (pure 1)] [(pure #t) (pure 2)])) 1)
    (check = (sync (event-cond [(pure #t) (pure 1)] [(pure #f) (pure 2)])) 1)
    (check = (sync (event-cond [(pure #f) (pure 1)] [(pure #t) (pure 2)])) 2)
    (check-pred void? (sync (event-cond [(pure #f) (pure 1)]
                                        [(pure #f) (pure 2)])))
    (check = (sync (event-cond [(pure #t) => (pure (λ (v) (if v 2 3)))])) 2)
    (check = (sync (event-cond [(pure 1)])) 1))

  ;; Concurrent

  (define-syntax-rule (async-test-case str L reset push body ...)
    (test-case
        str
      (define L null)
      (define (reset) (set! L null))
      (define (push x) (set! L (cons x L)))
      body ...))

  (async-test-case
   "async-let"
   L reset push
   (let loop ()
     (reset)
     (check
      = 3
      (sync (async-let
             ([x (seq (pure (push 0)) (pure 0))]
              [y (seq (pure (push 1)) (pure 1))]
              [z (seq (pure (push 2)) (pure 2))])
             (pure (+ x y z)))))
     (check = (length L) 3)
     (for ([j 3])
       (check-pred (curry member j) L))
     (when (equal? L '(0 1 2))
       (loop)))))
