#lang racket/base

(require
 event/sequential
 racket/contract/base)

(provide
 (contract-out
  [gate? predicate/c]
  [rename make-gate gate (-> gate?)]
  [open-gate (-> gate? void?)]
  [gate-open? predicate/c]))

(struct gate (thread) #:property prop:evt (λ (g) (fmap void (gate-thread g))))

(define (make-gate)
  (gate (thread (λ () (sync never-evt)))))

(define (open-gate g)
  (seq
   (pure (kill-thread (gate-thread g)))
   (fmap void (gate-thread g))))

(define (gate-open? g)
  (thread-dead? (gate-thread g)))

;;; Unit Tests

(module+ test
  (require rackunit)

  (test-case
    "gate"
    (define g (make-gate))
    (check-pred gate? g)
    (check-false (gate-open? g))
    (define ts (for/list ([_ 10]) (thread (λ () (sync g)))))
    (for-each (compose check-false thread-dead?) ts)
    (sync (open-gate g))
    (check-pred gate-open? g)
    (sync g)
    (for-each sync ts)))
