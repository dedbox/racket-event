#lang algebraic/racket/base

(require algebraic/class/applicative
         algebraic/class/functor
         algebraic/class/monad
         algebraic/data/event
         event
         racket/contract/base)

(provide
 (contract-out
  [gate? predicate/c]
  [make-gate (-> gate?)]
  [open-gate (-> gate? evt?)]
  [gate-open? predicate/c]
  [gated (-> gate? evt? evt?)]))

(instantiate EventApplicative)

(struct gate (thread)
  #:property prop:evt (φ G (void-evt (gate-thread G))))

(define (make-gate)
  (gate (thread (λ () (sync never-evt)))))

(define (open-gate G)
  (do (event (kill-thread (gate-thread G)))
      (void-evt (gate-thread G))))

(define (gate-open? G)
  (thread-dead? (gate-thread G)))

(define gated (flip <*))

;;; Unit Tests

(module+ test
  (require rackunit)

  (test-case "gate"
    (define G (make-gate))
    (check-pred gate? G)
    (check-false (gate-open? G))
    (define threads (for/list ([_ 10]) (thread (λ () (sync G)))))
    (for-each (.. check-false thread-dead?) threads)
    (sync (open-gate G))
    (check-pred gate-open? G)
    (sync G)
    (for-each sync threads))

  (test-case "gated"
    (define G (make-gate))
    (define t (thread (λ () (check = (sync (gated G (pure 123))) 123))))
    (sync (open-gate G))
    (void (sync t))))
