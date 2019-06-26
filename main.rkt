#lang algebraic/racket/base

(require algebraic/data/event
         algebraic/data/values
         racket/contract/base
         (for-syntax syntax/parse
                     syntax/strip-context))

(provide
 event become
 (contract-out
  ;; Sequential Event Combinators
  [list-evt (-> evt? ... evt?)]
  [id-evt (-> evt? ... evt?)]
  [void-evt (-> evt? ... evt?)]
  [test-evt (-> evt? evt? evt? evt?)]
  [series-evt (-> evt? procedure? ... evt?)]
  [reduce-evt (-> procedure? (unconstrained-domain-> boolean?) any/c ... any)]
  [memoize-evt (-> evt? evt?)]
  ;; Concurrent Event Combinators
  [list-set-evt (-> evt? ... evt?)]
  [id-set-evt (-> evt? ... evt?)]
  [async-list-evt (-> evt? ... evt?)]
  [async-id-evt (-> evt? ... evt?)]
  [async-void-evt (-> evt? ... evt?)]
  [promise-evt (-> evt? evt?)]
  [promises-evt (-> evt? ... evt?)]))

(instantiate EventApplicative)
(instantiate values- ValuesMonad)

(define-syntax event (μ* (body ...+) (handle-evt always-evt (λ _ body ...))))

(define-syntax become
  (μ* (body ...+) #,(replace-context this-syntax #'(join (event body ...)))))

;;; ----------------------------------------------------------------------------
;;; Sequential Event Combinators

(define (list-evt . evts)
  (foldr (λ (head-evt tail-evt)
           (do xs <- head-evt
               (ys) <- tail-evt
               (return (++ xs ys))))
         (return null)
         evts))

(define (id-evt . evts)
  (do (xs) <- ($ list-evt evts)
      (event ($ id xs))))

(define (void-evt . evts)
  (fmap void ($ list-evt evts)))

(define (test-evt test-evt then-evt else-evt)
  (do xs <- test-evt
      (if (andmap id xs) then-evt else-evt)))

(define (series-evt init-evt . fs)
  (foldl =<< init-evt fs))

(define (reduce-evt f check . xs)
  (do ys <- ($ f xs)
      (if ($ check (++ xs ys))
          ($ return ys)
          ($ reduce-evt f check ys))))

(define (memoize-evt evt)
  (define result #f)
  (define (save xs)
    (set! result (event ($ id xs))))
  (become (or result (>>= evt (λ xs (save xs) result)))))

;;; ----------------------------------------------------------------------------
;;; Concurrent Event Combinators 

(define list-set-evt
  (function*
    [() (return null)]
    [evts (do let identify (φ evt (fmap (>> list evt) evt))
              ((evt . xs)) <- ($ choice-evt (map identify evts))
              (ys) <- ($ list-set-evt (remq evt evts))
              (return (++ xs ys)))]))

(define (id-set-evt . evts)
  (do (xs) <- ($ list-set-evt evts)
      (event ($ id xs))))

(define (async-list-evt . evts)
  (do (xss) <- ($ list-set-evt
                  (map (>> liftA2 list) (build-list (length evts) pure) evts))
      (return ($ ++ (map cdr (sort xss < #:key car))))))

(define (async-id-evt . evts)
  (do (xs) <- ($ async-list-evt evts)
      (event ($ id xs))))

(define (async-void-evt . evts)
  (fmap void ($ async-list-evt evts)))

(define (promise-evt evt)
  (define ch (make-channel))
  (memoize-evt
   (<* ch (thread (λ () (values-fmap (>> channel-put ch) (λ () (sync evt))))))))

(define (promises-evt . evts)
  ($ async-list-evt (map promise-evt evts)))

;;; ----------------------------------------------------------------------------

(module+ test
  (require rackunit
           racket/list)

  (define (check-event evt want)
    (check equal? (values-> list (sync evt)) want))

  (test-case "event"
    (check-event (event 0) '(0))
    (check-event (event (id 1 2 3)) '(1 2 3)))

  (test-case "list-evt"
    (check-event (list-evt) '(()))
    (check-event (list-evt (event 1)) '((1)))
    (check-event (list-evt (event 1) (event 2)) '((1 2)))
    (check-event (list-evt (event 1) (event 2) (event 3)) '((1 2 3))))

  (test-case "id-evt"
    (check-event (id-evt) '())
    (check-event (id-evt (event 1)) '(1))
    (check-event (id-evt (event 1) (event 2)) '(1 2))
    (check-event (id-evt (event 1) (event 2) (event 3)) '(1 2 3)))

  (test-case "test-evt"
    (check-event (test-evt (event #t) (event 1) (event 2)) '(1))
    (check-event (test-evt (event #f) (event 1) (event 2)) '(2)))

  (test-case "series-evt"
    (with-instance EventMonad
      (check-event
       (series-evt (event 1) (.. return (>> + 2)) (.. return (>> * 3))) '(9))))

  (test-case "reduce-evt"
    (with-instance EventApplicative
      (check-event (reduce-evt (.. pure add1) (<< >= 10) 0) '(10))))

  (test-case "memoize-evt"
    (define L null)
    (define (push v)
      (set! L (:: v L)))
    (define evt1 (event (begin (push 'A) 1)))
    (check = (sync evt1) 1) (check equal? L '(A))
    (check = (sync evt1) 1) (check equal? L '(A A))
    (define evt2 (memoize-evt evt1))
    (check = (sync evt2) 1) (check equal? L '(A A A))
    (check = (sync evt2) 1) (check equal? L '(A A A))
    (check = (sync evt2) 1) (check equal? L '(A A A)))

  (test-case "promise-evt"
    (define L null)
    (define (push v)
      (set! L (:: v L)))
    (define ch (make-channel))
    (define ps (for/list ([_ 10]) (promise-evt (*> (event (push 'A)) ch))))
    (for ([i 10]) (channel-put ch i))
    (for ([p ps])
      (define j (sync p))
      (check >= j 0)
      (check <= j 9)
      (check = (sync p) j))
    (check equal? L '(A A A A A A A A A A)))

  (test-case "list-set-evt"
    (for ([k 10])
      (define xs (build-list k id))
      (define evts (map pure xs))
      (let loop ([try 1])
        (when (> try 10) (fail-check "too many tries"))
        (define ys (sync ($ list-set-evt evts)))
        (check = (length ys) k)
        (for ([j k]) (check-pred (>> member j) ys))
        (when (and (> k 1) (equal? ys xs))
          (loop (add1 try))))
      (define threads (for/list ([_ 100]) (thread (λ () (sleep 0.1)))))
      (or (sync/timeout 0.5 (fmap void ($ list-set-evt threads)))
          (fail-check "time out"))))

  (test-case "id-set-evt"
    (for ([k 10])
      (define xs (build-list k id))
      (define evts (map pure xs))
      (let loop ([try 1])
        (when (> try 10) (fail-check "too many tries"))
        (define ys (values-> list (sync ($ id-set-evt evts))))
        (check = (length ys) k)
        (for ([j k]) (check-pred (>> member j) ys))
        (when (and (> k 1) (equal? ys xs))
          (loop (add1 try)))))
    (define threads (for/list ([_ 100]) (thread (λ () (sleep 0.1)))))
    (or (sync/timeout 0.5 (fmap void ($ id-set-evt threads)))
        (fail-check "time out")))

  (test-case "async-list-evt"
    (for ([_ 10]) (check-event (async-list-evt) '(())))
    (for ([_ 10]) (check-event (async-list-evt (event 1)) '((1))))
    (for ([_ 10]) (check-event (async-list-evt (event 1) (event 2)) '((1 2))))
    (for ([_ 10]) (check-event (async-list-evt (event 1) (event 2) (event 3)) '((1 2 3)))))

  (test-case "async-id-evt"
    (for ([_ 10]) (check-event (async-id-evt) '()))
    (for ([_ 10]) (check-event (async-id-evt (event 1)) '(1)))
    (for ([_ 10]) (check-event (async-id-evt (event 1) (event 2)) '(1 2)))
    (for ([_ 10]) (check-event (async-id-evt (event 1) (event 2) (event 3)) '(1 2 3))))

  (test-case "promises-evt"
    (define ch (make-channel))
    (define ps ($ promises-evt (make-list 10 ch)))
    (for ([i 10]) (channel-put ch i))
    (define vs (sync ps))
    (check = (length vs) 10)
    (for ([v vs])
      (check >= v 0)
      (check <= v 9))
    (check equal? (sync ps) vs)))
