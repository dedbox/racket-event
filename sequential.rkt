#lang racket/base

(require
 event/renames
 racket/contract/base
 racket/function
 (for-syntax racket/base
             syntax/parse))

(provide
 pure event-let event-let*
 (contract-out
  [return (-> any/c evt?)]
  [args (-> evt? ... evt?)]
  [args* (-> (listof evt?) evt?)]
  [fmap (-> procedure? evt? ... evt?)]
  [fmap* (->  procedure? (listof evt?) evt?)]
  [app (-> evt? evt? ... evt?)]
  [app* (-> evt? (listof evt?) evt?)]
  [bind (-> (unconstrained-domain-> evt?) evt? ... evt?)]
  [bind* (-> (unconstrained-domain-> evt?) (listof evt?) evt?)]
  [seq (-> evt? evt? ... evt?)]
  [seq0 (-> evt? evt? ... evt?)]
  [test (-> evt? evt? evt? evt?)]
  [series (-> evt? (-> any/c evt?) ... evt?)]
  [series* (-> evt? (listof (-> any/c evt?)) evt?)]
  [reduce (-> (unconstrained-domain-> evt?)
              (unconstrained-domain-> boolean?)
              any/c ... evt?)]
  [reduce* (-> (unconstrained-domain-> evt?)
               (unconstrained-domain-> boolean?)
               (listof any/c) evt?)]
  [loop (-> (unconstrained-domain-> evt?) any/c ... evt?)]
  [loop* (-> (unconstrained-domain-> evt?) (listof any/c) evt?)]))

;; racket/base

(define-syntax (event-let stx)
  (syntax-parse stx
    [(_ ([x V] ...) E ...) #'(bind (λ (x ...) (seq E ...)) V ...)]))

(define-syntax (event-let* stx)
  (syntax-parse stx
    [(_ () E ...+) #'(seq E ...)]
    [(_ ([x V] bs ...) E ...+) #'(bind (λ (x) (event-let* (bs ...) E ...)) V)]))

(define-syntax (event-cond stx)
  (syntax-parse stx
    #:datum-literals (=> else)
    [(_) #'(pure (void))]
    [(_ [else E ...+]) #'(seq E ...)]
    [(_ [T => F] clause ...)
     #'(bind (λ (t) (if t (app F (pure t)) (event-cond clause ...))) T)]
    [(_ [T E ...+] clause ...) #'(test T (seq E ...) (event-cond clause ...))]))

;; event/sequential

(define-syntax-rule (pure datum)
  (handle always (λ _ datum)))

(define (return v)
  (pure v))

(define (args . Es)
  (args* Es))

(define (args* Es)
  (if (null? Es)
      (pure (values))
      (replace (car Es)
               (λ v
                 (fmap*
                  (λ vs (apply values (append v vs)))
                  (cdr Es))))))

(define (fmap f . Es)
  (fmap* f Es))

(define (fmap* f Es)
  (handle (args* Es) f))

(define (app F . Es)
  (app* F Es))

(define (app* F Es)
  (replace F (λ (f) (fmap* f Es))))

(define (bind f . Es)
  (bind* f Es))

(define (bind* f Es)
  (replace (args* Es) f))

(define (seq E . Es)
  (if (null? Es) E (replace E (λ _ (apply seq Es)))))

(define (seq0 E . Es)
  (replace E (λ (v) (handle (args* Es) (λ _ v)))))

(define (test E1 E2 E3)
  (replace E1 (λ (v) (if v E2 E3))))

(define (series E . fs)
  (series* E fs))

(define (series* E fs)
  (if (null? fs)
      E
      (replace E (λ vs (series* (apply (car fs) vs) (cdr fs))))))

(define (reduce f check . vs)
  (reduce* f check vs))

(define (reduce* f check vs)
  (replace
   (apply f vs)
   (λ vs*
     (if (apply check (append vs vs*))
         (pure (apply values vs*))
         (reduce* f check vs*)))))

(define (loop f . vs)
  (loop* f vs))

(define (loop* f vs)
  (reduce* f (λ _ #f) vs))

;;; Unit Tests

(module+ test
  (require rackunit)

  (define id values)

  ;; doc examples

  (test-case
    "event-let"
    (check = (sync (event-let ([x (pure 1)] [y (pure 2)]) (pure (+ x y))))
           3))

  (test-case
    "event-let*"
    (check
     = (sync (event-let* ([x (pure 1)] [y (pure (+ x 2))]) (pure (+ x y))))
     4))

  (test-case
    "event-cond"
    (check-pred void? (sync (event-cond))))

  (test-case
    "pure"
    (define N 0)
    (define evt (pure (set! N (add1 N))))
    (sync evt)
    (sync evt)
    (check = N 2))

  (test-case
    "return"
    (define N 0)
    (define evt (return (set! N (add1 N))))
    (sync evt)
    (sync evt)
    (check = N 1))

  (test-case
    "args"
    (check equal?
           (sync (handle (args (pure 1) (pure 2) (pure 3)) list))
           '(1 2 3)))

  (test-case
    "fmap"
    (check = (sync (fmap + (pure 1) (pure 2) (pure 3))) 6))

  (test-case
    "app"
    (check = (sync (app (pure +) (pure 1) (pure 2) (pure 3))) 6))

  (test-case
    "bind"
    (check =
           (sync
            (bind
             (compose return +)
             (pure 1)
             (pure 2)
             (pure 3)))
           6))

  (test-case
    "seq"
    (check = (sync (seq (pure 1) (pure 2) (pure 3))) 3))

  (test-case
    "seq0"
    (check = (sync (seq0 (pure 1) (pure 2) (pure 3))) 1))

  (test-case
    "test"
    (check = (sync (test (pure #t) (pure 1) (pure 2))) 1)
    (check = (sync (test (pure #f) (pure 1) (pure 2))) 2))

  (test-case
    "series"
    (check =
           (sync
            (series
             (pure 1)
             (compose return (curry + 2))
             (compose return (curry * 3))))
           9))

  (test-case
    "reduce"
    (check =
           (sync
            (reduce
             (λ (x) (pure (add1 x)))
             (λ (x y) (>= y 10))
             0))
           10))

  (test-case
    "loop"
    (check =
           (with-handlers ([number? values])
             (sync (loop (λ (x)
                           (if (< x 10)
                               (pure (add1 x))
                               (raise x)))
                         0)))
           10))

  ;; monad laws

  (test-case
    "fmap id == id"
    (for ([v 10])
      (check
       = v
       (sync (fmap id (pure v)))
       (sync (id (pure v))))))

  (test-case
    "fmap (f . g) == fmap f . fmap g"
    (for ([v 10])
      (check
       = (+ 2 (* 3 v))
       (sync (fmap (compose (curry + 2) (curry * 3)) (pure v)))
       (sync ((compose (λ (x) (fmap (curry + 2) x))
                       (λ (x) (fmap (curry * 3) x)))
              (pure v))))))

  (test-case
    "pure id <*> v = v"
    (for ([v 10])
      (check
       = v
       (sync (app (pure id) (pure v)))
       (sync (pure v)))))

  (test-case
    "pure (.) <*> u <*> v <*> w = u <*> (v <*> w)"
    (define u (pure (curry + 2)))
    (define v (pure (curry * 3)))
    (define w (pure (curry - 1)))
    (for ([i 10])
      (check
       = (+ 2 (* 3 (- 1 i)))
       (sync (app (app (pure compose) u v w) (pure i)))
       (sync (app u (app v (app w (pure i)))))
       )))

  (test-case
    "pure f <*> pure x = pure (f x)"
    (define f add1)
    (for ([x 10])
      (check
       = (add1 x)
       (sync (app (pure f) (pure x)))
       (sync (pure (f x))))))

  (test-case
    "u <*> pure y = pure ($ y) <*> u"
    (define u (pure (curry + 2)))
    (for ([y 10])
      (check
       = (+ 2 y)
       (sync (app u (pure y)))
       (sync (app (pure (λ (f) (f y))) u)))))

  (test-case
    "return a >>= k  =  k a"
    (define k (λ (x) (pure (add1 x))))
    (for ([a 10])
      (check
       = (add1 a)
       (sync (bind k (return a)))
       (sync (k a)))))

  (test-case
    "m >>= return  =  m"
    (for ([m 10])
      (check
       = m
       (sync (bind return (pure m)))
       (sync (pure m)))))

  (test-case
    "m >>= (\\x -> k x >>= h)  =  (m >>= k) >>= h"
    (define h (λ (x) (pure (+ 2 x))))
    (define k (λ (x) (pure (* 3 x))))
    (for ([m 10])
      (check
       = (+ 2 (* 3 m))
       (sync (bind (λ (x) (bind h (k x))) (pure m)))
       (sync (bind h (bind k (pure m)))))))

  (test-case
    "seq"
    (for ([v 10])
      (check
       = v
       (sync (seq (pure -1) (pure -2) (pure v))))))

  (test-case
    "seq0"
    (for ([v 10])
      (check
       = v
       (sync (seq0 (pure v) (pure -2) (pure -3))))))

  (test-case
    "test"
    (check = 1 (sync (test (pure #t) (pure 1) (pure 2))))
    (check = 2 (sync (test (pure #f) (pure 1) (pure 2)))))

  (test-case
    "series"
    (check
     = 5
     (sync
      (apply series (pure 0)
             (for/list ([_ 5]) (λ (x) (pure (+ x 1))))))))

  (test-case
    "reduce"
    (check
     = 10
     (sync
      (reduce
       (λ (x) (pure (add1 x)))
       (λ (x y) (>= y 10))
       0))))

  (test-case
    "loop"
    (check
     = 10
     (with-handlers ([number? values])
       (sync
        (loop
         (λ (x)
           (if (< x 10)
               (pure (add1 x))
               (raise x)))
         0))))))
