#lang racket/base

(provide (all-defined-out))

(require
 event/renames
 racket/function
 racket/list)

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
