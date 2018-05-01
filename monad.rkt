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

(define (args . Vs)
  (args* Vs))

(define (args* Vs)
  (if (null? Vs)
      (pure (values))
      (replace (car Vs)
               (λ v
                 (fmap*
                  (λ vs (apply values (append v vs)))
                  (cdr Vs))))))

(define (fmap f . Vs)
  (fmap* f Vs))

(define (fmap* f Vs)
  (handle (args* Vs) f))

(define (app F . Vs)
  (app* F Vs))

(define (app* F Vs)
  (replace F (λ (f) (fmap* f Vs))))

(define (bind f . Es)
  (bind* f Es))

(define (bind* f Es)
  (replace (args* Es) f))

(define (seq V . Vs)
  (if (null? Vs) V (replace V (λ _ (apply seq Vs)))))

(define (seq0 V . Vs)
  (replace V (λ (v) (handle (args* Vs) (λ _ v)))))

(define (test V1 V2 V3)
  (bind V1 (λ (v) (if v V2 V3))))

(define (series V . fs)
  (series* V fs))

(define (series* V fs)
  (if (null? fs)
      V
      (replace V (λ (v) (apply series ((car fs) v) (cdr fs))))))

(define (reduce f x check)
  (replace (f x) (λ (y) (if (check x y) (pure y) (reduce f y check)))))

(define (loop f x)
  (reduce f x (λ _ #f)))

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
       0
       (λ (_ y) (>= y 10))))))

  (test-case
    "loop"
    (check
     = 10
     (with-handlers ([number? values])
       (sync
        (loop (λ (x) (if (< x 10) (pure (add1 x)) (raise x))) 0))))))
