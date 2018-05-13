#lang racket/base

(require
 event/renames
 event/sequential
 racket/contract/base
 racket/list
 (for-syntax racket/base
             syntax/parse))

(provide
 async-let
 (contract-out
  [async-set (-> evt? ... evt?)]
  [async-set* (-> (listof evt?) evt?)]
  [async-args (-> evt? ... evt?)]
  [async-args* (-> (listof evt?) evt?)]
  [async-fmap (-> procedure? evt? ... evt?)]
  [async-fmap* (-> procedure? (listof evt?) evt?)]
  [async-app (-> evt? evt? ... evt?)]
  [async-app* (-> evt? (listof evt?) evt?)]
  [async-bind (-> evt? ... (unconstrained-domain-> evt?) evt?)]
  [async-bind* (-> (listof evt?) (unconstrained-domain-> evt?) evt?)]))

(define-syntax (async-let stx)
  (syntax-parse stx
    [(_ ([x:id V] ...) E ...+)
     #'(async-bind V ... (λ (x ...) (seq E ...)))]))

(define (async-set . Es)
  (async-set* Es))

(define (async-set* Es)
  (define (one-of Es)
    (apply choice (map (λ (E) (handle E (λ (v) (cons E v)))) Es)))
  (let loop ([Es Es]
             [vs null])
    (if (null? Es)
        (pure (apply values (reverse vs)))
        (replace (one-of Es)
                 (λ (E+v)
                   (loop (remq (car E+v) Es)
                         (cons (cdr E+v) vs)))))))

(define (async-args . Es)
  (async-args* Es))

(define (async-args* Es)
  (define vs (make-vector (length Es) '?))
  (define Hs (make-vector (vector-length vs) #f))
  (for/list ([E Es]
             [k (vector-length vs)])
    (vector-set!
     Hs k
     (handle E (λ (v)
                 (vector-set! vs k v)
                 (vector-set! Hs k #f)))))
  (let loop ()
    (define evts (filter values (vector->list Hs)))
    (if (null? evts)
        (pure (apply values (vector->list vs)))
        (replace (apply choice evts) (λ _ (loop))))))

(define (async-fmap f . Es)
  (async-fmap* f Es))

(define (async-fmap* f Es)
  (handle (async-args* Es) f))

(define (async-app F . Es)
  (async-app* F Es))

(define (async-app* F Es)
  (replace F (λ (f) (async-fmap* f Es))))

(define (async-bind . Es+f)
  (async-bind* (drop-right Es+f 1) (last Es+f)))

(define (async-bind* Es f)
  (replace (async-args* Es) f))

;;; Unit Tests

(module+ test
  (require
   rackunit
   racket/function)

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
       (loop))))

  (test-case
   "async-set"
   (for ([k 10])
     (define xs (build-list k values))
     (define Xs (map return xs))
     (let loop ()
       (define ys (sync (handle (apply async-set Xs) list)))
       (check = (length ys) k)
       (for ([j k])
         (check-pred (curry member j) ys))
       (when (and (> k 1) (equal? ys xs))
         (loop)))))

  (define id values)

  (async-test-case
   "async-args"
   L reset push
   (let loop ()
     (reset)
     (sync
      (async-args
       (pure (push 1))
       (pure (push 2))
       (pure (push 3))))
     (check = (length L) 3)
     (check-pred (curry member 1) L)
     (check-pred (curry member 2) L)
     (check-pred (curry member 3) L)
     (when (equal? L '(1 2 3))
       (loop))))

  (test-case
    "async-fmap id == id"
    (for* ([k 10])
      (define Vs (build-list k return))
      (check
       equal? (build-list k values)
       (sync (handle (async-fmap* id Vs) list))
       (map sync Vs))))

  (test-case
    "async-fmap (f . g) == async-fmap f . async-fmap g"
    (for* ([k 10])
      (define f (curry * 2))
      (define g +)
      (define vs (build-list k values))
      (define Vs (map return vs))
      (check
       = (* 2 (apply + vs))
       (sync (async-fmap* (compose f g) Vs))
       (replace (async-fmap* g Vs)
                (λ (x) (async-fmap f (pure x)))))))

  (test-case
    "pure id <*> v = v"
    (for* ([k 10])
      (define vs (build-list k values))
      (define Vs (map return vs))
      (check equal? vs (sync (handle (async-app* (pure id) Vs) list)))))

  (test-case
    "pure (.) <*> u <*> v <*> w = u <*> (v <*> w)"
    (define u (pure (curryr - 2)))
    (define v (pure (curry * 3)))
    (define w (pure (curry + 1)))
    (for* ([k 10])
      (define xs (build-list k values))
      (define Xs (map return xs))
      (check
       = (- (* 3 (apply + 1 xs)) 2)
       (apply (sync (async-app (pure compose) u v w)) xs)
       (sync (async-app u (async-app v (async-app* w Xs)))))))

  (test-case
    "pure f <*> pure x = pure (f x)"
    (define f +)
    (for* ([k 10])
      (define xs (build-list k values))
      (define Xs (map return xs))
      (check
       = (apply + xs)
       (sync (async-app* (pure f) Xs))
       (sync (pure (apply f xs))))))

  (test-case
    "u <*> pure y = pure ($ y) <*> u"
    (define u (pure +))
    (for* ([k 10])
      (define ys (build-list k values))
      (define Ys (map return ys))
      (check
       = (apply + ys)
       (sync (async-app* u Ys))
       (sync (async-app (pure (curryr apply ys)) u)))))

  (test-case
    "return a >>= k  =  k a"
    (define k (λ xs (pure (apply + xs))))
    (for* ([x 10])
      (define as (build-list x values))
      (define As (map return as))
      (check
       = (apply + as)
       (sync (async-bind* As k))
       (sync (apply k as)))))

  (test-case
    "m >>= return  =  m"
    (for ([k 10])
      (define ms (build-list k values))
      (define Ms (map return ms))
      (check
       equal? ms
       (sync (async-bind* Ms (compose return list))))))

  (test-case
    "m >>= (\\x -> k x >>= h)  =  (m >>= k) >>= h"
    (define h (λ (x) (pure (* 2 x))))
    (define k (λ xs (pure (apply + xs))))
    (for ([x 10])
      (define ms (build-list x values))
      (define Ms (map return ms))
      (check
       = (* 2 (apply + ms))
       (sync (async-bind* Ms (λ ys (async-bind (apply k ys) h))))
       (sync (async-bind (async-bind* Ms k) h))))))
