#lang racket/base

(require
 event/concurrent
 event/sequential
 racket/contract/base
 racket/function
 racket/list
 (for-syntax racket/base
             syntax/parse))

(provide
 event-let event-let* event-cond
 (contract-out
  [event-list (-> evt? ... evt?)]
  [event-list* (-> evt? ... (listof evt?) evt?)]
  [event-map (-> procedure? (listof evt?) ... evt?)])
 async-let
 (contract-out
  [async-list (-> evt? ... evt?)]
  [async-list* (-> evt? ... (listof evt?) evt?)]
  [async-map (-> procedure? (listof evt?) ... evt?)]
  [async-void (-> evt? ... evt?)]
  [async-void* (-> evt? ... (listof evt?) evt?)]))

;;; Syntactic Forms

(define-syntax (event-let stx)
  (syntax-parse stx
    [(_ ([x V] ...) E ...) #'(bind V ... (λ (x ...) (seq E ...)))]))

(define-syntax (event-let* stx)
  (syntax-parse stx
    [(_ () E ...+) #'(seq E ...)]
    [(_ ([x V] bs ...) E ...+) #'(bind V (λ (x) (event-let* (bs ...) E ...)))]))

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

(define (rest-args Es+Es*)
  (append (drop-right Es+Es* 1) (last Es+Es*)))

;;; Pairs and Lists

;; Pairs Constructors and Selectors

(define event-pair? (curry fmap pair?))
(define event-null? (curry fmap null?))
(define event-cons (curry fmap cons))
(define event-car (curry fmap car))
(define event-cdr (curry fmap cdr))
(define event-null (pure null))
(define event-list? (curry fmap list?))
(define event-list (curry fmap list))
(define event-list* (curry fmap list*))

(define (event-map f . Ess)
  (fmap* (curry map f) (map event-list* Ess)))

;; Concurrent

(define-syntax (async-let stx)
  (syntax-parse stx
    [(_ ([x:id V] ...) E ...+)
     #'(async-bind V ... (λ (x ...) (seq E ...)))]))

(define (async-list . Es)
  (async-list* Es))

(define (async-list* . Es+Es*)
  (fmap list (async-args* (rest-args Es+Es*))))

(define (async-map f . Ess)
  (async-fmap* (curry map f) (map async-list* Ess)))

(define (async-void . Es)
  (async-fmap* void Es))

(define (async-void* . Es+Es*)
  (async-fmap* void (rest-args Es+Es*)))

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

  ;; Pairs and Lists

  (test-case "event-pair?"
    (check-true (sync (event-pair? (pure (cons 1 2)))))
    (check-false (sync (event-pair? (pure null))))
    (check-false (sync (event-pair? (pure 3)))))

  (test-case "event-null?"
    (check-true (sync (event-null? event-null)))
    (check-true (sync (event-null? (pure null))))
    (check-false (sync (event-null? (pure (cons 1 2)))))
    (check-false (sync (event-null? (pure 3)))))

  (test-case "event-cons"
    (check equal? (sync (event-cons (pure 1) (pure 2))) (cons 1 2)))

  (test-case "event-car"
    (check = (sync (event-car (pure '(1)))) 1))

  (test-case "event-cdr"
    (check-pred null? (sync (event-cdr (pure '(1))))))

  (test-case "event-null"
    (check-pred null? (sync event-null)))

  (test-case "event-list?"
    (check-true (sync (event-list? event-null)))
    (check-true (sync (event-list? (pure null))))
    (check-true (sync (event-list? (pure '(1)))))
    (check-false (sync (event-list? (pure (cons 1 2)))))
    (check-false (sync (event-list? (pure 3)))))

  (test-case "event-list"
    (check-pred null? (sync (event-list)))
    (check equal? (sync (event-list (pure 1))) '(1))
    (check equal? (sync (event-list (pure 1) (pure 2))) '(1 2)))

  (test-case "event-list*"
    (check-pred null? (sync (event-list* event-null)))
    (check equal? (sync (event-list* (pure '(1 2))))
           '(1 2))
    (check equal? (sync (event-list* (pure 1) (pure 2) (pure '(3 4))))
           '(1 2 3 4)))

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
