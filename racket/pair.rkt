#lang racket/base

(require
 event/sequential
 racket/contract/base
 racket/function
 racket/list)

(provide
 (contract-out
  [event-pair? (-> evt? evt?)]
  [event-null? (-> evt? evt?)]
  [event-cons (-> evt? evt? evt?)]
  [event-car (-> evt? evt?)]
  [event-cdr (-> evt? evt?)]
  [event-null evt?]
  [event-list? (-> evt? evt?)]
  [event-list (-> evt? ... evt?)]
  [event-list* (-> evt? ... (listof evt?) evt?)]
  [event-map (-> procedure? (listof evt?) ... evt?)]))

;;; Pairs and Lists

;; Pair Constructors and Selectors

(define event-pair? (curry fmap pair?))
(define event-null? (curry fmap null?))
(define event-cons (curry fmap cons))
(define event-car (curry fmap car))
(define event-cdr (curry fmap cdr))
(define event-null (pure null))
(define event-list? (curry fmap list?))
(define event-list (curry fmap list))

(define (event-list* . args)
  (define vs (drop-right args 1))
  (define tail
    (let loop ([Es (last args)])
      (cond [(pair? Es) (fmap cons (car Es) (loop (cdr Es)))]
            [(null? Es) event-null]
            [else Es])))
  (fmap append (fmap* list vs) tail))

(define (event-map f . Ess)
  (fmap* (curry map f) (map event-list* Ess)))

;;; Unit Tests

(module+ test
  (require rackunit)

  ;; Pair Constructors and Selectors

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
    (check-pred null? (sync (event-list* null)))
    (check equal? (sync (event-list* (cons (pure 1) (pure 2))))
           '(1 . 2))
    (check equal? (sync (event-list* (list (pure 1) (pure 2))))
           '(1 2))
    (check equal? (sync (event-list* (pure 1) (cons (pure 2) (pure 3))))
           '(1 2 . 3))
    (check equal? (sync (event-list* (pure 1) (pure 2) (list (pure 3) (pure 4))))
           '(1 2 3 4)))

  (test-case "event-map"
    ))
