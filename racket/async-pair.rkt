#lang racket/base

(require
 event/concurrent
 event/sequential
 racket/contract/base
 racket/function
 racket/list)

(provide
 (contract-out
  [async-list (-> evt? ... evt?)]
  [async-list* (-> evt? ... (listof evt?) evt?)]
  [async-map (-> procedure? (listof evt?) ... evt?)]))

(define (rest-args Es+Es*)
  (append (drop-right Es+Es* 1) (last Es+Es*)))

(define (async-list . Es)
  (async-list* Es))

(define (async-list* . Es+Es*)
  (fmap list (async-args* (rest-args Es+Es*))))

(define (async-map f . Ess)
  (async-fmap* (curry map f) (map async-list* Ess)))

;;; Unit Tests

;; (module+ test
;;   (require (rackunit)))
