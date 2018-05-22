#lang racket/base

(require
 event/concurrent
 racket/contract/base
 racket/list)

(provide
 (contract-out
  [async-void (-> evt? ... evt?)]
  [async-void* (-> evt? ... (listof evt?) evt?)]))

(define (rest-args Es+Es*)
  (append (drop-right Es+Es* 1) (last Es+Es*)))

(define (async-void . Es)
  (async-fmap* void Es))

(define (async-void* . Es+Es*)
  (async-fmap* void (rest-args Es+Es*)))
