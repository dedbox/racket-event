#lang racket/base

(require
 event/concurrent
 event/sequential
 racket/contract/base
 racket/list)

(provide
 (contract-out
  [event-void (-> evt? ... evt?)]
  [event-void* (-> evt? ... (listof evt?) evt?)]
  [async-void (-> evt? ... evt?)]
  [async-void* (-> evt? ... (listof evt?) evt?)]))

(define (rest-args Es+Es*)
  (append (drop-right Es+Es* 1) (last Es+Es*)))

(define (event-void . Es)
  (fmap* void Es))

(define (event-void* . Es+Es*)
  (fmap* void (rest-args Es+Es*)))

(define (async-void . Es)
  (async-fmap* void Es))

(define (async-void* . Es+Es*)
  (async-fmap* void (rest-args Es+Es*)))
