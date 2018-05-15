#lang racket/base

(provide (all-defined-out))

(require
 event/concurrent
 event/sequential
 racket/function
 racket/list)

(define (merge-args Es+Es*)
  (append (drop-right Es+Es* 1) (last Es+Es*)))

;; Sequential

(define (event-list . Es)
  (event-list* Es))

(define (event-list* . Es+Es*)
  (fmap list (args* (merge-args Es+Es*))))

(define (event-map f . Ess)
  (fmap* (curry map f) (map event-list* Ess)))

;; Concurrent

(define (async-list . Es)
  (async-list* Es))

(define (async-list* . Es+Es*)
  (fmap list (async-args* (merge-args Es+Es*))))

(define (async-map f . Ess)
  (async-fmap* (curry map f) (map async-list* Ess)))

(define (async-void . Es)
  (async-fmap* void Es))

(define (async-void* . Es+Es*)
  (async-fmap* void (merge-args Es+Es*)))
