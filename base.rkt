#lang racket/base

(provide (all-defined-out))

(require
 event/concurrent
 event/sequential)

(define (async-list . Es)
  (fmap list (async-set* Es)))
