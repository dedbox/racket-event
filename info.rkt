#lang info

(define collection "event")

(define deps '("base"))

(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "sandbox-lib"
    "scribble-lib"))

(define scribblings '(("scribblings/event-lang.scrbl")))
