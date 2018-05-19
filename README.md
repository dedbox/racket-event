# Event-lang
**Synchronizable Event Programming**
[![Racket Package](https://img.shields.io/badge/raco%20pkg-event--lang-red.svg)](https://pkgd.racket-lang.org/pkgn/package/event-lang)
[![Documentation](https://img.shields.io/badge/read-docs-blue.svg)](http://docs.racket-lang.org/event-lang/)
[![Build Status](https://travis-ci.org/dedbox/racket-event-lang.svg?branch=master)](https://travis-ci.org/dedbox/racket-event-lang)
[![Coverage Status](https://coveralls.io/repos/github/dedbox/racket-event-lang/badge.svg?branch=master)](https://coveralls.io/github/dedbox/racket-event-lang?branch=master)

Event-lang is an experimental Racket library that simplifies the creation of
complex synchronizable events.

Event-lang provides a primitive expression lifting form,

```
> (pure 123)
#<evt>
```

some event combinators,

```
> (sync (fmap + (pure 1) (pure 2)))
3
> (sync (app (pure +) (pure 1) (pure 2)))
3
> (sync (bind (pure 1) (pure 2) (λ xs (pure (apply + xs)))))
3
```

and a collection of event-friendly alternatives to base Racket forms and
functions.

```
> (sync
   (event-let
    ([x (pure 1)]
     [y (pure 2)])
    (pure (list x y))))
'(1 2)
```

Composite events make progress by synchronizing constituent events, either
concurrently or in a predictable sequence. Synchronization results can be
ordered as specified,

```
> (let ([t0 (current-inexact-milliseconds)])
    (define (now) (- (current-inexact-milliseconds) t0))
    (sync
     (async-args
      (pure (cons 1 (now)))
      (pure (cons 2 (now)))
      (pure (cons 3 (now))))))
'(1 . 0.200927734375)
'(2 . 0.14990234375)
'(3 . 0.178955078125)
```

or as completed.

```
> (let ([t0 (current-inexact-milliseconds)])
    (define (now) (- (current-inexact-milliseconds) t0))
    (sync
     (async-set
      (pure (cons 1 (now)))
      (pure (cons 2 (now)))
      (pure (cons 3 (now))))))
'(2 . 0.0771484375)
'(3 . 0.093017578125)
'(1 . 0.123046875)
```

The project has three outstanding objectives:

1. _Provide a sophisticated lifting form_ to simplify usage of the provided
  constructs. The event/event module contains a first approximation. Its
  construction was tedious and error prone, so I commented out the docs.

2. _Provide a full-blown_ `#lang event/racket/base` for producing whole
  modules of events and event constructors from ordinary Racket code in a
  principled manner.

3. _Provide support for static analysis of synchronization behaviors._ Event
  programming in Racket is a curious form of meta-programming, and a few
  simple compile-time checks could reduce cognitive overhead.
