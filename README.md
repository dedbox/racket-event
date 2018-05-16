# Event-lang
**Synchronizable Event Programming**
[![Racket Package](https://img.shields.io/badge/raco%20pkg-event--lang-red.svg)](https://pkgd.racket-lang.org/pkgn/package/event-lang)
[![Documentation](https://img.shields.io/badge/read-docs-blue.svg)](http://docs.racket-lang.org/event-lang/)
[![Build Status](https://travis-ci.org/dedbox/racket-event-lang.svg?branch=master)](https://travis-ci.org/dedbox/racket-event-lang)
[![Coverage Status](https://coveralls.io/repos/github/dedbox/racket-event-lang/badge.svg?branch=master)](https://coveralls.io/github/dedbox/racket-event-lang?branch=master)

Event-lang is a Racket library that simplifies the creation of complex
synchronizable events. It provides a primitive expression-lifting form, some
event combinators, and a collection of functions that make progress by
synchronizing sub-events, either concurrently or in a predictable sequence.
The library also provides event-friendly alternatives to the functions and
special forms of the base Racket library.

```
> (sync
   (event-let ([x (pure 1)]
               [y (pure 2)]
               [z (pure 3)])
     (pure (list x y z))))
6
```
