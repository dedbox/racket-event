# Event-lang
**Synchronizable Event Programming**
[![Racket Package](https://img.shields.io/badge/raco%20pkg-event--lang-red.svg)](https://pkgd.racket-lang.org/pkgn/package/event-lang)
[![Documentation](https://img.shields.io/badge/read-docs-blue.svg)](http://docs.racket-lang.org/event-lang/)
[![Build Status](https://travis-ci.org/dedbox/racket-event-lang.svg?branch=master)](https://travis-ci.org/dedbox/racket-event-lang)
[![Coverage Status](https://coveralls.io/repos/github/dedbox/racket-event-lang/badge.svg?branch=master)](https://coveralls.io/github/dedbox/racket-event-lang?branch=master)

Event-lang is a Racket library that simplifies the creation of complex
synchronizable events. It provides a primitive expression-lifting form,

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

Composite events make progress by synchronizing sub-events, either
concurrently or in a predictable sequence.
