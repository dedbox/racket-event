#lang scribble/manual

@title{Event-lang: Synchronizable Event Programming}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@(require
  racket/sandbox
  scribble/examples
  (for-label event
             racket/base
             racket/contract/base))

@(define (rtech . args)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

@(define event-evaluator
   (parameterize
       ([sandbox-output 'string]
        [sandbox-error-output 'string]
        [sandbox-memory-limit 50]
        [sandbox-eval-limits '(30 50)]
        [sandbox-make-inspector current-inspector])
     (make-evaluator 'racket #:requires '(event))))

@(define-syntax-rule (example expr ...)
   @examples[
     #:eval event-evaluator
     #:label #f
     expr ...
   ])

Event-lang is a DSL for specifying @rtech{synchronizable events} with ordinary
Racket syntax. For example,

@racketblock[(event (let ([x 3] [y 2]) (+ x y)))]

creates an event that binds @racket[x] and @racket[y] to numbers internally
and then uses the sum as its @rtech{synchronization result}.

The @racket[event] form recursively rewrites its input expression into a
@rtech{synchronizable event} that uses a primitive lifting form and a small
set of combinators. An equivalent event can always be constructed manually
with the provided combinators.

@racketblock[
  (bind (λ (x)
          (bind (λ (y) (pure (+ x y)))
                (return 2)))
        (return 3))
]

@section{Event Construction}

@defmodule[event]

All of the bindings defined in this manual are exported by the
@racketmodname[event] module.

@defform[(event expr ...+)]{

  Returns as @rtech{synchronizable event} that delays evaluation of
  @racket[#,(var expr) #,(var ...)] until a thread synchronizes on it. The
  @rtech{synchronization result} is the evaluation result.

  @example[
    (event (let ([x 3] [y 2]) (+ x y)))
    (sync (event (let ([x 3] [y 2]) (+ x y))))
  ]
}

@defform[(event-do expr ...+)]{

  Calls @racket[(event #,(var expr) #,(var ...))] and immediately synchronizes
  the generated event.

  @example[
    (event-do (let ([x 3] [y 2]) (+ x y)))
  ]
}

@defform[(event-print expr ...+)]{

  Prints the event generated by @racket[(event #,(var expr) #,(var ...))] in
  combinator form.

  @example[
    (event-print (let ([x 3] [y 2]) (+ x y)))
  ]
}

@defform[(event-debug expr...+)]{

  Prints the event generated by @racket[(event #,(var expr) #,(var ...))],
  including all intermediate representations. Synchronizes the generated event
  and prints the @rtech{synchronization result}.

  @example[
    (event-debug (let ([x 3] [y 2]) (+ x y)))
  ]
}

@section{Sequencing Combinators}

@defmodule[event/monad]

@defform[(pure datum)]{

  Lifts @var[datum] into a into a @rtech{synchronizable event}. Delays
  evaluation of @var[datum] until a thread synchronizes on it. The
  @rtech{synchronization result} is the evaluation result.

  @example[
    (define N 0)
    (define evt (pure (set! N (add1 N))))
    (sync evt)
    (sync evt)
    N
  ]
}

@defproc[(return [v any/c]) evt? #:value (pure v)]{

  Pre-evaluates an expression and then lifts the result @var[v] into an event.
  Returns a @rtech{synchronizable event} that does nothing and uses @var[v] as
  its @rtech{synchronization result}.

  @example[
    (define N 0)
    (define evt (return (set! N (add1 N))))
    (sync evt)
    (sync evt)
    N
  ]
}

@deftogether[(
  @defproc[(args [V evt?] ...) evt?]
  @defproc[(args* [Vs (listof evt?)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that evaluates @racket[#,(var V)
  #,(var ...)] or @var[Vs] in order and then applies @racket[values] to a list
  of the @rtech{synchronization results}.

  @example[
    (sync (args (pure 1) (pure 2) (pure 3)))
  ]
}

@deftogether[(
  @defproc[(fmap [f (-> any/c ... any)] [V evt?] ...) evt?]
  @defproc[(fmap* [f (-> any/c ... any)] [Vs (listof evt?)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that synchronizes @racket[#,(var V)
  #,(var ...)] or @var[Vs] in order and then applies @var[f] to a list of the
  @rtech{synchronization results}.

  @example[
    (sync (fmap + (pure 1) (pure 2) (pure 3)))
  ]
}

@deftogether[(
  @defproc[(app [F evt?] [V evt?] ...) evt?]
  @defproc[(app* [F evt?] [Vs (listof evt?)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that synchronizes @var[F] and
  @racket[#,(var V) #,(var ...)] (or @var[Vs]) in order and then applies the
  @rtech{synchronization result} of the former to a list of the
  @rtech{synchronization results} of the latter.

}

@deftogether[(
  @defproc[(bind [f (-> any/c ... evt?)] [V evt?] ...) evt?]
  @defproc[(bind* [f (-> any/c ... evt?)] [Vs (listof evt?)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that synchronizes @racket[#,(var V)
  #,(var ...)] or @var[Vs] in order and then becomes the event returned from
  applying @var[f] to a list of the @rtech{synchronization results}.

}

@defproc[(seq [V evt?] ...+) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes @racket[#,(var V)
  #,(var ...)] in order and then uses the @rtech{synchronization result} of
  the final @var[V] as its own.

}

@defproc[(seq0 [V evt?] ...+) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes @racket[#,(var V)
  #,(var ...)] in order and then uses the @rtech{synchronization result} of
  the first @var[V] as its own.

}

@defproc[(test [E1 evt?] [E2 evt?] [E3 evt?]) evt?]{

  Returns a @rtech{synchronizable event} that becomes either @var[E2] or
  @var[E3]. If the @rtech{synchronization result} of @var[E1] is not
  @racket[#f], it becomes @var[E2]. Otherwise, it becomes @var[E3].

}

@section{Asynchronous Combinators}

@defmodule[event/async-monad]
