#lang scribble/manual

@title{Event-lang: synchronizable event programming}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@require{./event-lang-includes.rkt}

@require[
  @for-label[
    algebraic/data/event
    (except-in algebraic/racket/base do)
    event
    racket/contract/base
    racket/list
  ]
]

@example[#:hidden
  (require algebraic/data/event
           event
           racket/list
           racket/match)
  (instantiate EventApplicative)
]

@table-of-contents[]

@; =============================================================================

@section{Introduction}

Event-lang is a Racket library that simplifies the creation of complex
synchronizable events. It provides a primitive lifting form

@example[(event 123)]

along with a collection of event combinators.

@example[
  (sync (list-evt (event 1) (event 2) (event 3)))
]

Composite events make progress by synchronizing constituent events, either
concurrently or in a predictable sequence. Synchronization results can be
ordered as specified,

@example[
  (let ([t0 (current-inexact-milliseconds)])
    (define (now) (- (current-inexact-milliseconds) t0))
    (sync (async-list-evt
           (event (:: 1 (now)))
           (event (:: 2 (now)))
           (event (:: 3 (now)))
           (event (:: 4 (now)))
           (event (:: 5 (now))))))
]

or as completed.

@example[
  (let ([t0 (current-inexact-milliseconds)])
    (define (now) (- (current-inexact-milliseconds) t0))
    (sync (list-set-evt
           (event (:: 1 (now)))
           (event (:: 2 (now)))
           (event (:: 3 (now)))
           (event (:: 4 (now)))
           (event (:: 5 (now))))))
]

@; -----------------------------------------------------------------------------

@subsection{Some Examples}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection[#:style '(toc-hidden unnumbered)]{Wait until a bunch of stuff is done}

Use @racket[async-void-evt] to wait until all events are ready and then ignore
the results.

@example[
  (sync (async-void-evt (event (print 1) 1) (event (print 2) 2)))
  (sync (async-void-evt (event (print 3) 3) (event (print 4) 4)))
]

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection[#:style '(toc-hidden unnumbered)]{Memoization}

Record the @rtech{synchronization results} of an event and thereafter
reproduce them immediately, so only the first successful sync has side
effects.

@example[
  (define evt1 (event (write 'X)))
  (define evt2 (memoize-evt evt1))
  (begin (sync evt1) (sync evt1) (sync evt1))
  (begin (sync evt2) (sync evt2) (sync evt2))
]

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection[#:style '(toc-hidden unnumbered)]{Promises}

Capture the return values of a @racketid[thunk] in a background @rtech{thread}
and then produce them as @rtech{synchronization results}. The results are
memoized explicitly.

@example[
  (define p (promise-evt (event (writeln 1) 2)))
  (sync p)
  (sync p)
]

@; =============================================================================

@section{Event Combinators}

@; -----------------------------------------------------------------------------

@subsection{The @racket[event] Form}

The @racket[event] form creates an event that evaluates an arbitrary
expression at synchronization time. It is the @racket[lambda] of event
programming: @racket[event]s can close over free variables in its body and it
can produce multiple values simultaneously, but it has no argument list.

@; -----------------------------------------------------------------------------

@subsection{Connecting Events to Functions}

Racket provides the following basic event constructors: @racket[handle-evt],
@racket[replace-evt], and @racket[guard-evt].

@racket[handle-evt] extends the synchronization time of an event by applying a
function to its @rtech{synchronization result}.

@example[
  (sync (handle-evt (event (id 1 2 3)) +))
  (sync (handle-evt (event (id 1 2 3)) (λ xs (map sub1 xs))))
]

@racket[replace-evt] composes an event with any function that produces another
event. In the simplest case, this splices one event onto the end of another.

@example[
  (define (splice-evts e1 e2)
    (replace-evt e1 (λ _ e2)))
]

@example[
  (sync (splice-evts
         (event (writeln 'E1))
         (event (writeln 'E2))))
]

@racket[guard-evt] invokes a thunk to produce an event at synchronization
time.

@example[
  (define N 0)
  (define one-two-many
    (guard-evt (λ ()
                 (set! N (add1 N))
                 (return (if (< N 3) N 'many)))))
]

@example[
  (sync (apply list-evt (make-list 4 one-two-many)))
]

@; -----------------------------------------------------------------------------

@subsection{Lists and Values}

@racket[list-evt] takes a list of events and produces a list of their
@rtech{synchronization results}.

@example[
  (sync (list-evt (event 1) (event 2) (event (id 3 4))))
]

The @racket[id-evt] combinator composes @racket[list-evt] with @racket[id].

@example[
  (sync (id-evt (event 1) (event 2) (event 3)))
]

@; -----------------------------------------------------------------------------

@subsection{Connecting Functions to Events}

@racket[series-evt] passes results to arguments in a series of arbitrary event
combinators.

@example[
  (sync (series-evt
         (event (id 1 2 3))
         (λ xs (event (id ($ + xs) 4)))
         (.. return *)))
]

@racket[reduce-evt] recursively applies a function to its results until the
results satisfy a predicate.

@example[
  (define (two-to-the p)
    (reduce-evt
     (λ (n k) (event (id (* n 2) (+ k 1))))
     (φ* (__ __ __ k) (>= k p))
     1 0))
  (sync (two-to-the 10))
  (sync (two-to-the 16))
]

@; -----------------------------------------------------------------------------

@subsection{Connecting Events to Events}

@racket[become] synchronizes an event and then synchronizes its
@rtech{synchronization result}. It gets its name from the actor model.

@example[
  (define (worker N)
    (*> (thread-receive-evt)
        (become (match (thread-receive)
                  ['inc (become (worker (add1 N)))]
                  ['dec (become (worker (sub1 N)))]
                  [(? thread? t) (*> (deliver t N) (worker N))]))))
  (define (deliver t msg)
    (void-evt (thread (λ () (thread-send t msg)))))
  (define a-printer (thread (λ () (writeln (thread-receive)))))
  (define a-worker (thread (λ () (sync (worker 0)))))
  (for ([_ 5]) (sync (deliver a-worker 'inc)))
  (for ([_ 2]) (sync (deliver a-worker 'dec)))
  (sync (*> (deliver a-worker a-printer)
            (void-evt a-printer)))
]

@racket[test-evt] is a multi-valued @racket[if] expression for events. If none
of the values produced by @racketid[test-evt] are @racket[#f], the test
succeeds.

@example[
  (list
   (sync (test-evt (event (id 1 2 )) (event 'Tru) (event 'Fls)))
   (sync (test-evt (event (id 3 #f)) (event 'Tru) (event 'Fls))))
]

@; =============================================================================

@section{Synchronization Time}

The @rtech{semaphore} is a simple event-based @rtech{thread} synchronization
mechanism. Suppose we wanted to create a @rtech{semaphore} that short-circuits
after a few posts. We could use @racket[guard-evt] to choose its behavior at
each synchronization.

@example[
  (define (guarded-semaphore N)
    (define sema (make-semaphore))
    (define (next) (set! N (- N 1)) sema)
    (id sema (guard-evt (λ () (if (<= N 0) always-evt (next))))))
]

The @racket[become] combinator can do the same thing without the
@racket[lambda] abstraction.

@example[
  (define (bounded-semaphore N)
    (define sema (make-semaphore))
    (define (next) (set! N (- N 1)) sema)
    (id sema (become (if (<= N 0) always-evt (next)))))
]

@racketid[guarded-semaphore] returns two values: an actual semaphore for
posting and a bounded reference for synchronizing. At first, @racketid[sema]
and @racketid[semb] do the same thing. Each time @racketid[sema] receives a
post, a @rtech{thread} waiting on @racketid[semb] wakes up. After
@racketid[sema] receives @racketid[N] posts, all @rtech{threads} waiting on
@racketid[semb] wake up and it becomes permanently @rtech{ready for
synchronization}.

@example[
  (define-values (sema semb) (bounded-semaphore 2))
  (eval:alts
   (sync (async-void-evt
          (thread (λ ()
                    (writeln '(A 1)) (semaphore-post sema)
                    (writeln '(A 2)) (semaphore-post sema)))
          (thread (λ ()
                    (sync semb) (writeln '(B 1))
                    (sync semb) (writeln '(B 2))
                    (sync semb) (writeln '(B 3))))))
   @;-----------
   (sync (async-void-evt
          (thread (λ ()
                    (writeln '(A 1)) (semaphore-post sema)
                    (sleep 0.1)
                    (writeln '(A 2)) (semaphore-post sema)))
          (thread (λ ()
                    (sync semb) (writeln '(B 1))
                    (sync semb) (writeln '(B 2))
                    (sync semb) (writeln '(B 3)))))))
]

@; -----------------------------------------------------------------------------

@subsection{Duplicating @racket[channel-put-evt]s}

@example[
  (define (channel-dup-evt cs v)
    ($ async-void-evt (map (<< channel-put-evt v) cs)))
]

@example[
  (define chs (build-list 5 (λ _ (make-channel))))
  (code:line
   (define ts
     (for/list ([ch chs] [i 5]) (code:comment "read many times")
       (thread (λ () (writeln (:: i (channel-get ch))))))))
  (code:line
   (sync (*> (channel-dup-evt chs 'X) (code:comment "write once")
             ($ async-void-evt ts))))
]

@; -----------------------------------------------------------------------------

@subsection{Generating Sequences}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection[#:style '(toc-hidden unnumbered)]{The natural numbers}

Close over a counter and increment it once per sync.

@example[
  (define nat (let ([n 0]) (event (begin0 n (set! n (add1 n))))))
  (sync nat)
  (sync nat)
  (sync nat)
]

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection[#:style '(toc-hidden unnumbered)]{Fibonacci numbers}

The ``hello world'' of recursion.

@example[
  (define (naive-fib n)
    (case n
      [(0) (event 0)]
      [(1) (event 1)]
      [else (do (a) <- (naive-fib (- n 1))
                (b) <- (naive-fib (- n 2))
                (return (+ a b)))]))
]

The naive implementation is very slow.

@example[
  (eval:alts
   (time (sync (naive-fib 29)))
   (eval:result
    (racketresult 514229)
    "cpu time: 5826 real time: 5831 gc time: 1004"
    ""))
]

This one is much faster:

@example[
  (define fib
    (let ([a 1] [b 0])
      (event (begin0 b (set!-values (a b) (id (+ a b) a))))))
]

@example[
  (time (last (sync ($ list-evt (make-list 30 fib)))))
]

@racketid[fib] can be combined with @racketid[nat] to build an index.

@example[
  #:hidden
  (set! nat (let ([n 0]) (event (begin0 n (set! n (add1 n))))))
  (set! fib (let ([a 1] [b 0])
              (event (begin0 b (set!-values (a b) (id (+ a b) a))))))
]

@example[
  (define fibs (make-hash))
  (sync ($ async-void-evt
           (make-list
            30 (do (n) <- nat
                   (f) <- fib
                   (return (hash-set! fibs n f))))))
  (hash-ref fibs 29)
]

@; =============================================================================

@section{Reference}

@defmodule[event]

@; -----------------------------------------------------------------------------

@subsection{Sequential Combinators}

@defform[(event datum ...+)]{

  Lifts @var[datum]s into a into a @rtech{synchronizable event}. Delays
  evaluation of the @var[datum]s until a thread synchronizes on it. The
  @rtech{synchronization result} is the evaluation result.

  @example[
    (define evt (event (writeln (+ 1 2))))
    (sync evt)
    (sync evt)
  ]
}

@defform[(become evt-expr)]{

  Lifts an event-producing @var[evt-expr] into a @rtech{synchronizable event}
  that immediately replaces itself with the event produced by @var[evt-expr].

  @example[
    (sync (become (event 123)))
  ]
}

@defproc[(list-evt [evt evt?] ...) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes the @var[evt]s in
  order. The @rtech{synchronization result} is a list of the
  @rtech{synchronization results} of the @var[evt]s.

  @example[
    (sync (list-evt (event 1) (event 2) (event 3)))
  ]
}

@defproc[(id-evt [evt evt?] ...) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes the @var[evt]s in
  order and then applies @racket[id] to the @rtech{synchronization
  result}.

  @example[
    (sync (id-evt (event 1) (event 2) (event 3)))
  ]
}

@defproc[(void-evt [evt evt?] ...) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes the @var[evt]s in
  order and discards the results.

  Example:
  @example[
    (sync (void-evt
           (event (println 1) 1)
           (event (println 2) 2)
           (event (println 3) 3)
           (event (println 4) 4)
           (event (println 5) 5)))
  ]
}

@defproc[(test-evt [test-evt evt?]
                   [then-evt evt?]
                   [else-evt evt?]) evt?]{

  Returns a @rtech{synchronizable event} that becomes either @var[then-evt] or
  @var[else-evt]. If no value in the @rtech{synchronization result} of
  @var[test-evt] is @racket[#f], it becomes @var[then-evt]. Otherwise, it
  becomes @var[else-evt].

  @example[
  (list
   (sync (test-evt (event #t) (event 'Tru) (event 'Fls)))
   (sync (test-evt (event #f) (event 'Tru) (event 'Fls)))
   (sync (test-evt (event (id #t #t)) (event 'Tru) (event 'Fls)))
   (sync (test-evt (event (id #t #f)) (event 'Tru) (event 'Fls)))
   (sync (test-evt (event (id #f #t)) (event 'Tru) (event 'Fls)))
   (sync (test-evt (event (id #f #f)) (event 'Tru) (event 'Fls)))
   (sync (test-evt (event (id)) (event 'Tru) (event 'Fls))))
  ]
}

@defproc[(series-evt [init-evt evt?] [f (-> any/c evt?)] ...) evt?]{

  Returns a @rtech{synchronizable event} that applies the @var[f]s in series,
  starting with the @rtech{synchronization result} of @var[init-evt] and
  continuing with the @rtech{synchronization result} of the event generated by
  the previous @var[f]. The @rtech{synchronization result} is the
  @rtech{synchronization result} of the event generated by the final @var[f].

  @example[
    (sync (series-evt
           (event 1)
           (λ (x) (event (+ x 2)))
           (λ (x) (event (* x 3)))))
  ]
}

@defproc[(reduce-evt [f (-> any/c ... evt?)]
                     [check (-> any/c ... boolean?)]
                     [v any/c] ...) evt?]{

  Returns a @rtech{synchronizable event} that applies @var[f] repeatedly,
  starting with @var[v]s and continuing with the @rtech{synchronization
  result} of the events generated by @var[f].

  Applies @var[check] to the results of @var[f] and becomes @rtech{ready for
  synchronization} when @var[check] returns @racket[#t].

  Example:
  @example[
    (sync (reduce-evt
           (φ x (event (add1 x)))
           (λ (x y) (>= y 10))
           0))
  ]
}

@defproc[(memoize-evt [evt evt?]) evt?]{

  Returns a @rtech{synchronizable event} that syncs @var[evt] and remembers
  the result. The @rtech{synchronization result} is the @rtech{synchronization
  result} of @var[evt].

  Example:
  @example[
    (define evt1 (event (writeln '!!) (+ 1 2)))
    (sync evt1)
    (sync evt1)
    (define evt2 (memoize-evt evt1))
    (sync evt2)
    (sync evt2)
    (sync evt2)
  ]
}

@; -----------------------------------------------------------------------------

@subsection{Concurrent Combinators}

@defproc[(list-set-evt [evt evt?] ...) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes all the @var[evt]s
  opportunistically and then returns a list of the @rtech{synchronization
  results} in order of completion.

  Example:
  @example[
    (define evt
      (list-set-evt (event 1) (event 2) (event 3) (event 4) (event 5)))
    (sync evt)
    (sync evt)
    (sync evt)
  ]
}

@defproc[(id-set-evt [evt evt?] ...) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes all of the
  @var[evt]s opportunistically and then returns all of the
  @rtech{synchronization results} simultaneously as distinct return values in
  order of completion.

  Example:
  @example[
    (define evt
      (id-set-evt (event 1) (event 2) (event 3) (event 4) (event 5)))
    (sync evt)
  ]
}

@defproc[(async-list-evt [evt evt?] ...) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes all of the
  @var[evt]s opportunistically and then returns a list of the
  @rtech{synchronization results} in the original order.

  Example:
  @example[
    (define evt
      (async-list-evt (event 1) (event 2) (event 3) (event 4) (event 5)))
    (sync evt)
    (sync evt)
    (sync evt)
  ]
}

@defproc[(async-id-evt [evt evt?] ...) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes all of the
  @var[evt]s opportunistically and then returns all of the
  @rtech{synchronization results} simultaneously as distinct return values in
  the original order.

  Example:
  @example[
    (define evt
      (async-id-evt (event 1) (event 2) (event 3) (event 4) (event 5)))
    (sync evt)
  ]
}

@defproc[(async-void-evt [evt evt?] ...) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes the @var[evt]s
  opportunistically and discards the results.

  Example:
  @example[
    (sync (async-void-evt
           (event (println 1) 1)
           (event (println 2) 2)
           (event (println 3) 3)
           (event (println 4) 4)
           (event (println 5) 5)))
  ]
}

@defproc[(promise-evt [evt evt?]) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes @var[evt] in a
  background @rtech{thread} and becomes @rtech{ready for synchronization} when
  the @rtech{thread} finishes. The @rtech{synchronization result} is the
  @rtech{synchronization result} of @var[evt].

  Example:
  @example[
    (define ch (make-channel))
    (define ps (for/list ([_ 10]) (promise-evt ch)))
    (for ([i 10]) (channel-put ch i))
    (map sync ps)
  ]

  The @rtech{synchronization result} of a @racket[promise-evt] is memoized.
  Attempting to synchronize a finished @racket[promise-evt] immediately
  produces the original result without any side effects.

}

@defproc[(promises-evt [evt evt?] ...) evt?]{

  Returns a @rtech{synchronizable event} that syncs each of the @var[evt]s in
  a separate background @rtech{thread} and becomes @rtech{ready for
  synchronization} when all of the @rtech{threads} finish. The
  @rtech{synchronization result} is a list of the @rtech{synchronization
  results} of the @var[evt]s.

  Example:
  @example[
    (define ch (make-channel))
    (define ps ($ promises-evt (make-list 10 ch)))
    (for ([i 10]) (channel-put ch i))
    (sync ps)
  ]

  The @rtech{synchronization result} of a @racket[promises-evt] are memoized.
  Attempting to synchronize a finished @racket[promises-evt] immediately
  produce the original result without any side effects.

}

@; -----------------------------------------------------------------------------

@subsection{Gates}

@defmodule[event/gate]

A @deftech{gate} is a simple multi-thread synchronization primitive. A gate is
either opened or closed, and is closed initially. Threads synchronizing on a
closed gate will block until the gate is opened. Once a gate is opened, it
cannot be closed.

@defproc[(gate? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is a @tech{gate}, @racket[#f] otherwise.

}

@defproc[(gate) gate?]{

  Creates and returns a new closed gate.

}

@defproc[(open-gate [g gate?]) evt?]{

  Returns a @rtech{synchronizable event} that simultaneously unblocks all
  threads attempting to synchronize on the gate. Becomes @rtech{ready for
  synchronization} when the gate is opened.

}

@defproc[(gated [g gate?] [evt evt?]) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes @var[evt] and
  becomes @rtech{ready for synchronization} when @var[g] is opened. The
  @rtech{synchronization result} is the @rtech{synchronization result} of
  @var[evt].

}
