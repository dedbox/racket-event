#lang scribble/manual

@title{Event-lang: synchronizable event programming}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@require{./event-lang-includes.rkt}

@require[
  @for-label[
    event
    racket/base
    racket/contract/base
    racket/function
    racket/list
  ]
]

@example[#:hidden
  (require event
           racket/function
           racket/list
           racket/match)
]

@table-of-contents[]

@; =============================================================================

@section{Introduction}

Event-lang is a Racket library that simplifies the creation of complex
synchronizable events. It provides a primitive lifting form

@example[(event 123)]

along with a collection of event combinators.

@example[(sync (list-evt (event 1) (event 2) (event 3)))]

Composite events make progress by synchronizing constituent events, either
concurrently or in a predictable sequence. Synchronization results can be
ordered as specified,

@example[
  (let ([t0 (current-inexact-milliseconds)])
    (define (now) (- (current-inexact-milliseconds) t0))
    (sync (async-list-evt
           (event (cons 1 (now)))
           (event (cons 2 (now)))
           (event (cons 3 (now)))
           (event (cons 4 (now)))
           (event (cons 5 (now))))))
]

or as completed.

@example[
  (let ([t0 (current-inexact-milliseconds)])
    (define (now) (- (current-inexact-milliseconds) t0))
    (sync (list-bag-evt
           (event (cons 1 (now)))
           (event (cons 2 (now)))
           (event (cons 3 (now)))
           (event (cons 4 (now)))
           (event (cons 5 (now))))))
]

@; -----------------------------------------------------------------------------

@subsection{Some Examples}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection[#:style '(toc-hidden unnumbered)]{Wait until a bunch of stuff is done}

Use @racket[async-void-evt] to wait until all events are ready and then ignore
the results.

@example[
  (sync (async-void-evt (event (print 1) 4)
                        (event (print 2) 5)
                        (event (print 3) 6)))
  (sync (async-void-evt (event (print 7) 10)
                        (event (print 8) 11)
                        (event (print 9) 12)))
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

Synchronize an event in a background @rtech{thread} and then memoize the
@rtech{synchronization results}.

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
expression at synchronization time. It is like the @racket[lambda] of event
programming: an @racket[event] can close over free variables in its body and
it can produce multiple values simultaneously, but it has no argument list.

@; -----------------------------------------------------------------------------

@subsection{Connecting Events to Functions}

Racket comes with some basic event constructors: @racket[handle-evt],
@racket[replace-evt], and @racket[guard-evt].

@racket[handle-evt] extends the synchronization time of an event by applying a
function to its @rtech{synchronization result}.

@example[
  (sync (handle-evt (event (values 1 2 3)) +))
  (sync (handle-evt (event (values 1 2 3)) (λ xs (map sub1 xs))))
]

@racket[replace-evt] composes an event with a function that produces another
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

@racket[guard-evt] invokes an event-producing thunk at synchronization time.

@example[
  (define one-two-many
    (guard-evt (let ([N 0])
                 (λ ()
                   (set! N (add1 N))
                   (event (if (< N 3) N 'many))))))
]

@example[(sync (apply list-evt (make-list 4 one-two-many)))]

@; -----------------------------------------------------------------------------

@subsection{Lists and Values}

@racket[list-evt] takes a list of events and produces a list of their
@rtech{synchronization results}.

@example[(sync (list-evt (event 1) (event 2) (event (values 3 4))))]

The @racket[values-evt] combinator composes @racket[list-evt] with @racket[values].

@example[(sync (values-evt (event 1) (event 2) (event 3)))]

@; -----------------------------------------------------------------------------

@subsection{Connecting Functions to Events}

@racket[series-evt] passes results to arguments in a series of arbitrary event
combinators.

@example[
  (sync (series-evt
         (event (values 1 2 3))
         (λ xs (event (values (apply + xs) 4)))
         (λ ys (event (apply * ys)))))
]

@racket[reduce-evt] recursively applies a function to its results until the
results satisfy a predicate.

@example[
  (define (two-to-the p)
    (reduce-evt
     (λ (n k) (event (values (* n 2) (+ k 1))))
     1 0
     #:while (λ (__ k) (< k p))))
  (sync (two-to-the 4))
  (sync (two-to-the 7))
]

@; -----------------------------------------------------------------------------

@subsection{Connecting Events to Events}

The @racket[become-evt] form wraps an event-producing expression sequence in
an event with the same synchronization-time behavior and
@rtech{synchronization result}.

@example[
  (define evt (become-evt (println 'X) (event 'nested)))
  (sync evt)
  (sync evt)
]

Its name comes from the Actor model.

@example[
  (define (worker-evt N)
    (replace-evt
     (thread-receive-evt)
     (λ _
       (become-evt
        (match (thread-receive)
               ['inc (worker-evt (add1 N))]
               ['dec (worker-evt (sub1 N))]
               [(? thread? t) (replace-evt (deliver-evt t N)
                                           (λ _ (worker-evt N)))])))))
  (define (deliver-evt t msg)
    (void-evt (thread (λ () (thread-send t msg)))))
]

@example[
  (define a-printer (thread (λ () (writeln (thread-receive)))))
  (define a-worker (thread (λ () (sync (worker-evt 0)))))
  (for ([_ 5]) (sync (deliver-evt a-worker 'inc)))
  (for ([_ 2]) (sync (deliver-evt a-worker 'dec)))
  (sync (deliver-evt a-worker a-printer))
]

@racket[if-evt] is a multi-valued @racket[if] expression for events. If any of
the values produced by the test event are @racket[#f], the test fails.

@example[
  (sync (if-evt (event (values 1  2)) (event 'Tru) (event 'Fls)))
  (sync (if-evt (event (values 3 #f)) (event 'Tru) (event 'Fls)))
]

@; =============================================================================

@section{Synchronization Time}

A @rtech{semaphore} is a simple event-based @rtech{thread} synchronization
mechanism. Suppose we wanted to create a @rtech{semaphore} that short-circuits
after a few posts. We could use @racket[guard-evt] to choose its behavior at
each synchronization.

@example[
  (define (guarded-semaphore N)
    (define sema (make-semaphore))
    (define (next) (set! N (- N 1)) sema)
    (values sema (guard-evt (λ () (if (<= N 0) always-evt (next))))))
]

@racket[become-evt] can do the same thing without the @racket[lambda]
abstraction.

@example[
  (define (bounded-semaphore N)
    (define sema (make-semaphore))
    (define (next) (set! N (- N 1)) sema)
    (values sema (become-evt (if (<= N 0) always-evt (next)))))
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
    (apply async-void-evt
           (map (curryr channel-put-evt v) cs)))
]

@example[
  (define chs (build-list 5 (λ _ (make-channel))))
  (code:line
   (define ts
     (for/list ([ch chs] [i 5]) (code:comment "read many times")
       (thread (λ () (writeln (cons i (channel-get ch))))))))
  (code:line
   (sync (replace-evt (channel-dup-evt chs 'X) (code:comment "write once")
                      (λ _ (apply async-void-evt ts)))))
]

@; -----------------------------------------------------------------------------

@subsection{Generating Sequences}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection[#:style '(toc-hidden unnumbered)]{The natural numbers}

Close over a counter and increment it once per sync.

@example[
  (define nats (let ([n 0]) (event (begin0 n (set! n (add1 n))))))
  (sync nats)
  (sync nats)
  (sync nats)
]

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection[#:style '(toc-hidden unnumbered)]{Fibonacci numbers}

The ``hello world'' of recursion.

@example[
  (define (naive-fibs n)
    (case n
      [(0) (event 0)]
      [(1) (event 1)]
      [else (replace-evt
             (async-values-evt (naive-fibs (- n 1))
                               (naive-fibs (- n 2)))
             (λ (a b) (event (+ a b))))]))
]

The naive implementation is very slow.

@example[
  (eval:alts
   (time (sync (naive-fibs 29)))
   (eval:result
    (racketresult 514229)
    "cpu time: 164650 real time: 167406 gc time: 13820"
    ""))
]

We can make it faster with dynamic programming:

@example[
  (define dynamic-fibs
    (let ([a 1] [b 0])
      (event (begin0 b (set!-values (a b) (values (+ a b) a))))))
]

@example[
  (let ([evt (apply list-evt (make-list 30 dynamic-fibs))])
    (time (last (sync evt))))
]

@racketid[dynamic-fibs] can be combined with @racketid[nats] to build an
index.

@example[
  #:hidden
  (set! nats (let ([n 0]) (event (begin0 n (set! n (add1 n))))))
  (set! dynamic-fibs
    (let ([a 1] [b 0])
      (event (begin0 b (set!-values (a b) (values (+ a b) a))))))
]

@example[
  (define fibs (make-hash))
  (sync
   (apply async-void-evt
          (make-list 30 (handle-evt
                         (values-evt nats dynamic-fibs)
                         (λ (n f) (hash-set! fibs n f))))))
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

  Example:
  @example[
    (define evt (event (writeln (+ 1 2))))
    (sync evt)
    (sync evt)
  ]
}

@defform[(become-evt expr ... event-expr)]{

  Lifts an event-producing @var[event-expr] preceded by any number of
  arbitrary @var[expr]s into a @rtech{synchronizable event} that immediately
  replaces itself with the event produced by @var[event-expr].

  Example:
  @example[(sync (become-evt (event 123)))]
}

@defproc[(list-evt [evt evt?] ...) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes the @var[evt]s in
  order. The @rtech{synchronization result} is a list of the
  @rtech{synchronization results} of the @var[evt]s.

  Example:
  @example[(sync (list-evt (event 1) (event 2) (event 3)))]
}

@defproc[(values-evt [evt evt?] ...) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes the @var[evt]s in
  order and then applies @racket[values] to the @rtech{synchronization
  result}.

  Example:
  @example[(sync (values-evt (event 1) (event 2) (event 3)))]
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

@defproc[(if-evt [test-evt evt?]
                 [then-evt evt?]
                 [else-evt evt?]) evt?]{

  Returns a @rtech{synchronizable event} that becomes either @var[then-evt] or
  @var[else-evt]. If no value in the @rtech{synchronization result} of
  @var[test-evt] is @racket[#f], it becomes @var[then-evt]. Otherwise, it
  becomes @var[else-evt].

  Example:
  @example[
    (sync (if-evt (event #t) (event 'Tru) (event 'Fls)))
    (sync (if-evt (event #f) (event 'Tru) (event 'Fls)))
    (sync (if-evt (event (values #t #t)) (event 'Tru) (event 'Fls)))
    (sync (if-evt (event (values #t #f)) (event 'Tru) (event 'Fls)))
    (sync (if-evt (event (values #f #t)) (event 'Tru) (event 'Fls)))
    (sync (if-evt (event (values #f #f)) (event 'Tru) (event 'Fls)))
    (sync (if-evt (event (values)) (event 'Tru) (event 'Fls)))
  ]
}

@defproc[(series-evt [init-evt evt?] [f (-> any/c evt?)] ...) evt?]{

  Returns a @rtech{synchronizable event} that applies the @var[f]s in series,
  starting with the @rtech{synchronization result} of @var[init-evt] and
  continuing with the @rtech{synchronization result} of the event generated by
  the previous @var[f]. The @rtech{synchronization result} is the
  @rtech{synchronization result} of the event generated by the final @var[f].

  Example:
  @example[
    (sync (series-evt
           (event 1)
           (λ (x) (event (+ x 2)))
           (λ (x) (event (* x 3)))))
  ]
}

@defproc[(reduce-evt [f (-> any/c ... evt?)]
                     [v any/c] ...
                     [#:while pred (-> any/c ... boolean?) (λ _ #t)]) evt?]{

  Returns a @rtech{synchronizable event} that applies @var[f] repeatedly,
  starting with @var[v]s and continuing with the @rtech{synchronization
  result} of the events generated by @var[f], until @var[pred] returns
  @racket[#f].

  Example:
  @example[
    (sync (reduce-evt (λ (x) (event (add1 x)))
                      0
                      #:while (λ (x) (< x 10))))
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

@defproc[(list-bag-evt [evt evt?] ...) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes all the @var[evt]s
  opportunistically and then returns a list of the @rtech{synchronization
  results} in order of completion.

  Example:
  @example[
    (define evt
      (list-bag-evt (event 1) (event 2) (event 3) (event 4) (event 5)))
    (sync evt)
    (sync evt)
    (sync evt)
  ]
}

@defproc[(values-bag-evt [evt evt?] ...) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes all of the
  @var[evt]s opportunistically and then returns all of the
  @rtech{synchronization results} simultaneously as distinct return values in
  order of completion.

  Example:
  @example[
    (define evt
      (values-bag-evt (event 1) (event 2) (event 3) (event 4) (event 5)))
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

@defproc[(async-values-evt [evt evt?] ...) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes all of the
  @var[evt]s opportunistically and then returns all of the
  @rtech{synchronization results} simultaneously as distinct return values in
  the original order.

  Example:
  @example[
    (define evt
      (async-values-evt (event 1) (event 2) (event 3) (event 4) (event 5)))
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
    (define ps (apply promises-evt (make-list 10 ch)))
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
