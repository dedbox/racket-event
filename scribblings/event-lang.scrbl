#lang scribble/manual

@title{Event-lang: synchronizable event programming}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@(require
  racket/sandbox
  scribble/examples
  (for-label event
             racket/base
             racket/contract/base
             (except-in racket/function thunk)
             racket/list))

@(random-seed 7)

@(define (rtech . args)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

@(define (gtech . args)
   (apply tech #:doc '(lib "scribblings/guide/guide.scrbl") args))

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

@table-of-contents[]

@; =============================================================================

@section{Introduction}

Event-lang is a Racket library that simplifies the creation of complex
synchronizable events. It provides a primitive expression lifting form,

@example[(pure 123)]

some event combinators,

@example[
  (sync (fmap + (pure 1) (pure 2)))
  (sync (app (pure +) (pure 1) (pure 2)))
  (sync (bind (pure 1) (pure 2) (λ xs (pure (apply + xs)))))
]

and a collection of event-friendly alternatives to base Racket forms and
functions.

@example[
  (sync
   (event-let
    ([x (pure 1)]
     [y (pure 2)])
    (pure (list x y))))
]

Composite events make progress by synchronizing constituent events, either
concurrently or in a predictable sequence. Synchronization results can be
ordered as specified,

@example[
  (let ([t0 (current-inexact-milliseconds)])
    (define (now) (- (current-inexact-milliseconds) t0))
    (sync
     (async-args
      (pure (cons 1 (now)))
      (pure (cons 2 (now)))
      (pure (cons 3 (now))))))
]

or as completed.

@example[
  (let ([t0 (current-inexact-milliseconds)])
    (define (now) (- (current-inexact-milliseconds) t0))
    (sync
     (async-set
      (pure (cons 1 (now)))
      (pure (cons 2 (now)))
      (pure (cons 3 (now))))))
]

The project has three outstanding objectives:

@itemlist[
  @item{@emph{Provide a sophisticated lifting form} to simplify usage of the
    provided constructs.}
  @item{@emph{Provide a full-blown @racketmodfont{#lang event/racket/base}}
    for producing whole modules of events and event constructors from ordinary
    Racket code in a principled manner.}
  @item{@emph{Provide support for static analysis of synchronization
    behaviors.} Event programming in Racket is a curious form of
    meta-programming, and a few simple compile-time checks could reduce
    cognitive overhead.}
  #:style 'ordered
]

@; -----------------------------------------------------------------------------

@subsection{Some Examples}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection[#:style '(toc-hidden unnumbered)]{Wait until a bunch of stuff is done}

Use @racket[async-void] to wait until all events are ready and then ignore the
results.

@example[
  (sync (async-void (pure 1) (pure 2) (pure 3)))
  (sync (async-void (pure 4) (pure 5) (pure 6)))
]

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection[#:style '(toc-hidden unnumbered)]{Memoization}

Record the @rtech{synchronization results} of an event and thereafter
reproduce them immediately.

@example[
  (eval:alts
   (define (memoize evt)
     (define result #f)
     (define (save vs)
       (set! result (pure (apply values vs)))
     (become (or result (bind evt (λ vs (save vs) result))))))
   (void))
]

Only the first successful sync has side effects.

@example[
  (define e1 (pure (write 'X)))
  (define e2 (memoize e1))
  (begin (sync e1) (sync e1) (sync e1))
  (begin (sync e2) (sync e2) (sync e2))
]

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection[#:style '(toc-hidden unnumbered)]{Promises}

Capture the return values of a @racketid[thunk] in a background @rtech{thread}
and then produce them as @rtech{synchronization results}.

@example[
  (define (promised thunk)
    (define ch (make-channel))
    (define thunk*
      (λ () (call-with-values thunk (curry channel-put ch))))
    (memoize (seq0 ch (thread thunk*))))
]

The results are memoized explicitly.

@example[
  (define p (promised (λ () (writeln 123) 4)))
  (sync p)
  (sync p)
]

The @racket[promise] combinator is similar, but it takes an event instead of a
thunk.

@; =============================================================================

@section{Event Combinators}

@; use cases:
@; - control abstraction
@;   - goes well with functions
@;     - multi-value composition
@; - extend synchronization behaviors
@; - communications
@;   - gates
@;   - protocols
@; - cooperative concurrency
@;   - coroutines

@; extending synchronization behavior, thread messaging protocols, functional
@; control abstractions, cooperative concurrency

@; -----------------------------------------------------------------------------

@subsection{The @racket[pure] Form}

The @racket[pure] form wraps @racket[always-evt] to create an event that
evaluates an arbitrary expression at synchronization time. It is the
@racket[lambda] of event programming. The @racket[pure] form can close over
free variables in its body, and it can produce multiple values simultaneously,
but it has no argument list.

The implementation of @racket[pure] is dead simple.

@example[
  (eval:alts
   (define-syntax-rule (pure datum)
     (handle-evt always-evt (λ _ datum)))
   (void))
]

The @racketid[datum] is injected into a @racket[lambda] form and will be
re-evaluated each time the event is successfully synchronized. To pre-evaluate
@racketid[datum], use @racket[return] instead.

@; -----------------------------------------------------------------------------

@subsection{Connecting Events to Functions}

Racket provides the basic event constructors: @racket[handle-evt],
@racket[replace-evt], and @racket[guard-evt].

The @racket[handle-evt] constructor extends the synchronization time of an
event by applying a function to its @rtech{synchronization result}.

@example[
  (sync (handle-evt (pure (values 1 2 3)) +))
  (sync (handle-evt (pure (values 1 2 3)) (λ xs (map sub1 xs))))
]

The @racket[replace-evt] constructor composes an event with any function that
produces another event. In the simplest case, this splices one event onto the
end of another.

@example[
  (define (splice-evts e1 e2)
    (replace-evt e1 (λ _ e2)))
]

@example[
  (sync
   (splice-evts
    (pure (writeln 'E1))
    (pure (writeln 'E2))))
]

The @racket[guard-evt] constructor invokes a thunk to produce an event at
synchronization time.

@example[
  (define N 0)
  (define one-two-many
    (guard-evt
     (λ ()
      (set! N (add1 N))
      (return (if (< N 3) N 'many)))))
]

@example[
  (sync (event-list* (make-list 4 one-two-many)))
]

@; -----------------------------------------------------------------------------

@subsection{Argument Lists}

The @racket[arg-list] combinator takes a list of events and produces a list of
their @rtech{synchronization results}.

@example[
  (eval:alts
   (define (arg-list evts)
     (foldr
      (λ (x ys)
        (replace-evt x (λ v (handle-evt ys (λ (vs) (append v vs))))))
      (pure null) evts))
   (void))
]

@example[
  (sync (arg-list (list (pure 1) (pure 2) (pure (values 3 4)))))
]

It's just about as easy to do with direct recursion,

@example[
  (define (rec-arg-list evts [vs null])
    (if (null? evts)
        (return (reverse vs))
        (replace-evt (car evts)
                     (λ v (rec-arg-list (cdr evts) (append v vs))))))
  (sync
   (rec-arg-list (list (pure 1) (pure 2) (pure (values 3 4)))))
]

or with a loop.

@example[
  (define (loop-arg-list evts)
    (let loop ([evts evts] [acc null])
      (if (null? evts)
          (return (apply append (reverse acc)))
          (replace-evt (car evts)
                       (λ vs (loop (cdr evts) (cons vs acc)))))))
  (sync
   (loop-arg-list (list (pure 1) (pure 2) (pure (values 3 4)))))
]

The @racket[args] combinator composes @racket[arg-list] with @racket[values].

@example[
  (eval:alts
   (define (args . evts)
     (handle-evt (arg-list evts) (curry apply values)))
   (void))
]

@example[
  (sync (args (pure 1) (pure 2) (pure 3)))
]

Constructors like @racket[args] with a @gtech{rest argument} are handy in the
REPL. In library code, functions with a final list argument can be easier to
use. Some constructs have a starred variant that splices its last argument
onto the others in the way @racket[list*] does.

@example[
  (eval:alts
   (define (args* . evts)
     (apply args (append (drop-right evts 1) (last evts))))
   (void))
]

@example[
  (sync (args* (pure 1) (list (pure 2) (pure 3))))
]

@; -----------------------------------------------------------------------------

@subsection{Connecting Functions to Events}

The @racket[fmap] combinator applies a function to the @rtech{synchronization
results} of its arguments. The @racket[handle-evt] constructor is essentially
@racket[fmap], but @racket[handle-evt] wants the entire argument list of the
handler function to come from a single event. Providing an event for each
argument is often easier.

@example[
  (eval:alts
   (define (fmap f . evts)
     (handle-evt (args* evts) f))
   (void))
]

@example[
  (define calc1
    (match-lambda
     [`(,a + ,b) (fmap + (calc1 a) (calc1 b))]
     [`(,a * ,b) (fmap * (calc1 a) (calc1 b))]
     [(? number? n) (pure n)]))
  (sync (calc1 '((1 * 2) + (3 * 4))))
]

The @racket[bind] combinator also applies a function to the
@rtech{synchronization results} of its argument list, but the function must be
an event constructor and it must be the last argument. The
@racket[replace-evt] constructor is essentially @racket[bind], but
@racket[replace-evt] wants the entire argument list of the event constructor
to come from a single event. Again, giving each argument as a separate event
is often easier.

@example[
  (eval:alts
   (define (bind . evts+f)
     (replace-evt (args* (drop-right evts+f 1)) (last evts+f)))
   (void))
]

@example[
  (define (calc2 expr)
    (match expr
     [`(,a + ,b) (bind (calc2 a) (calc2 b) (compose return +))]
     [`(,a * ,b) (bind (calc2 a) (calc2 b) (compose return *))]
     [(? number? n) (pure n)]))
  (sync (calc2 '((1 * 2) + (3 * 4))))
]

The @racket[series] constructor passes results to arguments in a series of
arbitrary event combinators.

@example[
  (eval:alts
   (define (series evt . fs)
     (foldl (λ (f e) (replace-evt e f)) evt fs))
   (void))
]

@example[
  (sync
   (series
    (pure (values 1 2 3))
    (λ xs (pure (values (apply + xs) 4)))
    (compose return *)))
]

The @racket[reduce] constructor recursively applies a function to its results
until the results satisfy a predicate.

@example[
  (eval:alts
   (define (reduce f check . xs)
     (define (pred ys) (apply check (append xs ys)))
     (define (recur ys) (apply reduce f check ys))
     (replace-evt
      (apply f xs)
      (λ ys (if (pred ys) (pure (apply values ys)) (recur ys)))))
   (void))
]

@example[
  (define (two-to-the p)
    (reduce
     (λ (n k) (pure (values (* n 2) (+ k 1))))
     (λ (__ ___ ____ k) (>= k p))
     1 0))
  (sync (two-to-the 10))
  (sync (two-to-the 16))
]

@; -----------------------------------------------------------------------------

@subsection{Connecting Events to Events}

The @racket[become] combinator synchronizes an event and then synchronizes the
@rtech{synchronization result}.

@example[
  (eval:alts
   (define-syntax-rule (become expr)
     (join (pure expr)))
   (void))
]

It gets its name from the actor model.

@example[
  (define (worker [N 0])
    (seq
     (thread-receive-evt)
     (become
      (worker (match (thread-receive)
                ['inc (add1 N)]
                ['dec (sub1 N)]
                [(? thread? t) (deliver t N) N])))))
  (define (deliver t msg)
    (fmap void (thread (λ () (thread-send t msg)))))
  (define a-printer (thread (λ () (writeln (thread-receive)))))
  (define a-worker (thread (λ () (sync (worker)))))
  (for ([_ 5]) (sync (deliver a-worker 'inc)))
  (for ([_ 2]) (sync (deliver a-worker 'dec)))
  (sync (seq (deliver a-worker a-printer) (fmap void a-printer)))
]

The @racket[app] combinator applies the @rtech{synchronization result} of its
first argument to the @rtech{synchronization results} of the remaining
arguments.

@example[
  (eval:alts
   (define (app f-evt . evts)
     (replace-evt f-evt (λ (f) (fmap* f evts))))
   (void))
]

@example[
  (sync (app (pure +) (pure 1) (pure 2)))
]

The @racket[seq] combinator synchronizes one or more events and discards all
but the last @rtech{synchronization result}.

@example[
  (eval:alts
   (define (seq evt . evts)
     (if (null? evts) evt (replace-evt evt (λ _ (apply seq evts)))))
   (void))
]

@example[
  (sync (seq (pure 1) (pure 2) (pure 3)))
]

The @racket[seq0] combinator is similar, discarding all but the first
@rtech{synchronization result}.

@example[
  (eval:alts
   (define (seq0 evt . evts)
     (replace-evt
      evt (λ vs (handle-evt (args* evts) (λ _ (apply values vs))))))
   (void))
]

@example[
  (sync (seq0 (pure 1) (pure 2) (pure 3)))
]

The @racket[test] combinator is a multi-valued @racket[if] expression for
events.

@example[
  (eval:alts
   (define (test test-evt then-evt else-evt)
     (replace-evt
      test-evt (λ vs (if (andmap values vs) then-evt else-evt))))
   (void))
]

If none of the values produced by @racketid[test-evt] are @racket[#f], the
test succeeds.

@example[
  (list
   (sync (test (pure (values 1 2)) (pure 'Tru) (pure 'Fls)))
   (sync (test (pure (values 3 #f)) (pure 'Tru) (pure 'Fls))))
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
    (values sema (guard-evt (λ () (if (<= N 0) always-evt (next))))))
]

The @racket[become] combinator can do the same thing without the
@racket[lambda] abstraction.

@example[
  (define (bounded-semaphore N)
    (define sema (make-semaphore))
    (define (next) (set! N (- N 1)) sema)
    (values sema (become (if (<= N 0) always-evt (next)))))
]

Th @racketid[guarded-semaphore] constructor returns two values: an actual
semaphore for posting and a bounded reference for synchronizing. At first,
@racketid[sema] and @racketid[semb] do the same thing. Each time
@racketid[sema] receives a post, a @rtech{thread} waiting on @racketid[semb]
wakes up. After @racketid[sema] receives @racketid[N] posts, all
@rtech{threads} waiting on @racketid[semb] wake up and it becomes permanently
@rtech{ready for synchronization}.

@example[
  (define-values (sema semb) (bounded-semaphore 2))
  (eval:alts
   (sync
    (async-void
     (thread (λ ()
               (writeln '(T1 X)) (semaphore-post sema)
               (writeln '(T1 Y)) (semaphore-post sema)))
     (thread (λ ()
               (sync semb) (writeln '(T2 A))
               (sync semb) (writeln '(T2 B))
               (sync semb) (writeln '(T2 C))))))
   @;-----------
   (sync
    (async-void
     (thread (λ ()
               (writeln '(T1 X)) (semaphore-post sema)
               (sleep 0.1)
               (writeln '(T1 Y)) (semaphore-post sema)))
     (thread (λ ()
               (sync semb) (writeln '(T2 A))
               (sync semb) (writeln '(T2 B))
               (sync semb) (writeln '(T2 C)))))))
]

@; -----------------------------------------------------------------------------

@subsection{Duplicating @racket[channel-put-evt]}

Lists of events are easy to extend with @racket[map].

@example[
  (define (channel-dup-evt cs v)
    (async-void* (map (curryr channel-put-evt v) cs)))
]

@example[
  (define cs (build-list 5 (λ _ (make-channel))))
  (code:line
   (define ts
     (for/list ([c cs] [i 5]) (code:comment "read many times")
       (thread (λ () (writeln (cons i (channel-get c))))))))
  (code:line
   (sync (seq (channel-dup-evt cs 'X) (code:comment "write once")
              (async-void* ts))))
]

@; -----------------------------------------------------------------------------

@subsection{Sequence Generators}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection[#:style '(toc-hidden unnumbered)]{The natural numbers}

Close over a counter and increment it once per sync.

@example[
  (define nat
    (let ([n 0])
      (pure (begin0 n (set! n (add1 n))))))
]

Now we can use @racketid[nat] to get one number at a time.

@example[
  (sync nat)
  (sync nat)
  (sync nat)
]

@racketid[nat] is handy for generating indices and unique keys in bulk through
repetition.

@example[
  (sync (event-list* (make-list 4 nat)))
]

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection[#:style '(toc-hidden unnumbered)]{The Fibonacci sequence}

The ``hello world'' of recursion.

@example[
  (define (naive-fib n)
    (case n
      [(0) (pure 0)]
      [(1) (pure 1)]
      [else (fmap + (naive-fib (- n 1)) (naive-fib (- n 2)))]))
]

Of course, the naive implementation is very slow.

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
      (pure (begin0 b (set!-values (a b) (values (+ a b) a))))))
]

@example[
  (time (last (sync (event-list* (make-list 30 fib)))))
]

@racketid[fib] can be combined with @racketid[nat] to build an index.

@examples[
  #:eval event-evaluator
  #:hidden
  (define nat
    (let ([n 0]) (pure (begin0 n (set! n (add1 n))))))
  (define fib
    (let ([a 1] [b 0])
      (pure (begin0 b (set!-values (a b) (values (+ a b) a))))))
]

@example[
  (define fibs (make-hash))
  (sync
   (async-void*
    (make-list 30 (fmap (curry hash-set! fibs) nat fib))))
  (hash-ref fibs 29)
  (hash-ref fibs 15)
]

@; =============================================================================

@section{Cooperative Concurrency}

@; -----------------------------------------------------------------------------

@subsection{Synchronization Gates}

@; -----------------------------------------------------------------------------

@subsection{Messaging Protocols}

@; -----------------------------------------------------------------------------

@subsection{Interleaving Events}

@; =============================================================================

@section{Reference}

@defmodule[event]

@; -----------------------------------------------------------------------------

@subsection{Sequential Combinators}

@defform[(pure datum)]{

  Lifts @var[datum] into a into a @rtech{synchronizable event}. Delays
  evaluation of @var[datum] until a thread synchronizes on it. The
  @rtech{synchronization result} is the evaluation result.

  @example[
    (define evt (pure (writeln (+ 1 2))))
    (sync evt)
    (sync evt)
  ]
}

@defform[(become expr)]{

  Lifts an event-producing @var[expr] into a @rtech{synchronizable event} that
  immediately replaces itself with the event produced by evaluating
  @var[expr].

  @example[
    (sync (become (pure 123)))
  ]
}

@defproc[(return [v any/c]) evt? #:value (pure v)]{

  Evaluates @var[v] and then lifts the result into an event. Returns a
  @rtech{synchronizable event} that does nothing and uses @var[v] as its
  @rtech{synchronization result}.

  @example[
    (define evt (return (writeln (+ 1 2))))
    (sync evt)
    (sync evt)
  ]
}

@defproc[(arg-list [Es (listof evt?)]) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes @var[Es] in order.
  The @rtech{synchronization result} is a list of the @rtech{synchronization
  results} of the @var[Es].

  @example[
    (sync (arg-list (list (pure 1) (pure 2) (pure 3))))
  ]
}

@deftogether[(
  @defproc[(args [E evt?] ...) evt?]
  @defproc[(args* [E evt?] ... [Es (listof evt?)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that synchronizes @var[E]s in order
  and then applies @racket[values] to the @rtech{synchronization results}.

  @example[
    (sync (args (pure 1) (pure 2) (pure 3)))
  ]
}

@deftogether[(
  @defproc[(fmap [f (-> any/c ... any)] [E evt?] ...) evt?]
  @defproc[(fmap* [f (-> any/c ... any)] [E evt?] ... [Es (listof evt?)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that synchronizes @var[E]s in order
  and then applies @var[f] to the @rtech{synchronization results}.

  @example[
    (sync (fmap + (pure 1) (pure 2) (pure 3)))
  ]
}

@defproc[(join [E evt?]) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes @var[E] and then
  synchronizes the @rtech{synchronization result} of @var[E].

  @example[
    (sync (join (pure (pure 123))))
  ]
}

@deftogether[(
  @defproc[(app [F evt?] [E evt?] ...) evt?]
  @defproc[(app* [F evt?] [E evt?] ... [Es (listof evt?)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that synchronizes @var[F] and
  @var[E]s in order and then applies the @rtech{synchronization result} of the
  former to the @rtech{synchronization results} of the latter.

  @example[
    (sync (app (pure +) (pure 1) (pure 2) (pure 3)))
  ]
}

@deftogether[(
  @defproc[(bind [E evt?] ... [f (-> any/c ... evt?)]) evt?]
  @defproc[(bind* [E evt?] ... [Es (listof evt?)] [f (-> any/c ... evt?)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that synchronizes @var[E]s in order
  and then becomes the event returned from @var[f] applied to the
  @rtech{synchronization results}.

  @example[
    (sync
     (bind
      (pure 1)
      (pure 2)
      (pure 3)
      (compose return +)))
  ]
}

@defproc[(seq [E evt?] ...+) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes @var[E]s in order
  and then uses the final @rtech{synchronization result} as its own.

  @example[
    (sync (seq (pure 1) (pure 2) (pure 3)))
  ]
}

@defproc[(seq0 [E evt?] ...+) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes @var[E]s in order
  and then uses the first @rtech{synchronization result} as its own.

  @example[
    (sync (seq0 (pure 1) (pure 2) (pure 3)))
  ]
}

@defproc[(test [E1 evt?] [E2 evt?] [E3 evt?]) evt?]{

  Returns a @rtech{synchronizable event} that becomes either @var[E2] or
  @var[E3]. If no value in the @rtech{synchronization result} of @var[E1] is
  @racket[#f], it becomes @var[E2]. Otherwise, it becomes @var[E3].

  @example[
  (list
   (sync (test (pure #t) (pure 'Tru) (pure 'Fls)))
   (sync (test (pure #f) (pure 'Tru) (pure 'Fls)))
   (sync (test (pure (values #t #t)) (pure 'Tru) (pure 'Fls)))
   (sync (test (pure (values #t #f)) (pure 'Tru) (pure 'Fls)))
   (sync (test (pure (values #f #t)) (pure 'Tru) (pure 'Fls)))
   (sync (test (pure (values #f #f)) (pure 'Tru) (pure 'Fls)))
   (sync (test (pure (values)) (pure 'Tru) (pure 'Fls))))
  ]
}

@deftogether[(
  @defproc[(series [E evt?] [f (-> any/c evt)] ...) evt?]
  @defproc[
    (series* [E evt?] [f (-> any/c evt)] ... [fs (listof (-> any/c evt?))])
    evt?
  ]
)]{

  Returns a @rtech{synchronizable event} that applies @var[f]s in series,
  starting with the @rtech{synchronization result} of @var[E] and continuing
  with the @rtech{synchronization result} of the event generated by the
  previous @var[f]. Uses the final @rtech{synchronization result} as its own.

  @example[
    (sync
     (series
      (pure 1)
      (λ (x) (return (+ x 2)))
      (λ (x) (return (* x 3)))))
  ]
}

@deftogether[(
  @defproc[
    (reduce [f (-> any/c ... evt?)]
            [check (-> any/c ... boolean?)]
            [v any/c] ...)
    evt?]
  @defproc[
    (reduce* [f (-> any/c ... evt?)]
             [check (-> any/c ... boolean?)]
             [vs (listof any/c)])
    evt?]
)]{

  Returns a @rtech{synchronizable event} that applies @var[f] to a set of
  values recursively, starting with @var[v]s and continuing with the
  @rtech{synchronization result} of the event generated by applying @var[f] to
  the previous results. Applies @var[check] to an argument list created by
  appending @var[v]s onto the results of @var[f]. Becomes @rtech{ready for
  synchronization} when @var[check] returns @racket[#t]. Uses the final
  @rtech{synchronization result} as its own.

  @example[
    (sync
     (reduce
      (λ (x) (pure (add1 x)))
      (λ (x y) (>= y 10))
      0))
  ]
}

@deftogether[(
  @defproc[(loop [f (-> any/c ... evt?)] [v any/c] ...) evt?]
  @defproc[(loop* [f (-> any/c ... evt?)] [vs (listof any/c)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that applies @var[f] to a value
  recursively, starting with @var[v]s and continuing with the
  @rtech{synchronization result} of the event generated by applying @var[f] to
  the previous results.

  @example[
    (with-handlers ([number? values])
      (sync
       (loop (λ (x) (if (< x 10) (pure (+ x 1)) (raise x)))
             0)))
  ]
}

@defproc[(memoize [E evt?]) evt?]{

  Returns a @rtech{synchronizable event} that syncs @var[E] and remembers the
  result. The @rtech{synchronization result} of the whole event is the
  @rtech{synchronization result} of @var[E].

  @example[
    (define e1 (pure (begin (writeln '!!) (+ 1 2))))
    (sync e1)
    (sync e1)
    (define e2 (memoize e1))
    (sync e2)
    (sync e2)
    (sync e2)
  ]
}

@defproc[(promise [E evt?]) evt?]{

  Syncs @var[E] in a background @rtech{thread}. Returns a
  @rtech{synchronizable event} that becomes @rtech{ready for synchronization}
  when the background @rtech{thread} finishes. The @rtech{synchronization
  result} of the whole event is the @rtech{synchronization result} of @var[E].

  @example[
    (define ch (make-channel))
    (define ps (for/list ([_ 10]) (promise ch)))
    (for ([i 10]) (channel-put ch i))
    (map sync ps)
  ]

  The @rtech{synchronization result} of a promise is memoized. Attempting to
  synchronize a finished promise immediately produces the original result
  without any side effects.

}

@deftogether[(
  @defproc[(promises [E evt?] ...) evt?]
  @defproc[(promises* [Es (listof evt?)]) evt?]
)]{

  Syncs each of the @var[E]s in a separate background @rtech{thread}. Returns
  a @rtech{synchronizable event} that becomes @rtech{ready for
  synchronization} when all of the background @rtech{threads} finish. The
  @rtech{synchronization result} of the whole event is a list of the
  @rtech{synchronization results} of the @var[E]s.

  @example[
    (define ch (make-channel))
    (define ps (promises* (make-list 10 ch)))
    (for ([i 10]) (channel-put ch i))
    (sync ps)
  ]

  The @rtech{synchronization results} of the promises are memoized. Attempting
  to synchronize finished promises immediately produce the original results
  without any side effects.

}

@; -----------------------------------------------------------------------------

@subsection{Concurrent Combinators}

@deftogether[(
  @defproc[(async-set [E evt?] ...) evt?]
  @defproc[(async-set* [Es (listof evt?)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that synchronizes @racket[#,(var E)
  #,(var ...)] or @var[Es] concurrently and then applies @racket[values] to a
  list of the @rtech{synchronization results} in order of completion.

  @example[
    (define evt
      (handle-evt (async-set (pure 1) (pure 2) (pure 3)) list))
    (sync evt)
    (sync evt)
    (sync evt)
  ]
}

@deftogether[(
  @defproc[(async-args [E evt?] ...) evt?]
  @defproc[(async-args* [Es (listof evt?)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that evaluates @racket[#,(var E)
  #,(var ...)] or @var[Es] concurrently and then applies @racket[values] to a
  list of the @rtech{synchronization results} in the order defined.

  @example[
    (define evt
      (handle-evt
       (async-args (pure 1) (pure 2) (pure 3))
       list))
    (sync evt)
  ]
}

@deftogether[(
  @defproc[(async-fmap [f (-> any/c ... any)] [E evt?] ...) evt?]
  @defproc[(async-fmap* [f (-> any/c ... any)] [Es (listof evt?)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that synchronizes @racket[#,(var E)
  #,(var ...)] or @var[Vs] concurrently and then applies @var[f] to a list of
  the @rtech{synchronization results}.

  @example[
    (sync (async-fmap + (pure 1) (pure 2) (pure 3)))
  ]
}

@deftogether[(
  @defproc[(async-app [F evt?] [E evt?] ...) evt?]
  @defproc[(async-app* [F evt?] [Es (listof evt?)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that synchronizes @var[F] and
  @racket[#,(var E) #,(var ...)] or @var[Es] concurrently and then applies the
  @rtech{synchronization result} of the former to a list of the
  @rtech{synchronization results} of the latter.

  @example[
    (sync (async-app (pure +) (pure 1) (pure 2) (pure 3)))
  ]
}

@deftogether[(
  @defproc[(async-bind [E evt?] ... [f (-> any/c ... evt?)]) evt?]
  @defproc[(async-bind* [Es (listof evt?)] [f (-> any/c ... evt?)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that synchronizes @racket[#,(var E)
  #,(var ...)] or @var[Es] concurrently and then becomes the event returned
  from @var[f] applied to a list of the @rtech{synchronization results}.

  @example[
    (sync
     (async-bind
      (seq (pure (print 1)) (pure 1))
      (seq (pure (print 2)) (pure 2))
      (seq (pure (print 3)) (pure 3))
      (compose return list)))
  ]
}

@; -----------------------------------------------------------------------------

@subsection{Gates}

@defmodule[event/gate]

A @deftech{gate} is a simple primitive for synchronizing many threads at once.
A gate is either opened or closed and is closed initially. Threads
synchronizing on a closed gate will block until the gate is opened. Once a
gate is opened, it cannot be closed.

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

@defproc[(gated [g gate?] [E evt?]) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes @var[E] and becomes
  @rtech{ready for synchronization} when @var[g] is opened. The
  @rtech{synchronization result} is the @rtech{synchronization result} of
  @var[E].

}

@; -----------------------------------------------------------------------------

@subsection{The Racket API}

@defmodule[event/racket]

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection{Syntactic Forms}

@defform[(event-let ([id val-evt] ...) body-evt ...+)]{

  Creates a @rtech{synchronizable event} that synchronizes the @var[val-evt]s
  from left to right and binds the @var[id]s to the results, then synchronizes
  the @var[body-evt]s. Uses the @rtech{synchronization result} of its final
  @var[body-evt] as its own.

  @example[
    (sync
     (event-let ([x (pure 1)]
                 [y (pure 2)])
       (pure (+ x y))))
  ]
}

@defform[(event-let* ([id val-evt] ...) body-evt ...+)]{

  Like @racket[event-let], but synchronizes the @var[val-evt]s one by one,
  binding each @var[id] as soon as the value is available. The @var[id]s are
  bound in the remaining @var[val-evt]s as well as the @var[body]s, and the
  @var[id]s need not be distinct; later bindings shadow earlier bindings.

  Creates a @rtech{synchronizable event} that Synchronizes the @var[val-evt]s
  from left to right and binds the @var[id]s to the results, then synchronizes
  the @var[body-evt]s. Uses the @rtech{synchronization result} of its final
  @var[body-evt] as its own.

  @example[
    (sync
     (event-let* ([x (pure 1)]
                  [y (pure (+ x 2))])
       (pure (+ x y))))
  ]
}

@defform/subs[
  (event-cond event-cond-clause ...)
  [(event-cond-clause [test-evt then-body-evt ...+]
                      [@#,(racket else) then-body-evt ...+]
                      [test-evt @#,(racket =>) proc-evt]
                      [test-evt])]
]{

  Creates a @rtech{synchronizable event}. If no @var[event-cond-clause]s are
  present, the @rtech{synchronization result} is @(values void-const).

  An @var[event-cond-clause] that starts with @racket[else] must be the last
  @var[event-cond-clause].

  If only a @racket[[else then-body-evt ...+]] is present, then the
  @var[then-body-evt]s are synchronized. The @rtech{synchronization result}
  from all but the last @var[then-body-evt] are ignored. The
  @rtech{synchronization result} of the last @var[then-body-evt] is the
  @rtech{synchronization result} for the whole @racket[event-cond] form.

  Otherwise, the first @var[test-evt] is synchronized. If it produces
  @racket[#f], then the @rtech{synchronization result} is the same as an
  @racket[event-cond] form with the remaining @var[event-cond-clauses].

  @specsubform[[test-evt then-body-evt ...+]]{

    The @var[then-body-evt]s are synchronized in order, and the
    @rtech{synchronization result} from all but the last @var[then-body-evt]
    are ignored. The @rtech{synchronization result} of the last
    @var[then-body-evt] provides the result for the whole @racket[event-cond]
    form.

  }

  @specsubform[[test-evt => proc-evt]]{

    The @var[proc-evt] is synchronized, and it must produce a procedure that
    accepts one argument, otherwise the @racket[exn:fail:contract] exception
    is raised. The procedure is applied to the @rtech{synchronization result}
    of @var[test-evt]. The @rtech{synchronization result} for the whole
    @racket[event-cond] form is the values returned by the procedure call.

  }

  @specsubform[[test-evt]]{

    The @rtech{synchronization result} of @var[test-evt] is provided as the
    @rtech{synchronization result} of the @racket[event-cond] form.

  }

  Examples:
  @example[
    (sync (event-cond))
    (sync (event-cond [else (pure 5)]))
    (sync
     (event-cond
      [(pure (positive? -5)) (pure (error "doesn't get here"))]
      [(pure (zero? -5)) (pure (error "doesn't get here, either"))]
      [(pure (positive? 5)) (pure 'here)]))
    (sync
     (event-cond
      [(pure (member 2 '(1 2 3))) => (pure (lambda (l) (map - l)))]))
    (sync (event-cond [(pure (member 2 '(1 2 3)))]))
  ]
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection{Pairs and Lists}

@deftogether[(
  @defproc[(event-list [E evt?] ...) evt?]
  @defproc[(event-list* [E evt?] ... [Es (listof evt?)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that synchronizes all @var[E]s in
  order and then uses a list of the results as its @rtech{synchronization
  result}.

  @example[
    (sync (event-list (pure 1) (pure 2) (pure 3)))
  ]
}

@defproc[(event-map [f procedure?] [Es (listof evt?)] ...+) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes the elements of the
  @var[Es] lists and applies @var[f] to the @rtech{synchronization results} of
  the elements, from the first elements to the last. The @var[f] argument must
  accept the same number of arguments as the number of supplied @var[Es]s, and
  all @var[Es]s must have the same number of elements. The
  @rtech{synchronization result} is a list containing each result of @var[f]
  in order.

  @example[
    (sync
     (event-map
      +
      (list (pure 1) (pure 2) (pure 3))
      (list (pure 4) (pure 5) (pure 6))))
  ]
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection{Concurrent Syntactic Forms}

@defform[(async-let ([x Ex] ...) E ...+)]{

  Produces a @rtech{synchronizable event} that synchronizes @var[Ex]s
  concurrently, binds the @rtech{synchronization results} to @var[x]s
  internally, and synchronizes the @var[E]s. The @rtech{synchronization
  results} from all but the last @var[E] are ignored. The
  @rtech{synchronization result} of the last @var[E] is the
  @rtech{synchronization result} for the whole @racket[async-let] form.

  @example[
    (sync
     (async-let
         ([x (seq (pure (print 1)) (pure 1))]
          [y (seq (pure (print 2)) (pure 2))]
          [z (seq (pure (print 3)) (pure 3))])
       (pure (values x y z))))
  ]
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsubsection{Concurrent Pairs and Lists}

@deftogether[(
  @defproc[(async-list [E evt?] ...) evt?]
  @defproc[(async-list* [E evt?] ... [Es (listof evt?)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that synchronizes all @var[E]s
  simultaneously and becomes @rtech{ready for synchronization} when all the
  @var[E]s are ready. The @rtech{synchronization result} is a list of the
  results, in order.

  @example[
    (sync (async-list (pure 1) (pure 2) (pure 3)))
  ]
}

@defproc[(async-map [f procedure?] [Es (listof evt?)] ...+) evt?]{

  Returns a @rtech{synchronizable event} that synchronizes @var[Es] lists
  simultaneously and then applies @var[f] to the @rtech{synchronization
  results} of the elements, from the first elements to the last. The @var[f]
  argument must accept the same number of arguments as the number of supplied
  @var[Es]s, and all @var[Es]s must have the same number of elements. The
  @rtech{synchronization result} is a list containing each result of @var[f]
  in order.

  @example[
    (sync
     (async-map
      +
      (list (pure 1) (pure 2) (pure 3))
      (list (pure 4) (pure 5) (pure 6))))
  ]
}

@deftogether[(
  @defproc[(async-void [E evt?] ...) evt?]
  @defproc[(async-void* [E evt?] ... [Es (listof evt?)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that synchronizes all @var[E]s
  simultaneously and becomes @rtech{ready for synchronization} when all the
  @var[E]s are ready. The @rtech{synchronization result} is a single void.

  @example[
    (sync (async-void (pure 1) (pure 2) (pure 3)))
  ]
}

@; The @racket[event] form translates an ordinary Racket expression into a
@; @rtech{synchronizable event} that, when synchronized on, evaluates its
@; expression and then becomes @rtech{ready for synchronization} with the
@; evaluation result as its @rtech{synchronization result}. Sub-expressions are
@; lifted strategically to make large or long-lived events easier to create and
@; re-use.

@; @example[
@;   (define ch (make-channel))
@;   (sync
@;    (loop (λ _ (handle-evt ch write)))
@;    (reduce
@;     (λ (i) (seq (channel-put-evt ch i) (pure (sub1 i))))
@;     (λ (i j) (= j 0))
@;     9))
@; ]

@; @section{Event Construction}

@; @defmodule[event/event]

@; @defform[(event expr ...+)]{

@;   Returns a @rtech{synchronizable event} that delays evaluation of expression
@;   sequence @racket[#,(var expr) #,(var ...)] until synchronized on and then
@;   uses the evaluation result as its @rtech{synchronization result}.

@;   @example[
@;     (event 5 6 7)
@;     (event-do 5 6 7)
@;     (event-print 5 6 7)
@;   ]
@; }

@; @defform[(esc expr)]{

@;   This form can only appear as an expression within an @racket[event] form.
@;   Use it to embed a value directly into the result.

@;   @example[
@;     (event (esc 5))
@;   ]
@; }

@; @defform[(event-do expr ...+)]{

@;   Calls @racket[(event #,(var expr) #,(var ...))] and immediately synchronizes
@;   the generated event.

@;   @; @example[
@;   @;   (event-do (let ([x 3] [y 2]) (+ x y)))
@;   @; ]
@; }

@; @defform[(event-print expr ...+)]{

@;   Prints the event generated by @racket[(event #,(var expr) #,(var ...))] in
@;   combinator form.

@;   @; @example[
@;   @;   (event-print (let ([x 3] [y 2]) (+ x y)))
@;   @; ]
@; }

@; @defform[(event-debug expr...+)]{

@;   Prints the event generated by @racket[(event #,(var expr) #,(var ...))],
@;   including all intermediate representations. Synchronizes the generated event
@;   and prints the @rtech{synchronization result}.

@;   @; @example[
@;   @;   (event-debug (let ([x 3] [y 2]) (+ x y)))
@;   @; ]
@; }
