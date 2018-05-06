#lang racket/base

(require
 event/concurrent
 event/sequential
 racket/function
 racket/pretty
 syntax/parse
 (for-syntax racket/base
             racket/pretty
             syntax/parse))

(provide
 event event-do event-print event-debug
 (all-from-out
  event/concurrent
  event/sequential))

(define-syntax-rule (define-literals literals id ...)
  (begin
    (define-syntax (id stx)
      (raise-syntax-error #f "cannot evaluate abstract form" stx))
    ...
    (begin-for-syntax
     (define-literal-set literals (id ...)))))

(define-literals abstract-literals
  #%event-esc
  ;; racket/base
  #%event-let-values
  #%event-lambda
  #%event-case-lambda
  ;; event/sequential
  #%event-pure
  #%event-return
  #%event-args
  #%event-fmap
  #%event-app
  #%event-bind
  #%event-seq
  #%event-seq0
  #%event-test
  #%event-series
  #%event-reduce
  #%event-loop
  ;; event/concurrent
  #%event-async-set
  #%event-async-args
  #%event-async-fmap
  #%event-async-app
  #%event-async-bind
  ;; internal
  #%event-curry)

(begin-for-syntax
 (define-literal-set event-literals
   (pure return args fmap app bind seq seq0 test series reduce loop
         async-set async-args async-fmap async-app async-bind))

 (define-literal-set racket-literals
   (begin begin0 let let* let-values lambda λ case-lambda if quote))

 (define-syntax-class event
   #:description "EXPAND"
   #:attributes (expand)
   #:commit
   #:literal-sets (racket-literals event-literals)
   #:datum-literals (esc)
   [pattern (esc ~! datum) #:attr expand #'(#%event-esc datum)]
   ;; racket/base
   [pattern (begin ~! E:event ...+) #:attr expand #'(#%event-seq E.expand ...)]
   [pattern (begin0 ~! E:event ...+) #:attr expand #'(#%event-seq0 E.expand ...)]
   [pattern (let ~! bs E ...+) #:with e:event #'(let* bs E ...) #:attr expand #'e.expand]
   [pattern (let* ~! ([x V:event] ...) E:event ...+)
            #:attr expand
            #'(#%event-let-values ([(x) V.expand] ...) (#%event-seq E.expand ...))]
   [pattern (let-values ([xs Vs:event] ...) E:event ...+)
            #:attr expand
            #'(#%event-let-values ([xs Vs.expand] ...) (#%event-seq E.expand ...))]
   [pattern ((~or lambda λ) ~! _ _ ...+) #:attr expand #`(#%event-pure #,this-syntax)]
   [pattern (case-lambda ~! _ ...) #:attr expand #`(#%event-pure #,this-syntax)]
   [pattern (if ~! E1:event E2:event E3:event)
            #:attr expand #'(#%event-test E1.expand E2.expand E3.expand)]
   [pattern (quote ~! _) #:attr expand #`(#%event-pure #,this-syntax)]
   [pattern (((~or lambda λ) ~! xs E:event ...+) V:event ...)
            #:attr expand
            #'(#%event-app (#%event-lambda xs (#%event-seq E.expand ...)) V.expand ...)]
   [pattern ((case-lambda ~! [xs E:event ...+] ...) V:event ...)
            #:attr expand
            #'(#%event-app (#%event-case-lambda [xs (#%event-seq E.expand ...)] ...)
                           V.expand ...)]
   [pattern v:string #:attr expand #'(#%event-pure v)]
   [pattern v:boolean #:attr expand #'(#%event-pure v)]
   [pattern v:number #:attr expand #'(#%event-pure v)]
   ;; event/sequential
   [pattern (pure ~! datum) #:attr expand #'(#%event-pure datum)]
   [pattern (return ~! e) #:attr expand #'(#%event-return e)]
   [pattern (args ~! E:event ...) #:attr expand #'(#%event-args E.expand ...)]
   [pattern (fmap ~! f E:event ...) #:attr expand #'(#%event-fmap f E.expand ...)]
   [pattern (app ~! F:event E:event ...) #:attr expand #'(#%event-app F.expand E.expand ...)]
   [pattern (bind ~! f E:event ...) #:attr expand #'(#%event-bind f E.expand ...)]
   [pattern (seq ~! E:event ...+) #:attr expand #'(#%event-seq E.expand ...)]
   [pattern (seq0 ~! E:event ...+) #:attr expand #'(#%event-seq0 E.expand ...)]
   [pattern (test ~! E1:event E2:event E3:event)
            #:attr expand #'(#%event-test E1.expand E2.expand E3.expand)]
   [pattern (series ~! V:event f ...)
            #:attr expand #'(#%event-series V.expand f ...)]
   [pattern (reduce ~! f check v ...) #:attr expand #'(#%event-reduce f check v ...)]
   [pattern (loop ~! f v ...) #:attr expand #'(#%event-loop f v ...)]
   ;; event/concurrent
   [pattern (async-set ~! E:event ...) #:attr expand #'(#%event-async-set E.expand ...)]
   [pattern (async-args ~! E:event ...) #:attr expand #'(#%event-async-args E.expand ...)]
   [pattern (async-fmap ~! f E:event ...) #:attr expand #'(#%event-async-fmap f E.expand ...)]
   [pattern (async-app ~! F:event E:event ...)
            #:attr expand #'(#%event-async-app F.expand E.expand ...)]
   [pattern (async-bind ~! f E:event ...) #:attr expand #'(#%event-async-bind f E.expand ...)]
   ;; defaults
   [pattern x:id #:attr expand #'(#%event-pure x)]
   [pattern (F:event ~! E:event ...) #:attr expand #'(#%event-app F.expand E.expand ...)]
   [pattern _ #:fail-when #t "bad event expression syntax" #:attr expand #'#f])

 (define-syntax-class expanded
   #:description "REDUCE"
   #:attributes (reduce)
   #:commit
   #:literal-sets (abstract-literals)
   ;; eliminate single-element sequences
   [pattern (#%event-seq E:expanded) #:attr reduce #'E.reduce]
   [pattern (#%event-seq0 E:expanded) #:attr reduce #'E.reduce]
   ;; applicative functor laws
   [pattern (#%event-app (#%event-pure f) (#%event-pure x) ...)
            #:with e:expanded #'(#%event-pure (f x ...))
            #:attr reduce #'e.reduce]
   [pattern (#%event-app (#%event-pure f) (#%event-pure x) E ...)
            #:with e:expanded #'(#%event-app (#%event-curry f x) E ...)
            #:attr reduce #'e.reduce]
   [pattern (#%event-app (#%event-pure f) E ...)
            #:with e:expanded #'(#%event-fmap f E ...)
            #:attr reduce #'e.reduce]
   [pattern (#%event-app (#%event-curry f x ...) (#%event-pure y) E ...)
            #:with e:expanded #'(#%event-app (#%event-curry f x ... y) E ...)
            #:attr reduce #'e.reduce]
   [pattern (#%event-app (#%event-curry f x ...) E:expanded ...)
            #:attr reduce #'(#%event-fmap (curry f x ...) E.reduce ...)]
   ;; racket/base
   [pattern (#%event-let-values ~! ([xs Vs:expanded] ...) E:expanded)
            #:attr reduce #'(#%event-let-values ([xs Vs.reduce] ...) E.reduce)]
   [pattern (#%event-lambda ~! xs E:expanded) #:attr reduce #'(#%event-lambda xs E.reduce)]
   [pattern (#%event-case-lambda ~! [xs E:expanded] ...)
            #:attr reduce #'(#%event-case-lambda [xs E.reduce] ...)]
   ;; event/sequential
   [pattern (#%event-args ~! E:expanded ...) #:attr reduce #'(#%event-args E.reduce ...)]
   [pattern (#%event-fmap ~! f E:expanded ...) #:attr reduce #'(#%event-fmap f E.reduce ...)]
   [pattern (#%event-app ~! F:expanded E:expanded ...)
            #:attr reduce #'(#%event-app F.reduce E.reduce ...)]
   [pattern (#%event-bind ~! f E:expanded ...) #:attr reduce #'(#%event-bind f E.reduce ...)]
   [pattern (#%event-seq ~! E:expanded ...+) #:attr reduce #'(#%event-seq E.reduce ...)]
   [pattern (#%event-seq0 ~! E:expanded ...+) #:attr reduce #'(#%event-seq0 E.reduce ...)]
   [pattern (#%event-test ~! E1:expanded E2:expanded E3:expanded)
            #:attr reduce #'(#%event-test E1.reduce E2.reduce E3.reduce)]
   [pattern (#%event-series ~! V:expanded f ...)
            #:attr reduce #'(#%event-series V.reduce f ...)]
   [pattern (#%event-reduce ~! f check v ...) #:attr reduce #'(#%event-reduce f check v ...)]
   [pattern (#%event-loop ~! f v ...) #:attr reduce #'(#%event-loop f v ...)]
   ;; event/concurrent
   [pattern (#%event-async-set ~! E:expanded ...)
            #:attr reduce #'(#%event-async-set E.reduce ...)]
   [pattern (#%event-async-args ~! E:expanded ...)
            #:attr reduce #'(#%event-async-args E.reduce ...)]
   [pattern (#%event-async-fmap ~! f E:expanded ...)
            #:attr reduce #'(#%event-async-fmap f E.reduce ...)]
   [pattern (#%event-async-app ~! F:expanded E:expanded ...)
            #:attr reduce #'(#%event-async-app F.reduce E.reduce ...)]
   [pattern (#%event-async-bind ~! f E:expanded ...)
            #:attr reduce #'(#%event-async-bind f E.reduce ...)]
   ;; default
   [pattern _ #:attr reduce this-syntax])

 (define (reduce-event stx)
   (syntax-parse stx
     [e:expanded
      (let ([stx* #'e.reduce])
        ;; (pretty-write
        ;;  `(XXX
        ;;    ,(syntax->datum stx)
        ;;    ,(syntax->datum stx*)))
        (if (equal? (syntax->datum stx)
                    (syntax->datum stx*))
            stx*
            (reduce-event stx*)))]))

 (define-syntax-class reduced
   #:description "REALIZE"
   #:attributes (realize)
   #:commit
   #:literal-sets (abstract-literals)
   ;; event
   [pattern (#%event-esc ~! datum) #:attr realize #'datum]
   ;; racket/base
   [pattern (#%event-let-values () ~! E:reduced) #:attr realize #'E.realize]
   [pattern (#%event-let-values ~! ([xs Vs:reduced] ys ...) E:reduced)
            #:with e:reduced #'(#%event-let-values (ys ...) E)
            #:attr realize #'(bind (λ xs e.realize) Vs.realize)]
   [pattern (#%event-app (#%event-lambda xs E:reduced) V:reduced ...)
            #:attr realize #'(bind (lambda xs E.realize) V.realize ...)]
   [pattern (#%event-app (#%event-case-lambda [xs E:reduced] ...) V:reduced ...)
            #:attr realize #'(bind (case-lambda [xs E.realize] ...) V.realize ...)]
   ;; event/sequential
   [pattern (#%event-pure ~! datum) #:attr realize #'(pure datum)]
   [pattern (#%event-return ~! e) #:attr realize #'(return e)]
   [pattern (#%event-args ~! E:reduced ...) #:attr realize #'(args E.realize ...)]
   [pattern (#%event-fmap ~! f E:reduced ...) #:attr realize #'(fmap f E.realize ...)]
   [pattern (#%event-app ~! F:reduced E:reduced ...)
            #:attr realize #'(app F.realize E.realize ...)]
   [pattern (#%event-bind ~! f E:reduced ...) #:attr realize #'(bind f E.realize ...)]
   [pattern (#%event-seq ~! E:reduced ...+) #:attr realize #'(seq E.realize ...)]
   [pattern (#%event-seq0 ~! E:reduced ...+) #:attr realize #'(seq0 E.realize ...)]
   [pattern (#%event-test ~! E1:reduced E2:reduced E3:reduced)
            #:attr realize #'(test E1.realize E2.realize E3.realize)]
   [pattern (#%event-series ~! V:reduced f ...)
            #:attr realize #'(series V.realize (compose return f) ...)]
   [pattern (#%event-reduce ~! f check v ...)
            #:attr realize #'(reduce (compose return f) check v ...)]
   [pattern (#%event-loop ~! f v ...) #:attr realize #'(loop (compose return f) v ...)]
   ;; event/concurrent
   [pattern (#%event-async-set ~! E:reduced ...) #:attr realize #'(async-set E.realize ...)]
   [pattern (#%event-async-args ~! E:reduced ...) #:attr realize #'(async-args E.realize ...)]
   [pattern (#%event-async-fmap ~! f E:reduced ...)
            #:attr realize #'(async-fmap (compose return f) E.realize ...)]
   [pattern (#%event-async-app ~! F:reduced E:reduced ...)
            #:attr realize #'(async-app F.realize E.realize ...)]
   [pattern (#%event-async-bind ~! f E:reduced ...)
            #:attr realize #'(async-bind f E.realize ...)])

 (define (make-event stx-0 debugging?)
   (define stx-1 (syntax-parse stx-0 [e:event #'e.expand]))
   (define stx-2 (reduce-event stx-1))
   (define stx-3 (syntax-parse stx-2 [e:reduced #'e.realize]))
   (if debugging? (values stx-0 stx-1 stx-2 stx-3) stx-3)))

(define-syntax (event stx)
  (syntax-parse stx
    [(_ e ...+) (make-event #'(seq e ...) #f)]))

(define-syntax (event-do stx)
  (syntax-parse stx
    [(_ e ...+) #`(sync #,(make-event #'(seq e ...) #f))]))

(define-syntax (event-print stx)
  (syntax-parse stx
    [(_ e ...+)
     (pretty-write (syntax->datum (make-event #'(seq e ...) #f)))
     #'(values)]))

(define-syntax (event-debug stx)
  (syntax-parse stx
    [(_ e ...+)
     (define-values (stx-0 stx-1 stx-2 stx-3) (make-event #'(seq e ...) #t))
     (displayln "-- INPUT --\n") (pretty-write (syntax->datum stx-0))
     (displayln "\n-- EXPAND --\n") (pretty-write (syntax->datum stx-1))
     (displayln "\n-- REDUCE --\n") (pretty-write (syntax->datum stx-2))
     (displayln "\n-- FINAL --\n") (pretty-write (syntax->datum stx-3))
     (displayln "\n-- RESULT --\n") #`(pretty-write (sync #,stx-3))]))

;;; Unit Tests

(module+ test
  (require rackunit)

  ;; EXPAND

  (define-syntax (check-expand stx)
    (syntax-parse stx
      [(_ e:event v)
       #'(check equal?
                (syntax->datum #'e.expand)
                (syntax->datum #'v))]))

  (test-case
    "expand esc"
    (check-expand (esc 1) (#%event-esc 1)))

  (test-case
    "expand begin"
    (check-expand (begin 1) (#%event-seq (#%event-pure 1)))
    (check-expand
     (begin 1 2 3)
     (#%event-seq
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "expand begin0"
    (check-expand (begin0 1) (#%event-seq0 (#%event-pure 1)))
    (check-expand
     (begin0 1 2 3)
     (#%event-seq0
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "expand let"
    (check-expand (let () 1) (#%event-let-values () (#%event-seq (#%event-pure 1))))
    (check-expand
     (let ([x 1] [y 2]) (+ x y))
     (#%event-let-values
      ([(x) (#%event-pure 1)]
       [(y) (#%event-pure 2)])
      (#%event-seq
       (#%event-app
        (#%event-pure +)
        (#%event-pure x)
        (#%event-pure y))))))

  (test-case
    "expand let*"
    (check-expand
     (let* () 1)
     (#%event-let-values () (#%event-seq (#%event-pure 1))))
    (check-expand
     (let* ([x 1] [y 2]) (+ x y))
     (#%event-let-values
      ([(x) (#%event-pure 1)]
       [(y) (#%event-pure 2)])
      (#%event-seq
       (#%event-app
        (#%event-pure +)
        (#%event-pure x)
        (#%event-pure y))))))

  (test-case
    "expand let-values"
    (check-expand
     (let-values () 1)
     (#%event-let-values () (#%event-seq (#%event-pure 1))))
    (check-expand
     (let-values
         ([(x1 x2) (values 1 2)]
          [ys (f x1 x2)])
       3 4 5)
     (#%event-let-values
      ([(x1 x2) (#%event-app
                 (#%event-pure values)
                 (#%event-pure 1)
                 (#%event-pure 2))]
       [ys (#%event-app
            (#%event-pure f)
            (#%event-pure x1)
            (#%event-pure x2))])
      (#%event-seq
       (#%event-pure 3)
       (#%event-pure 4)
       (#%event-pure 5)))))

  (test-case
    "expand lambda"
    (check-expand (lambda _ 1) (#%event-pure (lambda _ 1)))
    (check-expand (lambda (x y) 1 2 3) (#%event-pure (lambda (x y) 1 2 3))))

  (test-case
    "expand λ"
    (check-expand (λ _ 1) (#%event-pure (λ _ 1)))
    (check-expand (λ (x y) 1 2 3) (#%event-pure (λ (x y) 1 2 3))))

  (test-case
    "expand case-lambda"
    (check-expand (case-lambda) (#%event-pure (case-lambda)))
    (check-expand
     (case-lambda [() 0] [(x) 1] [(y z) 2] [_ -1])
     (#%event-pure (case-lambda [() 0] [(x) 1] [(y z) 2] [_ -1]))))

  (test-case
    "expand if"
    (check-expand
     (if 1 2 3)
     (#%event-test (#%event-pure 1) (#%event-pure 2) (#%event-pure 3))))

  (test-case
    "expand quote"
    (check-expand (quote 1) (#%event-pure '1))
    (check-expand (quote (1 2 3)) (#%event-pure '(1 2 3))))

  (test-case
    "expand lambda app"
    (check-expand
     ((lambda _ 1) 2)
     (#%event-app (#%event-lambda _ (#%event-seq (#%event-pure 1)))
                  (#%event-pure 2)))
    (check-expand
     ((lambda (x y) (+ x y)) 1 2)
     (#%event-app
      (#%event-lambda
       (x y)
       (#%event-seq
        (#%event-app
         (#%event-pure +)
         (#%event-pure x)
         (#%event-pure y))))
      (#%event-pure 1)
      (#%event-pure 2))))

  (test-case
    "expand λ app"
    (check-expand
     ((λ _ 1) 2)
     (#%event-app (#%event-lambda _ (#%event-seq (#%event-pure 1)))
                   (#%event-pure 2)))
    (check-expand
     ((λ (x y) (+ x y)) 1 2)
     (#%event-app
      (#%event-lambda
       (x y)
       (#%event-seq
        (#%event-app
         (#%event-pure +)
         (#%event-pure x)
         (#%event-pure y))))
      (#%event-pure 1)
      (#%event-pure 2))))

  (test-case
    "expand case-lambda app"
    (check-expand ((case-lambda)) (#%event-app (#%event-case-lambda)))
    (check-expand
     ((case-lambda [() 0] [(x) 1] [(y z) 2] [_ -1]) 1 2 3)
     (#%event-app
      (#%event-case-lambda
       [() (#%event-seq (#%event-pure 0))]
       [(x) (#%event-seq (#%event-pure 1))]
       [(y z) (#%event-seq (#%event-pure 2))]
       [_ (#%event-seq (#%event-pure -1))])
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "expand <string>"
    (check-expand "a" (#%event-pure "a")))

  (test-case
    "expand <boolean>"
    (check-expand #t (#%event-pure #t))
    (check-expand #f (#%event-pure #f)))

  (test-case
    "expand <number>"
    (check-expand 1 (#%event-pure 1)))

  (test-case
    "expand pure"
    (check-expand (pure 1) (#%event-pure 1))
    (check-expand (pure (+ 1 2)) (#%event-pure (+ 1 2))))

  (test-case
    "expand return"
    (check-expand (return 1) (#%event-return 1))
    (check-expand (return (+ 1 2)) (#%event-return (+ 1 2))))

  (test-case
    "expand args"
    (check-expand (args) (#%event-args))
    (check-expand
     (args 1 2 3)
     (#%event-args
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "expand fmap"
    (check-expand (fmap +) (#%event-fmap +))
    (check-expand
     (fmap + 1 2 3)
     (#%event-fmap
      +
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "expand app"
    (check-expand (app +) (#%event-app (#%event-pure +)))
    (check-expand
     (app + 1 2 3)
     (#%event-app
      (#%event-pure +)
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "expand bind"
    (check-expand (bind f) (#%event-bind f))
    (check-expand
     (bind (compose return +) 1 2 3)
     (#%event-bind
      (compose return +)
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "expand seq"
    (check-expand (seq 1) (#%event-seq (#%event-pure 1)))
    (check-expand
     (seq 1 2 3)
     (#%event-seq
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "expand seq0"
    (check-expand (seq0 1) (#%event-seq0 (#%event-pure 1)))
    (check-expand
     (seq0 1 2 3)
     (#%event-seq0
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "expand test"
    (check-expand
     (test #t 1 2)
     (#%event-test
      (#%event-pure #t)
      (#%event-pure 1)
      (#%event-pure 2))))

  (test-case
    "expand series"
    (check-expand (series 1) (#%event-series (#%event-pure 1)))
    (check-expand
     (series 1 add1 (λ (x) (+ x 2)))
     (#%event-series (#%event-pure 1) add1 (λ (x) (+ x 2)))))

  (test-case
    "expand reduce"
    (check-expand (reduce f = 0) (#%event-reduce f = 0))
    (check-expand
     (reduce sub1 (curry = 0) 10)
     (#%event-reduce sub1 (curry = 0) 10)))

  (test-case
    "expand loop"
    (check-expand (loop add1 0) (#%event-loop add1 0)))

  (test-case
    "expand async-set"
    (check-expand (async-set) (#%event-async-set))
    (check-expand
     (async-set 1 2 3)
     (#%event-async-set
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "expand async-args"
    (check-expand (async-args) (#%event-async-args))
    (check-expand
     (async-args 1 2 3)
     (#%event-async-args
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "expand async-fmap"
    (check-expand (async-fmap f) (#%event-async-fmap f))
    (check-expand
     (async-fmap f 1 2 3)
     (#%event-async-fmap
      f
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "expand async-app"
    (check-expand (async-app f) (#%event-async-app (#%event-pure f)))
    (check-expand
     (async-app f 1 2 3)
     (#%event-async-app
      (#%event-pure f)
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "expand async-bind"
    (check-expand (async-bind f) (#%event-async-bind f))
    (check-expand
     (async-bind f 1 2 3)
     (#%event-async-bind
      f
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "expand <identifier>"
    (check-expand x (#%event-pure x)))

  (test-case
    "expand <app>"
    (check-expand
     (+ 1 2 3)
     (#%event-app
      (#%event-pure +)
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  ;; REDUCE

  (define-syntax (check-reduce stx)
    (syntax-parse stx
      [(_ e:event v)
       (define e* (syntax-parse #'e.expand [e** (reduce-event #'e**)]))
       #`(check equal?
                (syntax->datum #'#,e*)
                (syntax->datum #'v))]))

  (test-case
    "reduce single-element sequence"
    (check-reduce (seq 1) (#%event-pure 1))
    (check-reduce (seq0 1) (#%event-pure 1)))

  (test-case
    "reduce curry"
    (check-reduce curry (#%event-pure curry))
    (check-reduce (curry +) (#%event-pure (curry +)))
    (check-reduce (curry + 1 2) (#%event-pure (curry + 1 2)))
    (check-reduce ((curry + 1) 2) (#%event-pure ((curry + 1) 2)))
    (check-reduce (app (pure f)) (#%event-pure (f))))

  (test-case
    "reduce #%event-let-values"
    (check-reduce (let-values () 1) (#%event-let-values () (#%event-pure 1)))
    (check-reduce
     (let ([x 1] [y (+ 2 3)]) (- x y))
     (#%event-let-values
      ([(x) (#%event-pure 1)]
       [(y) (#%event-pure (+ 2 3))])
      (#%event-pure (- x y)))))

  (test-case
    "reduce #%event-lambda"
    (check-reduce ((λ _ 1)) (#%event-app (#%event-lambda _ (#%event-pure 1))))
    (check-reduce
     ((λ (x y) (+ x y)) 1 2)
     (#%event-app
      (#%event-lambda (x y) (#%event-pure (+ x y)))
      (#%event-pure 1)
      (#%event-pure 2))))

  (test-case
    "reduce #%event-case-lambda"
    (check-reduce ((case-lambda)) (#%event-app (#%event-case-lambda)))
    (check-reduce
     ((case-lambda [() 1] [_ -1]) 2)
     (#%event-app
      (#%event-case-lambda
       [() (#%event-pure 1)]
       [_ (#%event-pure -1)])
      (#%event-pure 2))))

  (test-case
    "reduce args"
    (check-reduce (args) (#%event-args))
    (check-reduce
     (args 1 2 3)
     (#%event-args
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "reduce fmap"
    (check-reduce (fmap +) (#%event-fmap +))
    (check-reduce
     (fmap + 1 2 3)
     (#%event-fmap
      +
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "reduce app"
    (check-reduce (app f) (#%event-pure (f)))
    (check-reduce (app f 1 2 3) (#%event-pure (f 1 2 3))))

  (test-case
    "reduce bind"
    (check-reduce (bind f) (#%event-bind f))
    (check-reduce
     (bind f 1 2 3)
     (#%event-bind
      f
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "reduce seq"
    (check-reduce (seq 1) (#%event-pure 1))
    (check-reduce
     (seq 1 2 3)
     (#%event-seq
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "reduce seq0"
    (check-reduce (seq0 1) (#%event-pure 1))
    (check-reduce
     (seq0 1 2 3)
     (#%event-seq0
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "reduce test"
    (check-reduce
     (test 1 2 3)
     (#%event-test
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "reduce series"
    (check-reduce (series 1) (#%event-series (#%event-pure 1)))
    (check-reduce
     (series 1 add1 (λ (x) (+ x 2)))
     (#%event-series (#%event-pure 1) add1 (λ (x) (+ x 2)))))

  (test-case
   "reduce reduce"
   (check-reduce (reduce f = 1) (#%event-reduce f = 1))
   (check-reduce
    (reduce sub1 (λ (_ x) (= x 1)) 5)
    (#%event-reduce sub1 (λ (_ x) (= x 1)) 5)))

  (test-case
    "reduce loop"
    (check-reduce (loop f 1) (#%event-loop f 1))
    (check-reduce (loop f 1 2 3) (#%event-loop f 1 2 3)))

  (test-case
    "reduce async-set"
    (check-reduce (async-set) (#%event-async-set))
    (check-reduce
     (async-set 1 (+ 2 3))
     (#%event-async-set
      (#%event-pure 1)
      (#%event-pure (+ 2 3)))))

  (test-case
    "reduce async-args"
    (check-reduce (async-args) (#%event-async-args))
    (check-reduce
     (async-args 1 2 3)
     (#%event-async-args
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "reduce async-fmap"
    (check-reduce (async-fmap f) (#%event-async-fmap f))
    (check-reduce
     (async-fmap f 1 2 3)
     (#%event-async-fmap
      f
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "reduce async-app"
    (check-reduce (async-app f) (#%event-async-app (#%event-pure f)))
    (check-reduce
     (async-app f 1 2 3)
     (#%event-async-app
      (#%event-pure f)
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "reduce async-bind"
    (check-reduce (async-bind f) (#%event-async-bind f))
    (check-reduce
     (async-bind f 1 2 3)
     (#%event-async-bind
      f
      (#%event-pure 1)
      (#%event-pure 2)
      (#%event-pure 3))))

  (test-case
    "reduce <default>"
    (check-reduce 1 (#%event-pure 1))
    (check-reduce (esc 1) (#%event-esc 1)))

  ;; REALIZE

  (define-syntax (check-realize stx)
    (syntax-parse stx
      [(_ e:event v)
       (define e* (syntax-parse #'e.expand
                    [e:expanded (syntax-parse #'e.reduce
                                  [e:reduced #'e.realize])]))
       #`(check equal?
                (syntax->datum #'#,e*)
                (syntax->datum #'v))]))

  (test-case
    "realize #%event-esc"
    (check-realize (esc 1) 1))

  (test-case
    "realize #%event-let-values"
    (check-realize (let () 1) (pure 1))
    (check-realize
     (let ([x 1] [y 2]) (+ x y))
     (bind (λ (x) (bind (λ (y) (pure (+ x y))) (pure 2))) (pure 1))))

  (test-case
    "realize #%event-lambda app"
    (check-realize ((λ _ 1)) (bind (lambda _ (pure 1))))
    (check-realize
     ((lambda (x y) (+ x y)) 1 2)
     (bind (lambda (x y) (pure (+ x y)))
           (pure 1)
           (pure 2))))

  (test-case
    "realize #%event-case-lambda app"
    (check-realize ((case-lambda)) (bind (case-lambda)))
    (check-realize
     ((case-lambda [(x) 1] [_ -1]) 2 3)
     (bind (case-lambda [(x) (pure 1)] [_ (pure -1)])
           (pure 2)
           (pure 3))))

  (test-case
    "realize #%event-pure"
    (check-realize 1 (pure 1)))

  (test-case
    "realize #%event-return"
    (check-realize (return 1) (return 1))
    (check-realize (return (+ 1 2)) (return (+ 1 2))))

  (test-case
    "realize #%event-args"
    (check-realize (args) (args))
    (check-realize
     (args 1 2 3)
     (args (pure 1)
           (pure 2)
           (pure 3))))

  (test-case
    "realize #%event-fmap"
    (check-realize (fmap (f x)) (fmap (f x)))
    (check-realize
     (fmap + 1 2 3)
     (fmap + (pure 1) (pure 2) (pure 3))))

  (test-case
    "realize #%event-app"
    (check-realize (app +) (pure (+)))
    (check-realize (app + 1 2 3) (pure (+ 1 2 3)))
    (check-realize
     (app + 1 (app + 2 3))
     (fmap (curry + 1) (pure (+ 2 3)))))

  (test-case
    "realize #%event-bind"
    (check-realize (bind f) (bind f))
    (check-realize
     (bind (compose return +) 1 2 3)
     (bind
      (compose return +)
      (pure 1)
      (pure 2)
      (pure 3))))

  (test-case
    "realize #%event-seq"
    (check-realize (seq 1) (pure 1))
    (check-realize
     (seq 1 2 3)
     (seq
      (pure 1)
      (pure 2)
      (pure 3))))

  (test-case
    "realize #%event-seq0"
    (check-realize (seq0 1) (pure 1))
    (check-realize
     (seq0 1 2 3)
     (seq0
      (pure 1)
      (pure 2)
      (pure 3))))

  (test-case
    "realize #%event-test"
    (check-realize (test A B C) (test (pure A) (pure B) (pure C)))
    (check-realize
     (test (+ 1 2) (- 3 4) (* 5 6))
     (test
      (pure (+ 1 2))
      (pure (- 3 4))
      (pure (* 5 6)))))

  (test-case
    "realize #%event-series"
    (check-realize (series 1) (series (pure 1)))
    (check-realize
     (series 1 f g)
     (series
      (pure 1)
      (compose return f)
      (compose return g))))

  (test-case
    "realize #%event-reduce"
    (check-realize (reduce f = 0) (reduce (compose return f) = 0))
    (check-realize (reduce f = 1 2 3) (reduce (compose return f) = 1 2 3)))

  (test-case
    "realize #%event-loop"
    (check-realize (loop f 0) (loop (compose return f) 0))
    (check-realize (loop f 1 2 3) (loop (compose return f) 1 2 3)))

  (test-case
    "realize #%event-async-set"
    (check-realize (async-set) (async-set))
    (check-realize
     (async-set 1 2 3) (async-set (pure 1) (pure 2) (pure 3))))

  (test-case
    "realize #%event-async-args"
    (check-realize (async-args) (async-args))
    (check-realize
     (async-args 1 2 3)
     (async-args (pure 1) (pure 2) (pure 3))))

  (test-case
    "realize #%event-async-fmap"
    (check-realize (async-fmap f) (async-fmap (compose return f)))
    (check-realize
     (async-fmap f 1 2 3)
     (async-fmap (compose return f) (pure 1) (pure 2) (pure 3))))

  (test-case
    "realize #%event-async-app"
    (check-realize (async-app f) (async-app (pure f)))
    (check-realize
     (async-app f 1 2 3)
     (async-app (pure f) (pure 1) (pure 2) (pure 3))))

  (test-case
    "realize #%event-async-bind"
    (check-realize (async-bind f) (async-bind f))
    (check-realize
     (async-bind f 1 2 3)
     (async-bind f (pure 1) (pure 2) (pure 3))))

  ;; MAKE

  (test-case
    "event-do args"
    (check equal?
           (call-with-values (λ () (event-do (args 1 2 3))) list)
           '(1 2 3)))

  (test-case
    "event-do fmap"
    (check = (event-do (fmap + 1 2 3)) 6))

  (test-case
    "event-do app"
    (check = (event-do (app + 1 2 3)) 6))

  (test-case
    "event-do bind"
    (check = (event-do (bind (compose return +) 1 2 3)) 6))

  (test-case
    "event-do seq"
    (check = (event-do (seq 1 2 3)) 3))

  (test-case
    "event-do seq0"
    (check = (event-do (seq0 1 2 3)) 1))

  (test-case
    "event-do test"
    (check = (event-do (test #t 1 2)) 1)
    (check = (event-do (test #f 1 2)) 2)))
