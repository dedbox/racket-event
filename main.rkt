#lang racket/base

(provide
 event event-do event-print event-debug
 (all-from-out event/monad))

(require
 event/monad
 racket/function
 racket/pretty
 syntax/parse
 (for-syntax racket/base
             racket/pretty
             syntax/parse))

(define (#%event-esc stx)
  (raise-syntax-error #f "cannot evaluate abastract syntax" stx))

(define (#%event-pure stx)
  (raise-syntax-error #f "cannot evaluate abastract syntax" stx))

(define (#%event-return stx)
  (raise-syntax-error #f "cannot evaluate abastract syntax" stx))

(define (#%event-args stx)
  (raise-syntax-error #f "cannot evaluate abastract syntax" stx))

(define (#%event-fmap stx)
  (raise-syntax-error #f "cannot evaluate abastract syntax" stx))

(define (#%event-app stx)
  (raise-syntax-error #f "cannot evaluate abastract syntax" stx))

(define (#%event-bind stx)
  (raise-syntax-error #f "cannot evaluate abastract syntax" stx))

(define (#%event-seq stx)
  (raise-syntax-error #f "cannot evaluate abastract syntax" stx))

(define (#%event-seq0 stx)
  (raise-syntax-error #f "cannot evaluate abastract syntax" stx))

(define (#%event-test stx)
  (raise-syntax-error #f "cannot evaluate abastract syntax" stx))

(define (#%event-curry stx)
  (raise-syntax-error #f "cannot evaluate abastract syntax" stx))

(begin-for-syntax
 (define-literal-set abstract-literals
   (#%event-esc
    #%event-pure
    #%event-return
    #%event-args
    #%event-fmap
    #%event-app
    #%event-bind
    #%event-seq
    #%event-seq0
    #%event-test
    #%event-curry))

 (define-literal-set monad-literals
   (pure return args fmap app bind seq seq0 test series reduce loop))

 (define monad-literal-ids
   (list #'pure #'return #'args #'fmap #'app #'bind #'seq #'seq0 #'test
         #'series #'reduce #'loop))

 (define-syntax-class event
   #:description "EXPAND"
   #:attributes (expand)
   #:commit
   #:literal-sets (monad-literals)
   #:datum-literals (esc)
   [pattern (esc ~! datum) #:attr expand #'(#%event-esc datum)]
   [pattern (pure ~! datum) #:attr expand #'(#%event-pure (#%event-pure datum))]
   [pattern (return ~! e) #:attr expand #'(#%event-return e)]
   [pattern (args ~! E:event ...) #:attr expand #'(#%event-args E.expand ...)]
   [pattern (fmap ~! f E:event ...) #:attr expand #'(#%event-fmap f E.expand ...)]
   [pattern (app ~! E:event ...+) #:attr expand #'(#%event-app E.expand ...)]
   [pattern (bind ~! f E:event ...) #:attr expand #'(#%event-bind f E.expand ...)]
   [pattern (seq ~! E:event ...+) #:attr expand #'(#%event-seq E.expand ...)]
   [pattern (seq0 ~! E:event ...+) #:attr expand #'(#%event-seq0 E.expand ...)]
   [pattern (test ~! E1:event E2:event E3:event)
            #:attr expand #'(#%event-test E1.expand E2.expand E3.expand)]
   [pattern v:string #:attr expand #'(#%event-pure v)]
   [pattern v:boolean #:attr expand #'(#%event-pure v)]
   [pattern v:number #:attr expand #'(#%event-pure v)]
   [pattern x:id #:attr expand #'(#%event-pure x)]
   [pattern (E:event ...+) #:attr expand #'(#%event-app E.expand ...)]
   [pattern _ #:fail-when #t "bad event expression syntax" #:attr expand #'#f])

 (define-syntax-class expanded
   #:description "REDUCE"
   #:attributes (reduce)
   #:commit
   #:literal-sets (abstract-literals)
   #:literals (curry)
   ;; eliminate single-element sequences
   [pattern (#%event-seq E:expanded) #:attr reduce #'E.reduce]
   [pattern (#%event-seq0 E:expanded) #:attr reduce #'E.reduce]
   ;; currying
   [pattern (#%event-app (#%event-pure curry) (#%event-pure f) x ...)
            #:with E:expanded #'(#%event-app (#%event-curry f) x ...)
            #:attr reduce #'E.reduce]
   [pattern (#%event-app (#%event-pure (curry f x ...))
                         (#%event-pure y)
                         ~! z ...)
            #:with E:expanded #'(#%event-app (#%event-curry f x ... y) z ...)
            #:attr reduce #'E.reduce]
   ;; [pattern (#%event-app (#%event-curry f x ...))
   ;;          #:attr reduce #'(#%event-pure (f x ...))]
   ;; [pattern (#%event-app (#%event-pure f) (#%event-pure x) ...)
   ;;          #:attr reduce #'(#%event-pure (f x ...))]
   ;; [pattern (#%event-app (#%event-pure f) (#%event-pure x) ~! y ...+)
   ;;          #:with E:expanded #'(#%event-app (#%event-curry f x) y ...)
   ;;          #:attr reduce #'E.reduce]
   [pattern (#%event-app (#%event-curry f x ...) (#%event-pure y) ~! z ...)
            #:with E:expanded #'(#%event-app (#%event-curry f x ... y) z ...)
            #:attr reduce #'E.reduce]
   [pattern (#%event-app (#%event-curry f x ...))
            #:attr reduce #'(#%event-pure (curry f x ...))]
   [pattern (#%event-app (#%event-curry f x ...) y:expanded ...+)
            #:with E:expanded
            #'(#%event-app (#%event-pure (curry f x ...)) y.reduce ...)
            #:attr reduce #'E.reduce]
   ;; recur
   [pattern (#%event-args ~! E:expanded ...) #:attr reduce #'(#%event-args E.reduce ...)]
   [pattern (#%event-fmap ~! f E:expanded ...) #:attr reduce #'(#%event-fmap f E.reduce ...)]
   [pattern (#%event-app ~! E:expanded ...+) #:attr reduce #'(#%event-app E.reduce ...)]
   [pattern (#%event-bind ~! f E:expanded ...) #:attr reduce #'(#%event-bind f E.reduce ...)]
   [pattern (#%event-seq ~! E:expanded ...+) #:attr reduce #'(#%event-seq E.reduce ...)]
   [pattern (#%event-seq0 ~! E:expanded ...+) #:attr reduce #'(#%event-seq0 E.reduce ...)]
   [pattern (#%event-test ~! E1:expanded E2:expanded E3:expanded)
            #:attr reduce #'(#%event-test E1.reduce E2.reduce E3.reduce)]
   ;; default
   [pattern _ #:attr reduce this-syntax])

 (define-syntax-class reduced
   #:description "REALIZE"
   #:attributes (realize)
   #:commit
   #:literal-sets (abstract-literals)
   [pattern (#%event-esc ~! datum) #:attr realize #'datum]
   [pattern (#%event-pure ~! datum) #:attr realize #'(pure datum)]
   [pattern (#%event-return ~! e) #:attr realize #'(return e)]
   [pattern (#%event-args ~! E:reduced ...) #:attr realize #'(args E.realize ...)]
   [pattern (#%event-fmap ~! f E:reduced ...) #:attr realize #'(fmap f E.realize ...)]
   [pattern (#%event-app ~! E:reduced ...+) #:attr realize #'(app E.realize ...)]
   [pattern (#%event-bind ~! f E:reduced ...) #:attr realize #'(bind f E.realize ...)]
   [pattern (#%event-seq ~! E:reduced ...+) #:attr realize #'(seq E.realize ...)]
   [pattern (#%event-seq0 ~! E:reduced ...+) #:attr realize #'(seq0 E.realize ...)]
   [pattern (#%event-test ~! E1:reduced E2:reduced E3:reduced)
            #:attr realize #'(test E1.realize E2.realize E3.realize)])

 (define (make-event stx-0 debugging?)
   (define stx-1 (syntax-parse stx-0 [e:event #'e.expand]))
   (define stx-2 (syntax-parse stx-1 [e:expanded #'e.reduce]))
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
    "expand pure"
    (check-expand (pure 1) (#%event-pure (#%event-pure 1)))
    (check-expand (pure (+ 1 2)) (#%event-pure (#%event-pure (+ 1 2)))))

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
     (#%event-fmap +
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
       (define e* (syntax-parse #'e.expand [e:expanded #'e.reduce]))
       #`(check equal?
                (syntax->datum #'#,e*)
                (syntax->datum #'v))]))

  (test-case
    "reduce single-element sequence"
    (check-reduce (seq 1) (#%event-pure 1))
    (check-reduce (seq0 1) (#%event-pure 1)))

  (test-case
    "reduce currying"
    (check-reduce (curry +) (#%event-pure (curry +)))
    (check-reduce
     ((curry + 1) 2)
     (#%event-app
      (#%event-pure (curry + 1))
      (#%event-pure 2))))

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
     (fmap +
           (pure 1)
           (pure 2)
           (pure 3))))

  (test-case
    "realize #%event-app"
    (check-realize (app +) (app (pure +)))
    (check-realize
     (app + 1 2 3)
     (app
      (pure +)
      (pure 1)
      (pure 2)
      (pure 3))))

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
      (app (pure +) (pure 1) (pure 2))
      (app (pure -) (pure 3) (pure 4))
      (app (pure *) (pure 5) (pure 6)))))

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

