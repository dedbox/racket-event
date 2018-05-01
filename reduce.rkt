#lang racket/base

(provide (for-syntax (all-defined-out)))

(require
 event/ast
 (for-syntax racket/base
             racket/function
             syntax/parse))

(begin-for-syntax
 (define-syntax-class event1
   #:description "reducing event"
   #:attributes (reduce)
   #:commit
   #:literal-sets (event-literals)

   ;; single-element sequence elimination

   [pattern (#%event-begin  x:event1) #:attr reduce #'x.reduce]
   [pattern (#%event-begin0 x:event1) #:attr reduce #'x.reduce]

   ;; currying

   [pattern (#%event-app (#%event-curry f x ...+))
            #:attr reduce
            #'(#%event-pure (f x ...))]

   [pattern (#%event-app (#%event-pure (curry f x ...+))
                         (#%event-pure y)
                         ~! z ...)
            #:with e:event1 #'(#%event-app (#%event-curry f x ... y) z ...)
            #:attr reduce #'e.reduce]

   [pattern (#%event-app (#%event-pure f) (#%event-pure x) ...)
            #:attr reduce
            #'(#%event-pure (f x ...))]

   [pattern (#%event-app (#%event-pure f) (#%event-pure x) ~! y ...+)
            #:with e:event1 #'(#%event-app (#%event-curry f x) y ...)
            #:attr reduce #'e.reduce]

   [pattern (#%event-app (#%event-curry f x ...+) (#%event-pure y) ~! z ...)
            #:with e:event1 #'(#%event-app (#%event-curry f x ... y) z ...)
            #:attr reduce #'e.reduce]

   [pattern (#%event-app (#%event-curry f x ...+) ~! y:event1 ...)
            #:with e:event1 #'(#%event-app
                               (#%event-pure (curry f x ...)) y.reduce ...)
            #:attr reduce #'e.reduce]

   ;; simple recursion

   [pattern (#%event-begin ~! e:event1 ...+)
            #:attr reduce
            #'(#%event-begin e.reduce ...)]

   [pattern (#%event-begin0 ~! e:event1 ...+)
            #:attr reduce
            #'(#%event-begin0 e.reduce ...)]

   [pattern (#%event-let-values ~! ([xs vs:event1] ...) e:event1 ...+)
            #:attr reduce
            #'(#%event-let-values ([xs vs.reduce] ...) e.reduce ...)]

   [pattern (#%event-lambda ~! xs e:event1 ...+)
            #:attr reduce
            #'(#%event-lambda xs e.reduce ...)]

   ;; [pattern (#%event-case-lambda ~! [xs e:event1 ...+] ...)
   ;;          #:attr reduce
   ;;          #'(#%event-case-lambda [xs e.reduce ...] ...)]

   [pattern (#%event-if ~! e1:event1 e2:event1 e3:event1)
            #:attr reduce
            #'(#%event-if e1.reduce e2.reduce e3.reduce)]

   [pattern (#%event-app ~! f:event1 x:event1 ...)
            #:attr reduce
            #'(#%event-app f.reduce x.reduce ...)]

   [pattern _ #:attr reduce this-syntax]))
