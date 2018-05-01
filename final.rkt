#lang racket/base

(provide (for-syntax (all-defined-out)))

(require
 event/ast
 event/monad
 (for-syntax racket/base
             syntax/parse))

(begin-for-syntax
 (define-syntax-class event2
   #:description "finalizing event"
   #:attributes (final)
   #:commit
   #:literal-sets (event-literals)

   [pattern (#%event-esc e) #:attr final #'e]

   [pattern (#%event-app (#%event-lambda ~! xs e) y:event2 ...)
            #:with f:event2 #'(#%event-lambda xs e)
            #:attr final
            #'(bind f.final y.final ...)]

   [pattern (#%event-pure ~! e) #:attr final #'(pure e)]
   [pattern (#%event-app ~! e:event2 ...+) #:attr final #'(app e.final ...)]
   [pattern (#%event-begin ~! e:event2 ...+) #:attr final #'(seq e.final ...)]
   [pattern (#%event-begin0 ~! e:event2 ...+) #:attr final #'(seq0 e.final ...)]

   [pattern (#%event-let-values () ~! e:event2)
            #:attr final
            #'e.final]
   
   [pattern (#%event-let-values ~! ([xs vs:event2] ys ...) e ...+)
            #:with ws:event2 #'(#%event-let-values (ys ...) e ...)
            #:attr final
            #'(bind (λ xs ws.final) vs.final)]

   [pattern (#%event-lambda ~! xs e:event2)
            #:attr final
            #'(lambda xs e.final)]

   ;; [pattern (#%event-case-lambda ~! [xs e:event2] ...)
   ;;          #:attr final
   ;;          #'(case-lambda [xs e.final] ...)]

   [pattern (#%event-if ~! e1:event2 e2:event2 e3:event2)
            #:attr final
            #'(test e1.final e2.final e3.final)]

   [pattern _ #:attr final this-syntax]))
