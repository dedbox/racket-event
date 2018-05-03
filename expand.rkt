#lang racket/base

;; (provide
;;  (for-syntax (all-defined-out)))

;; (require
;;  event/ast
;;  (for-syntax event/ast
;;              racket/base
;;              syntax/parse))

;; (require (for-syntax racket/pretty))

;; (begin-for-syntax
;;  (define-literal-set racket-literals
;;    (begin begin0 let-values case-lambda if quote))

;;  (define-syntax-class input-expr
;;    #:description
;;    #:attributes (expand)
;;    [pattern _:racket-expr ])

;;  (define-syntax-class racket-expr-expander
;;    #:description #f
;;    #:attributes (expand)
;;    #:literal-sets (racket-literals))

;;  (define-syntax-class event0
;;    #:description "event"
;;    #:attributes (expand)
;;    #:commit
;;    #:literal-sets (racket-literals)

;;    ;; Racket

;;    [pattern (begin ~! e:event0 ...+)
;;             #:attr expand
;;             #'(#%event-begin e.expand ...)]

;;    [pattern (begin0 ~! e:event0 ...+)
;;             #:attr expand
;;             #'(#%event-begin0 e.expand ...)]

;;    [pattern (let-values ~! ([xs vs:event0] ...) e:event0 ...+)
;;             #:attr expand
;;             #`(#%event-let-values
;;                ([xs vs.expand] ...)
;;                (#%event-begin e.expand ...))]

;;    [pattern (quote ~! _)
;;             #:attr expand
;;             #`(#%event-pure #,this-syntax)]

;;    [pattern _
;;             #:with :event0
;;             #:fail-when (equal? )
;;             (let ()
;;               (define stx
;;                 (local-expand this-syntax 'expression event-ast-literal-ids))
;;               (pretty-write
;;                `(XXX ,(syntax->datum this-syntax)
;;                      ,(syntax->datum stx)))
;;               stx)]))








;;  (define-syntax-class expanded-event
;;    #:attributes (expand)

;;    [pattern (lambda ~! _ _ ...+)
;;             #:attr expand
;;             #`(#%event-pure #,this-syntax)]

;;    ;; [pattern (case-lambda ~! [(x:id ...) e ...+] ...)
;;    ;;          #:attr expand
;;    ;;          #`(#%event-pure #,this-syntax)]

;;    [pattern (if ~! e1:event0 e2:event0 e3:event0)
;;             #:attr expand
;;             #`(#%event-if e1.expand e2.expand e3.expand)]

;;    [pattern (#%app f:event0 ~! e:event0 ...)
;;             #:attr expand
;;             #'(#%event-app f.expand e.expand ...)]))

;; ;;    [pattern (#%app ((~or lambda λ) ~! xs e:event0 ...+) v:event0 ...)
;; ;;             #:attr expand
;; ;;             #'(#%event-app (#%event-lambda xs (#%event-begin e.expand ...))
;; ;;                            v.expand ...)]

;; ;;    [pattern x:id
;; ;;             #:attr expand
;; ;;             #'(#%event-pure x)]















;;  (define event-literal-ids
;;    (append event-ast-literal-ids event-monad-literal-ids))

;;  (define-syntax-class event0
;;    #:description "expanding event"
;;    #:attributes (expand)
;;    #:datum-literals (unquote)
;;    [pattern (unquote e) #:attr expand #'(#%event-esc e)]
;;    [pattern _
;;             #:with :expanded-event
;;             (let ()
;;               (define stx (local-expand this-syntax 'expression event-literal-ids))
;;               (writeln `(XXX ,(syntax->datum this-syntax)
;;                              ,(syntax->datum stx)))
;;               stx)])

;;  (define-syntax-class expanded-event
;;    #:description #f
;;    #:attributes (expand)
;;    #:commit
;;    #:literal-sets (racket-literals event-monad-literals)
;;    #:datum-literals (#%app lambda !)

;;    ;; monad

;;    ;; [pattern (pure ~! datum) #:attr expand #'(#%event-pure datum)]
;;    ;; [pattern (return ~! e:event0) #:attr expand #'(#%event-return e.expand)]
;;    ;; [pattern (args ~! e:event0 ...+) #:attr expand #'(#%event-args e.expand ...)]

;;    ;; default

;;    [pattern _
;;             #:fail-when #t (format "bad event expression")
;;             #:attr expand #'#f]))

;; racket@> (require syntax/kerncase)
;; racket@> (map syntax->datum (kernel-form-identifier-list))
;; X '(begin
;; X   begin0
;;     define-values
;;     define-syntaxes
;;     begin-for-syntax
;;     set!
;; X   let-values
;;     letrec-values
;;     #%plain-lambda
;; /   case-lambda
;; X   if
;; X   quote
;;     letrec-syntaxes+values
;;     with-continuation-mark
;;     #%expression
;;     #%plain-app
;;     #%top
;;     #%datum
;;     #%variable-reference
;;     module
;;     module*
;;     #%provide
;;     #%require
;;     #%declare)

;;; Unit Tests

;; (module+ test
;;   (require rackunit)

;;   (define-syntax (check-expanded stx)
;;     (syntax-parse stx
;;       [(_ e:event0 v)
;;        #`(check equal?
;;                 (syntax->datum #'e.expand)
;;                 (syntax->datum #'v))]))

;;   (test-case
;;     "expand begin"
;;     (check-expanded
;;      (begin 1 2 3)
;;      (#%event-begin
;;       (#%event-pure '1)
;;       (#%event-pure '2)
;;       (#%event-pure '3))))

;;   (test-case
;;     "expand begin0"
;;     (check-expanded
;;      (begin0 1 2 3)
;;      (#%event-begin0
;;       (#%event-pure '1)
;;       (#%event-pure '2)
;;       (#%event-pure '3))))

;;   (test-case
;;     "expand let"
;;     (check-expanded
;;      (let ([x (+ 5 7)]) (* x 2))
;;      (#%event-let-values
;;       ([(x) (#%event-app
;;              (#%event-pure +)
;;              (#%event-pure '5)
;;              (#%event-pure '7))])
;;       (#%event-begin
;;        (#%event-app
;;         (#%event-pure *)
;;         (#%event-pure x)
;;         (#%event-pure '2))))))

;;   (test-case
;;     "expand lambda"
;;     (check-expanded (lambda (x) x) (#%event-pure (lambda (x) x)))
;;     (check-expanded (λ (x) x) (#%event-pure (lambda (x) x))))

;;   ;; (test-case
;;   ;;   "expand case-lambda"
;;   ;;   (check-expanded
;;   ;;    (case-lambda
;;   ;;      [() (list 0 null)]
;;   ;;      [(x) (list 1 x)]
;;   ;;      [(x y) (list 2 x y)]
;;   ;;      [xs (list -1 xs)])
;;   ;;    (#%event-case-lambda
;;   ;;     [()
;;   ;;      (#%event-app
;;   ;;       (#%event-pure list)
;;   ;;       (#%event-pure '0)
;;   ;;       (#%event-pure null))]
;;   ;;     [(x)
;;   ;;      (#%event-app
;;   ;;       (#%event-pure list)
;;   ;;       (#%event-pure '1)
;;   ;;       (#%event-pure x))]
;;   ;;     [(x y)
;;   ;;      (#%event-app
;;   ;;       (#%event-pure list)
;;   ;;       (#%event-pure '2)
;;   ;;       (#%event-pure x)
;;   ;;       (#%event-pure y))]
;;   ;;     [xs
;;   ;;      (#%event-app
;;   ;;       (#%event-pure list)
;;   ;;       (#%event-pure '-1)
;;   ;;       (#%event-pure xs))])))

;;   (test-case
;;     "expand if"
;;     (check-expanded
;;      (if (> x 5) (- x 2) (+ x 3))
;;      (#%event-if
;;       (#%event-app (#%event-pure >) (#%event-pure x) (#%event-pure '5))
;;       (#%event-app (#%event-pure -) (#%event-pure x) (#%event-pure '2))
;;       (#%event-app (#%event-pure +) (#%event-pure x) (#%event-pure '3)))))

;;   (test-case
;;     "expand <id>"
;;     (let ([x #f]) (check-expanded x (#%event-pure x))))

;;   (test-case
;;     "expand <number>"
;;     (check-expanded 5 (#%event-pure '5)))

;;   (test-case
;;     "expand <boolean>"
;;     (check-expanded #t (#%event-pure '#t))
;;     (check-expanded #f (#%event-pure '#f)))

;;   (test-case
;;     "expand <string>"
;;     (check-expanded "a" (#%event-pure '"a")))

;;   (test-case
;;     "expand <symbol>"
;;     (check-expanded 'z (#%event-pure 'z)))

;;   (test-case
;;     "expand #%app lambda"
;;     (check-expanded
;;      ((λ (x) x) 3)
;;      (#%event-app
;;       (#%event-lambda (x) (#%event-begin (#%event-pure x)))
;;       (#%event-pure '3))))

;;   (test-case
;;     "expand #%app"
;;     (check-expanded
;;      (1 2 3)
;;      (#%event-app
;;       (#%event-pure '1)
;;       (#%event-pure '2)
;;       (#%event-pure '3)))))
