#lang racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))

(define-syntax never (make-rename-transformer #'never-evt))
(define-syntax always (make-rename-transformer #'always-evt))
(define-syntax choice (make-rename-transformer #'choice-evt))
(define-syntax handle (make-rename-transformer #'handle-evt))
(define-syntax replace (make-rename-transformer #'replace-evt))
