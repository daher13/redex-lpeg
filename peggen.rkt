#lang racket

(require redex)

(require "peg.rkt")

(define-extended-language PEGGEN PEG
  (g ::=  (x e g) ∅) ;; grammar
  (start ::= e) ;; start expression
  ;; (t ::= (x boolean (x ...))) ;; state type
  (t ::= any)
  (tlist ::= (t ...))
  (seq ::= (g start tlist)) ;; generated sequence
  )

(provide PEGGEN)
