#lang racket

(require redex)
(require "lpeg.rkt")

(define-language TypeSystemEq
  (pc ::= natural)
  (b ::=
  (term ::= boolean pc )
  (eq ::= (+ pc pc))
  )
