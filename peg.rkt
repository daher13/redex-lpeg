#lang racket

(require redex)

(define-language PEG
  (x ::= variable-not-otherwise-mentioned)
  (terminal ::= natural)
  (e ::=
     x
     terminal
     (• e e)
     (/ e e)
     (* e)
     (! e))
  (prod ::= (x e))
  (g ::= (prod ...)))

(provide PEG)
