#lang racket

(require redex)

(define-language
 PEG
 (terminal ::= natural)
 (s ::= variable)
 (e ::=
    terminal
    x
    (@ e e)
    (+ e e)
    (* e)
    (! e))
 (prod ::= (x e))
 (grammar ::= (prod ...))
 (x ::= variable-not-otherwise-mentioned))

(define-extended-language
  LPEG PEG
  (l ::= integer x)
  (ch ::= natural)
  (i ::=
     (char ch)
     (jump l)
     (choice l)
     (call l)
     (opencall l)
     return
     (commit l)
     capture
     fail
     failure
     end)
  (ilist ::= (i ...))
  (b ::= (l ilist))
  (blist ::= (b ...)))

(provide (all-defined-out))
