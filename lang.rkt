#lang racket

(require redex)

(define-language
 PEG
 (terminal ::= natural)
 (s ::= variable)
 (e ::=
    terminal
    x
    ε
    (• e e)
    (/ e e)
    (* e)
    (! e))
 (g ::= (x e g) ∅)
 (x ::= variable-not-otherwise-mentioned))

(define-extended-language
  LPEG PEG
  (l ::= integer)
  (ch ::= natural)
  (i ::=
     (char ch)
     (jump l)
     (choice l)
     (opencall x)
     (call l)
     return
     emp
     (commit l)
     capture
     fail
     failure
     end)
  (b ::= (x ilist)) ;; block
  (blist ::= (b ...)) ;; grammar
  (ilist ::= (i ...)))

(provide (all-defined-out))
