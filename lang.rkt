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
 (g ::= (prod ...))
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
     (commit l)
     capture
     fail
     failure
     end)
  (b ::= (x ilist)) ;; block
  (blist ::= (b ...)) ;; grammar
  (ilist ::= (i ...)))

(provide (all-defined-out))
