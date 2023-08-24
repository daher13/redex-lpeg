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
     (openjump x)
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

(define-extended-language
  TypeSystem LPEG
  (pc ::= natural) ;; actual position
  (pastl ::= (pc ...)) ;; past labels (for calls)
  (pastc ::= (pc ...)) ;; past commits
  (t ::= (pastl pastc boolean)) ;; past_labels past_commits can_empty fail
  (xlist ::= (x ...))
  (ptype ::= (x boolean xlist)) ;; peg type
  ;; (stk ::= (pc ...))
  )

(provide (all-defined-out))
