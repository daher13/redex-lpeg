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

(define-metafunction
  LPEG
  find-block-index : blist l -> natural
  [(find-block-index ((l_1 ilist_1) b ...) l_1) 0]
  [(find-block-index ((l ilist) b ...) l_1) ,(+ (length (term ilist)) (term (find-block-index (b ...) l_1)))])

(define-metafunction
  LPEG
  fetch-i : ilist natural -> i
  [(fetch-i ilist natural) ,(list-ref (term ilist) (term natural))])

(provide (all-defined-out))
