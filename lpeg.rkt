#lang racket

(require redex)

(define-language LPEG
  (ch ::= natural)
  (l ::= integer)
  (i ::=
     (char ch)
     (jump l)
     (choice l)
     (call l)
     return
     emp
     (commit l)
     capture
     fail
     end)
  (ilist ::= (i ...)))

(provide LPEG)
