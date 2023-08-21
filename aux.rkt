#lang racket

(require "lang.rkt")

(require redex)

(define-metafunction
  LPEG
  add : natural integer -> natural
  [(add natural_1 integer_2) ,(+ (term natural_1) (term integer_2))])

(define-metafunction
  LPEG
  fetch-i : ilist natural -> i
  [(fetch-i ilist natural) ,(list-ref (term ilist) (term natural))])

(provide (all-defined-out))
