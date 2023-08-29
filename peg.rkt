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

(define-metafunction PEG
  fetch-e : (prod ...) x -> e
  [(fetch-e ((x e) prod ...) x) e]
  [(fetch-e (prod_1 prod ...) x) (fetch-e (prod ...) x)]
  )

(define-metafunction PEG
  ;; fetch the list of all accessible variables
  fetch-call : (prod ...) e (x ...) -> (x ...)
  [(fetch-call (prod ...) terminal (x ...)) (x ...)]
  [(fetch-call (prod ...) ϵ (x ...)) (x ...)]
  [(fetch-call (prod ...) x (x_1 ... x x_2 ...)) (x_1 ... x x_2 ...)]
  [(fetch-call (prod ...) x (x_1 ...))
   (x_2 ...)
   (where e_1 (fetch-e (prod ...) x))
   (where (x_2 ...) (fetch-call (prod ...) e_1 (x_1 ... x)))]
  [(fetch-call (prod ...) (_ e_1 e_2) (x ...))
   (x_2 ...)
   (where (x_1 ...) (fetch-call (prod ...) e_1 (x ...)))
   (where (x_2 ...) (fetch-call (prod ...) e_2 (x_1 ...)))]
  [(fetch-call (prod ...) (_ e) (x ...))
   (x_1 ...)
   (where (x_1 ...) (fetch-call (prod ...) e (x ...)))])

(define-metafunction PEG
  mount-peg : (x ...) (prod ...) -> (prod ...)
  [(mount-peg () _) ()]
  [(mount-peg (x x_1 ...) (prod ...))
   ((x e) prod_1 ...)
   (where e (fetch-e (prod ...) x))
   (where (prod_1 ...) (mount-peg (x_1 ...) (prod ...)))])

(define-metafunction PEG
  elim-unre : (prod ...) -> (prod ...)
  [(elim-unre (prod ...))
   (prod_2 ...)
   (where ((x_1 e_1) prod_1 ...) (prod ...))
   (where (x ...) (fetch-call (prod ...) x_1 ()))
   (where (prod_2 ...) (mount-peg (x ...) (prod ...)))])

(provide (all-defined-out))
