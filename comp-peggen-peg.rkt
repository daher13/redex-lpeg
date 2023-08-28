#lang racket

(require redex)

(require "peg.rkt")
(require "peggen.rkt")

(define-metafunction PEGGEN
  ;; fetch productions
  fetch-prods : g -> (prod ...)
  [(fetch-prods ∅) ()]
  [(fetch-prods (x e g)) ((x e) prod ...)
                         (where (prod ...) (fetch-prods g))])

(define-metafunction PEG
  fetch-e : (prod ...) x -> e
  [(fetch-e ((x e) prod ...) x) e]
  [(fetch-e (prod_1 prod ...) x) (fetch-e (prod ...) x)]
  )

(define-metafunction PEG
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

(define-metafunction PEGGEN
  peggen->peg : seq -> (prod ...)
  [(peggen->peg (g start tlist))
   (prod_3 ...)
   (where (prod_1 ...) (fetch-prods g)) ;; fetch prods
   (where (prod_2 ...) ((s1 start) prod_1 ...))
   (where (x ...) (fetch-call (prod_2 ...) s1 ()))
   (where (prod_3 ...) (mount-peg (x ...) (prod_2 ...)))])

(define generated (term ((K T (T K (S T ∅))) T ())))

(define peg (term (peggen->peg ,generated)))

(provide peggen->peg)
