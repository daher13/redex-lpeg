#lang racket

(require redex)

(require "../peg.rkt")
(require "../peggen.rkt")

(define-metafunction PEGGEN
  ;; fetch productions
  fetch-prods : g -> (prod ...)
  [(fetch-prods ∅) ()]
  [(fetch-prods (x e g)) ((x e) prod ...)
                         (where (prod ...) (fetch-prods g))])

(define-metafunction PEG
  ;; fetch expression on a production list
  fetch-e : (prod ...) x -> e
  [(fetch-e ((x e) prod ...) x) e]
  [(fetch-e (prod_1 prod ...) x) (fetch-e (prod ...) x)]
  )

(define-metafunction PEG
  ;; fetch the list of all reachable variables
  fetch-reach : (prod ...) e (x ...) -> (x ...)
  [(fetch-reach (prod ...) terminal (x ...)) (x ...)]
  [(fetch-reach (prod ...) ϵ (x ...)) (x ...)]
  [(fetch-reach (prod ...) x (x_1 ... x x_2 ...)) (x_1 ... x x_2 ...)]
  [(fetch-reach (prod ...) x (x_1 ...))
   (x_2 ...)
   (where e_1 (fetch-e (prod ...) x))
   (where (x_2 ...) (fetch-reach (prod ...) e_1 (x_1 ... x)))]
  [(fetch-reach (prod ...) (_ e_1 e_2) (x ...))
   (x_2 ...)
   (where (x_1 ...) (fetch-reach (prod ...) e_1 (x ...)))
   (where (x_2 ...) (fetch-reach (prod ...) e_2 (x_1 ...)))]
  [(fetch-reach (prod ...) (_ e) (x ...))
   (x_1 ...)
   (where (x_1 ...) (fetch-reach (prod ...) e (x ...)))])

(define-metafunction PEG
  ;; mount peg following the order of a list of variables
  mount-peg : (x ...) (prod ...) -> (prod ...)
  [(mount-peg () _) ()]
  [(mount-peg (x x_1 ...) (prod ...))
   ((x e) prod_1 ...)
   (where e (fetch-e (prod ...) x))
   (where (prod_1 ...) (mount-peg (x_1 ...) (prod ...)))])

(define-metafunction PEG
  ;; eliminate unreachable variables
  peg-reduce : (prod ...) -> (prod ...)
  [(peg-reduce (prod ...))
   (prod_2 ...)
   (where ((x_1 e_1) prod_1 ...) (prod ...))
   (where (x ...) (fetch-reach (prod ...) x_1 ()))
   (where (prod_2 ...) (mount-peg (x ...) (prod ...)))])

(define-metafunction PEGGEN
  peggen->peg : seq -> (prod ...)
  [(peggen->peg (g start tlist))
   (prod_2 ...)
   (where (prod_1 ...) (fetch-prods g)) ;; fetch prods
   (where (prod_2 ...) ((s1 start) prod_1 ...)) ;; create initial state
   (where (prod_3 ...) (peg-reduce (prod_2 ...))) ;; eliminate unreachable variables
   ])

(provide peggen->peg)
