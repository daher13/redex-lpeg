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

(define-metafunction PEGGEN
  peggen->peg : seq -> (prod ...)
  [(peggen->peg (g start tlist))
   (prod_3 ...)
   (where (prod_1 ...) (fetch-prods g)) ;; fetch prods
   (where (prod_2 ...) ((s1 start) prod_1 ...)) ;; create initial state
   (where (prod_3 ...) (elim-unre (prod_2 ...))) ;; eliminate unreachable variables
   ])

(provide peggen->peg)
