#lang racket

(require redex)

(require "peggen.rkt")

(define-metafunction PEGGEN
  ;; fetch productions
  fetch-prods : g -> (prod ...)
  [(fetch-prods ∅) ()]
  [(fetch-prods (x e g)) ((x e) prod ...)
                         (where (prod ...) (fetch-prods g))])

(define-metafunction PEGGEN
  ;; compile peggen to peg
  peggen->peg : seq -> (prod ...)
  [(peggen->peg (g start tlist)) ((s0 start) prod ...)
                                     (where (prod ...) (fetch-prods g))])


(provide peggen->peg)
