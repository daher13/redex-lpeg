#lang racket

(require redex)
(require "lpeg.rkt")

(module type-system racket/base)

(define-extended-language TypeSystem LPEG
  (pc ::= natural)
  (b ::= boolean)
  (eq ::= (pc boolean) (pc (+ pc pc)) (pc pc))
  (eqlist ::= (eq ...))
  (pastl ::= (pc ...))
  )

(define-metafunction TypeSystem
  fetch-i : ilist pc -> i
  [(fetch-i ilist pc) ,(list-ref (term ilist) (term pc))])

(define-metafunction TypeSystem
  sum : integer integer -> integer
  [(sum integer_1 integer_2) ,(+ (term integer_1) (term integer_2))])


(define-metafunction TypeSystem
  fetch-eq : eqlist pc -> eq
  [(fetch-eq eqlist pc) ,(list-ref (term eqlist) (term pc))])

(define-judgment-form TypeSystem
  #:mode (ts I I I O O)
  #:contract (ts eqlist eq pastl pastl b)

  [
   ---------------------------------------------------------------------- "T-false"
   (ts eqlist (pc #f) pastl pastl #f)
   ]

  [
   ---------------------------------------------------------------------- "T-true"
   (ts eqlist (pc #t) pastl pastl #t)
   ]

  [
   (side-condition ,(not (member (term pc_2) (term (pc ...)))))
   (where eq (fetch-eq eqlist pc_2))
   (ts eqlist eq (pc ... pc_1) pastl_1 b)
   ---------------------------------------------------------------------- "T-pc"
   (ts eqlist (pc_1 pc_2) (pc ...) pastl_1 b)
   ]

  [
   (side-condition ,(not (member (term pc_1) (term pastl))))
   (side-condition ,(not (member (term pc_2) (term pastl))))

   ;; first option
   (where eq_1 (fetch-eq eqlist pc_1))
   (ts eqlist eq_1 pastl (pc_3 ...) b_3)

   ;; second option
   (where eq_2 (fetch-eq eqlist pc_2))
   (ts eqlist eq_2 pastl (pc_4 ...) b_4)

   (where b_5 ,(or (term b_3) (term b_4)))
   (where pastl_5 (pc_3 ... pc_4 ...))
   ---------------------------------------------------------------------- "T-or"
   (ts eqlist (pc (+ pc_1 pc_2)) pastl pastl_5 b_5)
   ]
  )

(define-judgment-form TypeSystem
  #:mode (eqr I I O)
  #:contract (eqr eqlist eq b)

  [
   (ts eqlist eq () pastl b)

   (where (pc _) eq)
   (where pc_1 (sum pc 1))
   (where eq_1 (fetch-eq eqlist pc_1))

   (ts eqlist eq_1 () pastl_1 b_1)
   ---------------------------------------------------------------------- "T-eq"
   (eqr eqlist eq b)
   ])

(provide (all-defined-out))
