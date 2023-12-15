#lang racket

(require redex)
(require "lpeg.rkt")

(module type-system racket/base)

(define-extended-language TypeSystem LPEG
  (pc ::= natural)
  (b ::= boolean)
  (lc ::= l) ;; label for commits
  (bc ::= b) ;; boolean for commits
  (ce ::= (lc bc)) ;; commit entry
  (pastc ::= (ce ...)) ;; commit stack
  (ll ::= l) ;; label for calls
  (bl ::= b) ;; boolean for calls
  (le ::= (ll bl)) ;; call entry
  (pastl ::= (le ...))
  )

(define-metafunction TypeSystem
  fetch-i : ilist pc -> i
  [(fetch-i ilist pc) ,(list-ref (term ilist) (term pc))])

(define-metafunction TypeSystem
  sum : integer integer -> integer
  [(sum integer_1 integer_2) ,(+ (term integer_1) (term integer_2))])

(define-metafunction TypeSystem
  is-negative-commit : i -> b
  [(is-negative-commit (commit l)) #t
                                   (side-condition (< (term l) 0))]
  [(is-negative-commit i) #f])

(define-metafunction TypeSystem
  change-head : ((l b) ...) -> ((l b) ...)
  [(change-head ((l_1 b_1) ... (l b))) ((l_1 b_1) ... (l #t))]
  [(change-head ()) ()])

(define-metafunction TypeSystem
  merge-choice : pastc pastc -> pastc
  [(merge-choice () ()) ()]
  [(merge-choice (ce ... (p bc_1)) (ce ... (p bc_2))) (ce ... (p bc_3))
                                                      (where bc_3 ,(and (term bc_1) (term bc_2)))
                                                      ])

(define-judgment-form TypeSystem
  #:mode (ts I I I I O)
  #:contract (ts ilist pc i pastc pastc) ;; b means that a char is mandatory

  [
   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (where pastc_1 (change-head pastc))

   (ts ilist pc_1 i_1 pastc_1 pastc_2)
   ---------------------------------------------------------------------- "T-char"
   (ts ilist pc (char ch) pastc pastc_2)
   ]

  [
   (where pc_1 (sum pc l)) ;; labelled instruction
   (where i_1 (fetch-i ilist pc_1))

   (where pc_0 (sum pc_1 -1)) ;; previous from labelled instruction
   (where i_0 (fetch-i ilist pc_0))

   (side-condition (is-negative-commit i_0))

   (where pc_2 (sum pc 1)) ;; next instruction
   (where i_2 (fetch-i ilist pc_2))

   (ts ilist pc_2 i_2 (ce ... (pc_0 #f)) pastc_2)

   (ts ilist pc_1 i_1 (ce ...) pastc_1)
   -------------------------------------------------------------------- "T-choice-prev-negative"
   (ts ilist pc (choice l) (ce ...) pastc_2)
   ]

   [
   (where pc_1 (sum pc l)) ;; labelled instruction
   (where i_1 (fetch-i ilist pc_1))

   (where pc_0 (sum pc_1 -1)) ;; previous from labelled instruction
   (where i_0 (fetch-i ilist pc_0))
   (side-condition ,(not (term (is-negative-commit i_0))))

   (where pc_2 (sum pc 1)) ;; next instruction
   (where i_2 (fetch-i ilist pc_2))

   (ts ilist pc_1 i_1 pastc pastc_1)
   (ts ilist pc_2 i_2 pastc pastc_2)

   (where pastc_3 (merge-choice pastc_1 pastc_2))
   -------------------------------------------------------------------- "T-choice"
   (ts ilist pc (choice l) pastc pastc_3)
   ]

  [
   (side-condition ,(< (term l) 0))

   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 (ce ...) pastc_1)
   -------------------------------------------------------------------- "T-commit-negative"
   (ts ilist pc (commit l) (ce ... (pc #t)) pastc_1)
   ]

  [
   (side-condition ,(> (term l) 0))

   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 pastc pastc_1)
   -------------------------------------------------------------------- "T-commit"
   (ts ilist pc (commit l) pastc pastc_1)
   ]

  [
   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 () pastc_1)
   ---------------------------------------------------------------------- "T-fail"
   (ts ilist pc fail () pastc_1)
   ]

  [
   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 (ce ... (lc bc)) pastc_1)
   ---------------------------------------------------------------------- "T-fail-pop"
   (ts ilist pc fail (ce ... (lc bc)) pastc_1)
   ]

  [
   ---------------------------------------------------------------------- "T-return"
   (ts ilist pc return pastc pastc)
   ]
)


(provide (all-defined-out))
