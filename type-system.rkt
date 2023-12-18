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
  merge-pastc : pastc pastc -> pastc
  [(merge-pastc () ()) ()]
  [(merge-pastc (ce ... (p bc_1)) (ce ... (lc bc_2))) (ce ... (lc bc_3))
                                                      (where bc_3 ,(and (term bc_1) (term bc_2)))
                                                      ])

(define-metafunction TypeSystem
  find-le : pastl ll -> le or ()
  [(find-le ((le_1 ... (ll bl) le_2 ...)) ll) (ll bl)]
  [(find-le pastl ll) ()])

(define-metafunction TypeSystem
  merge-pastl : pastl pastl pastl -> pastl ;; pastl_current pastl_1 pastl_2
  [(merge-pastl () () ()) ()]
  [
   (merge-pastl
    (le_0 ... (ll_0 bl_0))
    (le_1 ... (ll_0 bl_1) le_2 ...)
    (le_3 ... (ll_0 bl_2) le_4 ...))
   (le_0 ... (ll_0 bl))
   (where bl ,(or (term bl_0) (and (term bl_1) (term bl_2))))])

(define-judgment-form TypeSystem
  #:mode (ts I I I I O I O)
  #:contract (ts ilist pc i pastc pastc pastl pastl)

  [
   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (where pastc_1 (change-head pastc))
   (where pastl_1 (change-head pastl))

   (ts ilist pc_1 i_1 pastc_1 pastc_2 pastl_1 pastl_2)
   ---------------------------------------------------------------------- "T-char"
   (ts ilist pc (char ch) pastc pastc_2 pastl pastl_2)
   ]

  [
   (where pc_1 (sum pc l)) ;; labelled instruction
   (where i_1 (fetch-i ilist pc_1))

   (where pc_0 (sum pc_1 -1)) ;; previous from labelled instruction
   (where i_0 (fetch-i ilist pc_0))

   (side-condition (is-negative-commit i_0))

   (where pc_2 (sum pc 1)) ;; next instruction
   (where i_2 (fetch-i ilist pc_2))

   (ts ilist pc_2 i_2 (ce ... (pc_0 #f)) pastc_2 pastl pastl_2) ;; goto next instruction

   (ts ilist pc_1 i_1 (ce ...) pastc_1 pastl pastl_2) ;; goto labelled instruction
   -------------------------------------------------------------------- "T-choice-prev-negative"
   (ts ilist pc (choice l) (ce ...) pastc_2 pastl pastl)
   ]

  [
   (where pc_1 (sum pc l)) ;; labelled instruction
   (where i_1 (fetch-i ilist pc_1))

   (where pc_0 (sum pc_1 -1)) ;; previous from labelled instruction
   (where i_0 (fetch-i ilist pc_0))
   (side-condition ,(not (term (is-negative-commit i_0))))

   (where pc_2 (sum pc 1)) ;; next instruction
   (where i_2 (fetch-i ilist pc_2))

   (ts ilist pc_1 i_1 pastc pastc_1 pastl pastl_1) ;; goto labelled
   (ts ilist pc_2 i_2 pastc pastc_2 pastl pastl_2) ;; goto next

   (where pastc_3 (merge-pastc pastc_1 pastc_2))
   (where pastl_3 (merge-pastl pastl pastl_1 pastl_2))
   -------------------------------------------------------------------- "T-choice"
   (ts ilist pc (choice l) pastc pastc_3 pastl pastl_3)
   ]

  [
   (side-condition ,(< (term l) 0))

   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 (ce ...) pastc_1 pastl pastl_1)
   -------------------------------------------------------------------- "T-commit-negative"
   (ts ilist pc (commit l) (ce ... (pc #t)) pastc_1 pastl pastl_1)
   ]

  [
   (side-condition ,(> (term l) 0))

   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 pastc pastc_1 pastl pastl_1)
   -------------------------------------------------------------------- "T-commit"
   (ts ilist pc (commit l) pastc pastc_1 pastl pastl_1)
   ]

  [
   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 pastc pastc_1 pastl pastl_1)
   ---------------------------------------------------------------------- "T-fail"
   (ts ilist pc fail pastc pastc_1 pastl pastl_1)
   ]

  [
   ---------------------------------------------------------------------- "T-return"
   (ts ilist pc return pastc pastc pastl pastl)
   ]

  [
   ---------------------------------------------------------------------- "T-end"
   (ts ilist pc end pastc pastc pastl pastl)
   ]

   [
    (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 pastc pastc_1 pastl pastl_1)
   ---------------------------------------------------------------------- "T-jump"
   (ts ilist pc (jump l) pastc pastc_1 pastl pastl_1)
   ]

  [
   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))

   (where () (find-le (le ...) pc_1))

   (ts ilist pc_1 i_1 pastc pastc_1 (le ... (pc_1 #f)) pastl_1) ;; goto label

   (where pc_2 (sum pc 1))
   (where i_2 (fetch-i ilist pc_2))

   (ts ilist pc_2 i_2 pastc_1 pastc_2 pastl_1 pastl_2) ;; goto next (backtrack)
   ---------------------------------------------------------------------- "T-call"
   (ts ilist pc (call l) pastc pastc (le ...) (le ...))
   ]

  [
   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))

   (where (ll_1 bl_1) (find-le (le ...) pc_1))
   (side-condition ,(or (term bl) (term bl_1)))
   ---------------------------------------------------------------------- "T-call-passed"
   (ts ilist pc (call l) pastc pastc (le ... (l_0 bl)) (le ...))
   ]

  )


(provide (all-defined-out))
