#lang racket

(require redex)
(require "lpeg.rkt")

(module type-system racket/base)

(define-extended-language TypeSystem LPEG
  (pc ::= natural)
  (b ::= boolean)
  (bc ::= b)
  (bl ::= b)
  (pastl ::= (l ...))
  (lstk ::= (bl pastl))
  (p ::= natural)
  (ce ::= (p bc)) ;; commit entry
  (cstk ::= (ce ...)) ;; commit stack
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
  merge-choice : cstk cstk -> cstk
  [(merge-choice () ()) ()]
  [(merge-choice (ce ... (p bc_1)) (ce ... (p bc_2))) (ce ... (p bc_3))
                                                      (where bc_3 ,(and (term bc_1) (term bc_2)))
                                                      ])

(define-judgment-form TypeSystem
  #:mode (ts I I I I O I O)
  #:contract (ts ilist pc i cstk cstk lstk lstk) ;; b means that a char is mandatory

  [
   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 () cstk_1 (#t pastl) lstk_1)
   ---------------------------------------------------------------------- "T-char"
   (ts ilist pc (char ch) () cstk_1 (bl pastl) lstk_1)
   ]

  [
   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 (ce ... (p #t)) cstk_1 (#t pastl) lstk_1)
   ---------------------------------------------------------------------- "T-char-pop"
   (ts ilist pc (char ch) (ce ... (p bc)) cstk_1 (bl pastl) lstk_1)
   ]

  [
   (where pc_1 (sum pc l)) ;; labelled instruction
   (where i_1 (fetch-i ilist pc_1))

   (where pc_0 (sum pc_1 -1)) ;; previous from labelled instruction
   (where i_0 (fetch-i ilist pc_0))

   (side-condition (is-negative-commit i_0))

   (where pc_2 (sum pc 1)) ;; next instruction
   (where i_2 (fetch-i ilist pc_2))

   (ts ilist pc_2 i_2 (ce ... (pc_0 #f)) cstk_2 lstk lstk_2) ;; stk set and goto next

   (ts ilist pc_1 i_1 (ce ...) cstk_1 lstk lstk_1) ;; just goto label
   -------------------------------------------------------------------- "T-choice-prev-negative"
   (ts ilist pc (choice l) (ce ...) cstk_2 lstk lstk_1)
   ]

   [
   (where pc_1 (sum pc l)) ;; labelled instruction
   (where i_1 (fetch-i ilist pc_1))

   (where pc_0 (sum pc_1 -1)) ;; previous from labelled instruction
   (where i_0 (fetch-i ilist pc_0))
   (side-condition ,(not (term (is-negative-commit i_0))))

   (where pc_2 (sum pc 1)) ;; next instruction
   (where i_2 (fetch-i ilist pc_2))

   (ts ilist pc_1 i_1 cstk cstk_1 lstk (bl_1 (l_1 ...)))
   (ts ilist pc_2 i_2 cstk cstk_2 lstk (bl_2 (l_2 ...)))

   (where cstk_3 (merge-choice cstk_1 cstk_2))
   (where bl_3 ,(and (term bl_1) (term bl_2)))
   (where lstk_3 (bl_3 (l_1 ... l_2 ...)))
   -------------------------------------------------------------------- "T-choice"
   (ts ilist pc (choice l) cstk cstk_3 lstk lstk_3)
   ]

  [
   (side-condition ,(< (term l) 0))

   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 (ce ...) cstk_1 lstk lstk_1)
   -------------------------------------------------------------------- "T-commit-negative"
   (ts ilist pc (commit l) (ce ... (pc #t)) cstk_1 lstk lstk_1)
   ]

  [
   (side-condition ,(> (term l) 0))

   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 cstk cstk_1 lstk lstk_1)
   -------------------------------------------------------------------- "T-commit"
   (ts ilist pc (commit l) cstk cstk_1 lstk lstk_1)
   ]

  [
   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 () cstk_1 lstk lstk_1)
   ---------------------------------------------------------------------- "T-fail"
   (ts ilist pc fail () cstk_1 lstk lstk_1)
   ]

  [
   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 (ce ... (p bc)) cstk_1 lstk lstk_1)
   ---------------------------------------------------------------------- "T-fail-pop"
   (ts ilist pc fail (ce ... (p bc)) cstk_1 lstk lstk_1)
   ]

  [
   ---------------------------------------------------------------------- "T-return"
   (ts ilist pc return cstk cstk lstk lstk)
   ]

  [
   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))

   (side-condition ,(not (member (term pc_1) (term (l_0 ...)))))

   (ts ilist pc_1 i_1 cstk cstk_1 (#f (l_0 ... pc_1)) (bl_1 (l_1 ...)))

   (where pc_2 (sum pc 1))
   (where i_2 (fetch-i ilist pc_2))

   (ts ilist pc_2 i_2 cstk_1 cstk_2 (bl_1 (l_0 ...)) (bc_2 (l_2 ...)))

   (where pastl_3 (l_1 ... l_2 ...))
   (where lstk_3 (bl pastl_3))
   ---------------------------------------------------------------------- "T-call"
   (ts ilist pc (call l) cstk cstk_2 (bl (l_0 ...)) lstk_3)
   ]

  [
   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))

   (side-condition ,(member (term pc_1) (term pastl)))

   (where pc_2 (sum pc 1))
   (where i_2 (fetch-i ilist pc_2))

   (ts ilist pc_2 i_2 cstk cstk_2 (#t pastl) lstk_2)
   ---------------------------------------------------------------------- "T-call-passed"
   (ts ilist pc (call l) cstk cstk_2 (#t pastl) lstk_2)
   ]
)


(provide (all-defined-out))
