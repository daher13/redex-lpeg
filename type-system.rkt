#lang racket

(require redex)
(require "lpeg.rkt")

(module type-system racket/base)

(define-extended-language TypeSystem LPEG
  (pc ::= natural)
  (b ::= boolean)
  (bl ::= boolean)
  (pastl ::= (pc ...)) ;; past calls
  )

(define-metafunction TypeSystem
  fetch-i : ilist pc -> i
  [(fetch-i ilist pc) ,(list-ref (term ilist) (term pc))])

(define-metafunction TypeSystem
  sum : integer integer -> integer
  [(sum integer_1 integer_2) ,(+ (term integer_1) (term integer_2))])

(define-metafunction TypeSystem
  is-call-loop : i pc pc -> b ;; instruction current_pc next_pc
  [(is-call-loop (call l) pc_1 pc_2) #t
                                     (side-condition (equal? (term pc_1) (term (sum pc_2 l))))]
  [(is-call-loop i pc_1 pc_2) #f])

(define-metafunction TypeSystem
  is-negative-commit : i -> b
  [(is-negative-commit (commit l)) #t
                                   (side-condition (< (term l) 0))]
  [(is-negative-commit i) #f])

(define-judgment-form TypeSystem
  #:mode (ts I I I I O I O I O)
  #:contract (ts ilist pc i b b bl bl pastl pastl) ;; b means that a char is mandatory

  [
   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 #f b_1 #f bl_1 pastl pastl_1)
   ---------------------------------------------------------------------- "T-char"
   (ts ilist pc (char ch) b b_1 bl bl_1 pastl pastl_1)
   ]

  [
   (where pc_1 (sum pc l)) ;; labelled instruction
   ;; (where i_1 (fetch-i ilist pc_1))

   (where pc_0 (sum pc_1 -1)) ;; previous from labelled instruction
   (where i_0 (fetch-i ilist pc_0))

   (side-condition (is-negative-commit i_0))

   (where pc_2 (sum pc 1)) ;; next instruction
   (where i_2 (fetch-i ilist pc_2))


   (ts ilist pc_2 i_2 #t b_2 bl bl_1 pastl pastl_1) ;; set stk and goto next
   -------------------------------------------------------------------- "T-choice-prev-negative"
   (ts ilist pc (choice l) #f b_2 bl bl_1 pastl pastl_1)
   ]

   [
   (where pc_1 (sum pc l)) ;; labelled instruction
   (where i_1 (fetch-i ilist pc_1))

   (where pc_0 (sum pc_1 -1)) ;; previous from labelled instruction
   (where i_0 (fetch-i ilist pc_0))
   (side-condition ,(not (term (is-negative-commit i_0))))

   (where pc_2 (sum pc 1)) ;; next instruction
   (where i_2 (fetch-i ilist pc_2))

   (ts ilist pc_1 i_1 b b_1 bl bl_1 pastl pastl_1)
   (ts ilist pc_2 i_2 b  b_2 bl bl_2 pastl pastl_2)

   (where b_3 ,(or (term b_1) (term b_2)))
   (where bl_3 ,(or (term bl_1) (term bl_2)))
   (where pastl_3 ,(append (term pastl_1) (term pastl_2)))
   -------------------------------------------------------------------- "T-choice"
   (ts ilist pc (choice l) b b_3 bl bl_3 pastl pastl_3)
   ]

  [
   (side-condition ,(< (term l) 0))

   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))


   (where pc_2 (sum pc l))
   (where i_2 (fetch-i ilist pc_2))

   (ts ilist pc_1 i_1 b b_1 bl bl_1 pastl pastl_1)
   -------------------------------------------------------------------- "T-commit-negative"
   (ts ilist pc (commit l) b b_1 bl bl_1 pastl pastl_1)
   ]

  [
   (side-condition ,(> (term l) 0))

   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 b b_1 bl bl_1 pastl pastl_1)
   -------------------------------------------------------------------- "T-commit"
   (ts ilist pc (commit l) b b_1 bl bl_1 pastl pastl_1)
   ]

  [
   ---------------------------------------------------------------------- "T-end"
   (ts ilist pc end b b bl bl pastl pastl)
   ]

  [
   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 b b_1 bl bl_1 pastl pastl_1) ;; alterei de bl para false
   ---------------------------------------------------------------------- "T-return"
   (ts ilist pc return b b_1 bl bl_1 pastl pastl_1)
   ]

  [

   ---------------------------------------------------------------------- "T-fail"
   (ts ilist pc fail b b bl bl pastl pastl)
   ]

  [
   (side-condition ,(> (term l) 0))

   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 b b_1 #t bl_1 pastl pastl_1)
   -------------------------------------------------------------------- "T-call"
   (ts ilist pc (call l) b b_1 bl bl_1 pastl pastl_1)
   ]

  [
   (side-condition ,(< (term l) 0))
   (side-condition ,(not (member (term pc) (term (l_0 ...)))))

   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 b b_1 bl bl_1 (l_0 ... pc) pastl_1)
   -------------------------------------------------------------------- "T-call-negative"
   (ts ilist pc (call l) b b_1 bl bl_1 (l_0 ...) pastl_1)
   ]

  [
   (side-condition ,(< (term l) 0))

   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 b b_1 #f bl_1 (l_0 ... pc l_1 ...) pastl_1)
   -------------------------------------------------------------------- "T-call-negative-passed"
   (ts ilist pc (call l) b b_1 #f bl_1 (l_0 ... pc l_1 ...) pastl_1)
   ]
)


(provide (all-defined-out))
