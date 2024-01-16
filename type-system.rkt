#lang racket

(require redex)
(require "lpeg.rkt")

(define-extended-language TypeSystem LPEG
  (pc ::= natural)
  (blk ::= bt bf) ;; blocked true and false
  (b ::= boolean blk)
  (cml ::= l) ;; label for commits
  (cmb ::= b) ;; boolean for commits
  (cme ::= (cml cmb)) ;; commit entry
  (pastc ::= (cme ...)) ;; commit stack
  (cll ::= l) ;; label for calcl
  (clb ::= b) ;; boolean for calcl
  (cle ::= (cll clb)) ;; call entry
  (pastl ::= (cle ...))
  (e ::= (l b) ()) ;; global entry
  (el ::= (e ...)) ;; global entry list
  )

(define-metafunction TypeSystem
  fetch-i : ilist pc -> i
  [(fetch-i ilist pc) ,(list-ref (term ilist) (term pc))])

(define-metafunction TypeSystem
  sum : integer integer -> integer
  [(sum integer_1 integer_2) ,(+ (term integer_1) (term integer_2))])

(define-metafunction TypeSystem
  b->boolean : b -> boolean
  [(b->boolean bf) #f]
  [(b->boolean bt) #t]
  [(b->boolean #t) #t]
  [(b->boolean #f) #f])

(define-metafunction TypeSystem
  is-negative-commit : i -> b
  [(is-negative-commit (commit l)) #t
                                   (side-condition (< (term l) 0))]
  [(is-negative-commit i) #f])

(define-metafunction TypeSystem
  is-fail : i -> b
  [(is-fail fail) #t]
  [(is-fail _) #f])

(define-metafunction TypeSystem
  update-head : el b -> el
  [(update-head (e ... (l blk)) _) (e ... (l blk))]
  [(update-head (e ... (l b)) b_1) (e ... (l b_1))]
  [(update-head () _) ()])

(define-metafunction TypeSystem
  fetch-head : el -> b
  [(fetch-head (e ... (_ b))) (b->boolean b)]
  [(fetch-head ()) #t])

(define-metafunction TypeSystem
  find-e : el l -> e
  [(find-e (e_1 ... (l b) e_2 ...) l) (l b)]
  [(find-e _ _) ()])

(define-metafunction TypeSystem
  update-pastl : pastl -> pastl
  [(update-pastl (cle ... (cll blk))) (cle ... (cll blk))]
  [(update-pastl (cle ... (cll_1 boolean))) (cle_1 ... (cll_1 #t))
                                            (where (cle_1 ...) (update-pastl (cle ...)))]
  [(update-pastl ()) ()])

(define-metafunction TypeSystem
  fetch-pastl-interval : pastl l -> pastl
  [(fetch-pastl-interval (cle ... (l b) cle_1 ...) l) ((l b) cle_1 ...)]
  [(fetch-pastl-interval pastl _) ()])

(define-metafunction TypeSystem
  unify-b : pastl -> b
  [(unify-b (cle ... (l b))) ,(or (term b_1) (term b_2))
                             (where b_1 (b->boolean b))
                             (where b_2 (unify-b (cle ...)))]
  [(unify-b ()) #f])

(define-judgment-form TypeSystem
  #:mode (ts I I I I O I O)
  #:contract (ts ilist pc i pastc pastc pastl pastl)

  [
   (where pc_next (sum pc 1))
   (where i_1 (fetch-i ilist pc_next))

   (where pastc_1 (update-head pastc #t))
   (where pastl_1 (update-pastl pastl))

   (ts ilist pc_next i_1 pastc_1 pastc_2 pastl_1 pastl_2)
   ---------------------------------------------------------------------- "T-char"
   (ts ilist pc (char ch) pastc pastc_2 pastl pastl_2)
   ]

[
   (where pc_1 (sum pc l)) ;; labelled instruction
   (where i_1 (fetch-i ilist pc_1))

   (where pc_2 (sum pc 1)) ;; next instruction
   (where i_2 (fetch-i ilist pc_2))

   (where () (find-e pastl pc_1))

   (where (cle ...) pastl)
   (where clb (fetch-head pastl))

   (ts ilist pc_1 i_1 pastc pastc_1 (cle ... (pc_1 #f)) (_ ... (pc_1 b_1) cle_1 ...)) ;; goto label

   (where clb_2 (unify-b ((pc_1 b_1) cle_1 ...)))
   (where clb_3 ,(or (term clb_2) (term clb)))
   (where pastl_2 (update-head pastl clb_3))

   (ts ilist pc_2 i_2 pastc_1 pastc_2 pastl_2 pastl_3) ;; goto next
   ------------------------------------------------------------------------------------- "T-call"
   (ts ilist pc (call l) pastc pastc_2 pastl pastl_3)
   ]

  [
   (where pc_1 (sum pc l)) ;; labelled instruction
   (where i_1 (fetch-i ilist pc_1))

   (where pc_2 (sum pc 1)) ;; next instruction
   (where i_2 (fetch-i ilist pc_2))

   (where (_ _) (find-e pastl pc_1))
   (where cmb (fetch-head pastc))
   (where clb (fetch-head pastl))

   (where pastl_0 (fetch-pastl-interval pastl pc_1))
   (where clb_3 (unify-b pastl_0))

   (side-condition ,(or (term clb_3) (term clb)))

   (where pastc_1 (update-head pastc ,(or (term cmb) (term clb_3))))
   (where pastl_1 (update-head pastl #t))

   (ts ilist pc_2 i_2 pastc_1 pastc_2 pastl_1 pastl_2) ;; goto next
   ------------------------------------------------------------------------------------ "T-call-passed"
   (ts ilist pc (call l) pastc pastc_2 pastl pastl_2)
   ]

  [
   (where pc_1 (sum pc l)) ;; labelled instruction
   (where i_1 (fetch-i ilist pc_1))

   (where pc_0 (sum pc_1 -1)) ;; previous from labelled instruction
   (where i_0 (fetch-i ilist pc_0))

   (where pc_2 (sum pc 1)) ;; next instruction
   (where i_2 (fetch-i ilist pc_2))

   (where clb (fetch-head pastl))
   (where cmb (fetch-head pastc))

   (side-condition ,(not (term (is-negative-commit i_0))))

   (ts ilist pc_1 i_1 pastc pastc_1 pastl pastl_1) ;; goto labelled
   (ts ilist pc_2 i_2 pastc pastc_2 pastl pastl_2) ;; goto next

   (where clb_1 (fetch-head pastl_1))
   (where clb_2 (fetch-head pastl_2))
   (where cmb_1 (fetch-head pastc_1))
   (where cmb_2 (fetch-head pastc_2))

   (where clb_3 ,(and (term clb_1) (term clb_2)))
   (where clb_4 ,(or (term clb) (term clb_3)))
   (where pastl_3 (update-head pastl_2 clb_4))

   (where cmb_3 ,(and (term cmb_1) (term cmb_2)))
   (where cmb_4 ,(or (term cmb) (term cmb_3)))
   (where pastc_3 (update-head pastc_2 cmb_3))
   ----------------------------------------------------------------------------------- "T-choice"
   (ts ilist pc (choice l) pastc pastc_3 pastl pastl_3)
   ]

  [
   (where pc_1 (sum pc l)) ;; labelled instruction
   (where i_1 (fetch-i ilist pc_1))

   (where pc_0 (sum pc_1 -1)) ;; previous from labelled instruction
   (where i_0 (fetch-i ilist pc_0))

   (where pc_2 (sum pc 1)) ;; next instruction
   (where i_2 (fetch-i ilist pc_2))

   (where clb (fetch-head pastl))

   (side-condition (is-negative-commit i_0))

   (ts ilist pc_1 i_1 (cme ...) pastc_1 pastl pastl_1) ;; goto labelled ;; loop is here
   (ts ilist pc_2 i_2 (cme ... (pc_0 #f)) pastc_2 pastl _) ;; goto next instruction

   (where clb_1 (fetch-head pastl_1))

   (where clb_3 ,(or (term clb) (term clb_1)))
   (where pastl_3 (update-head pastl_1 clb_3))
   ------------------------------------------------------------------------------------ "T-choice-prev-negative"
   (ts ilist pc (choice l) (cme ...) pastc_2 pastl pastl_3)
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
   (side-condition ,(< (term l) 0))

   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 (cme ...) pastc_1 pastl pastl_1)
   -------------------------------------------------------------------- "T-commit-negative"
   (ts ilist pc (commit l) (cme ... (pc #t)) pastc_1 pastl pastl_1)
   ]

  [
   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 pastc pastc_1 pastl pastl_1)
   ---------------------------------------------------------------------- "T-fail"
   (ts ilist pc fail pastc pastc_1 pastl pastl_1)
   ]

  [
   ---------------------------------------------------------------------- "T-jump"
   (ts ilist pc (jump l) pastc pastc pastl pastl)
   ]

  [
   ---------------------------------------------------------------------- "T-return"
   (ts ilist pc return pastc pastc pastl pastl)
   ]

  [
   ---------------------------------------------------------------------- "T-end"
   (ts ilist pc end pastc pastc pastl pastl)
   ]
)


(provide (all-defined-out))
