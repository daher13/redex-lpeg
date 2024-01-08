#lang racket

(require redex)
(require "lpeg.rkt")

(module type-system racket/base)

(define-extended-language TypeSystem LPEG
  (pc ::= natural)
  (b ::= boolean)
  (cml ::= l) ;; label for commits
  (cmb ::= b) ;; boolean for commits
  (cme ::= (cml cmb)) ;; commit entry
  (pastc ::= (cme ...)) ;; commit stack
  (cll ::= l) ;; label for calcl
  (clb ::= b) ;; boolean for calcl
  (cle ::= (cll clb)) ;; call entry
  (pastl ::= (cle ...))
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
  is-fail : i -> b
  [(is-fail fail) #t]
  [(is-fail _) #f])

(define-metafunction TypeSystem
  update-head : ((l b) ...) boolean -> ((l b) ...)
  [(update-head ((l_1 b_1) ... (l b)) boolean) ((l_1 b_1) ... (l boolean))]
  [(update-head () _) ()])

(define-metafunction TypeSystem
  fetch-tail : ((l b) ...) -> b
  [(fetch-tail ((l_1 b_1) ... (l b))) b]
  [(fetch-tail ()) #t])

(define-metafunction TypeSystem
  find-e : ((l b) ...) l -> (l b) or ()
  [(find-e ((l_1 b_1) ... (l b) (l_2 b_2) ...) l) (l b)]
  [(find-e _ _) ()])

(define-metafunction TypeSystem
  update-pastl : pastl -> pastl
  [(update-pastl ((cll_1 clb_1) cle ...)) ((cll_1 #t) cle_1 ...)
                                          (where (cle_1 ...) (update-pastl (cle ...)))]
  [(update-pastl ()) ()])

(define-metafunction TypeSystem
  fetch-pastl-interval : pastl l -> pastl
  [(fetch-pastl-interval (cle ... (l b) cle_1 ...) l) ((l b) cle_1 ...)]
  [(fetch-pastl-interval pastl l) ()])

(define-metafunction TypeSystem
  unify-b : pastl -> b
  [(unify-b ((l b) cle ...)) ,(or (term b) (term b_1))
                             (where b_1 (unify-b (cle ...)))]
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

   (where () (find-e (cle ...) pc_1))

   (ts ilist pc_1 i_1 pastc pastc_1 (cle ... (pc_1 #f)) (cle_1 ... (cll_1 clb_1))) ;; goto label


   ;; (where clb_2 (fetch-tail (cle ...)))
   ;; (where clb_3 ,(or (term clb_1) (term clb_2)))
   ;; (where pastl_1 (update-head (cle ...) clb_3))

   (where clb_2 (fetch-tail (cle ...)))
   (where pastl_0 (fetch-pastl-interval (cle_1 ... (cll_1 clb_1)) pc_1))
   (where clb_3 (unify-b pastl_0))
   (where clb_4 ,(or (term clb_3) (term clb_2)))
   (where pastl_1 (update-head (cle ...) clb_4))

   (ts ilist pc_2 i_2 pastc_1 pastc_2 pastl_1 pastl_2) ;; goto next
   ---------------------------------------------------------------------------- "T-call"
   (ts ilist pc (call l) pastc pastc_2 (cle ...) pastl_2)
   ]

  [
   (where pc_1 (sum pc l)) ;; labelled instruction
   (where i_1 (fetch-i ilist pc_1))

   (where pc_2 (sum pc 1)) ;; next instruction
   (where i_2 (fetch-i ilist pc_2))

   (where (cll_1 clb_1) (find-e (cle ... (cll clb)) pc_1))

   ;; (side-condition ,(or (term clb) (term clb_1)))
   ;; (where clb_3 ,(or (term clb) (term clb_1)))

   (where clb_2 (fetch-tail (cle ... (cll clb))))
   (where pastl_0 (fetch-pastl-interval (cle ... (cll clb)) pc_1))
   (where clb_3 (unify-b pastl_0))
   (where clb_4 ,(or (term clb_3) (term clb_2)))

   (side-condition clb_4)


   (where pastc_1 (update-head pastc ,(or (term (fetch-tail pastc)) (term clb_3))))

   ;; (ts ilist pc_2 i_2 pastc_1 pastc_2 (cle ... (cll clb_3)) pastl_2)
   (ts ilist pc_2 i_2 pastc_1 pastc_2 (cle ... (cll clb_4)) pastl_2)
   ------------------------------------------------------------------------------------ "T-call-passed"
   (ts ilist pc (call l) pastc pastc_2 (cle ... (cll clb)) pastl_2)
   ]

  [
   (where pc_1 (sum pc l)) ;; labelled instruction
   (where i_1 (fetch-i ilist pc_1))

   (where pc_0 (sum pc_1 -1)) ;; previous from labelled instruction
   (where i_0 (fetch-i ilist pc_0))

   (where pc_2 (sum pc 1)) ;; next instruction
   (where i_2 (fetch-i ilist pc_2))

   (side-condition (is-negative-commit i_0))

   (ts ilist pc_1 i_1 (cme ...) pastc_1 (cle ... (cll clb)) pastl_1) ;; goto labelled

   (ts ilist pc_2 i_2 (cme ... (pc_0 #f)) pastc_2 (cle ... (cll clb)) pastl_2) ;; goto next instruction

   (where pastl_3 (update-head pastl_1 ,(or (term clb) (term (fetch-tail pastl_1)))))
   ------------------------------------------------------------------------------------ "T-choice-prev-negative"
   (ts ilist pc (choice l) (cme ...) pastc_2 (cle ... (cll clb)) pastl_3)
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
   (where pc_1 (sum pc l)) ;; labelled instruction
   (where i_1 (fetch-i ilist pc_1))

   (where pc_0 (sum pc_1 -1)) ;; previous from labelled instruction
   (where i_0 (fetch-i ilist pc_0))

   (where pc_2 (sum pc 1)) ;; next instruction
   (where i_2 (fetch-i ilist pc_2))

   (side-condition ,(not (term (is-negative-commit i_0))))

   (ts ilist pc_1 i_1 pastc pastc_1 pastl pastl_1) ;; goto labelled
   (ts ilist pc_2 i_2 pastc pastc_2 pastl pastl_2) ;; goto next

   (where clb_1 (fetch-tail pastl_1))
   (where clb_2 (fetch-tail pastl_2))
   (where clb_3 ,(and (term clb_1) (term clb_2)))
   (where clb_4 ,(or (term (fetch-tail pastl)) (term clb_3)))
   (where pastl_3 (update-head pastl_2 clb_4))

   (where cmb_1 (fetch-tail pastc_1))
   (where cmb_2 (fetch-tail pastc_2))
   (where cmb_3 ,(and (term cmb_1) (term cmb_2)))
   (where cmb_0 (fetch-tail pastc)) ;; está peganbdo true no tail
   (where cmb_4 ,(or (term cmb_0) (term cmb_3)))
   (where pastc_3 (update-head pastc_2 cmb_3))
   ----------------------------------------------------------------------------------- "T-choice"
   (ts ilist pc (choice l) pastc pastc_3 pastl pastl_3)
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
