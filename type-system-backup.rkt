#lang racket

(require redex)
(require "lpeg.rkt")

(module type-system racket/base)

(define-extended-language TypeSystem LPEG
  (pc ::= natural)
  (b ::= boolean)
  (cl ::= l) ;; label for commits
  (cb ::= b) ;; boolean for commits
  (ce ::= (cl cb)) ;; commit entry
  (pastc ::= (ce ...)) ;; commit stack
  (ll ::= l) ;; label for calcl
  (lb ::= b) ;; boolean for calcl
  (le ::= (ll lb)) ;; call entry
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
  is-fail : i -> b
  [(is-fail fail) #t]
  [(is-fail _) #f])

(define-metafunction TypeSystem
  change-head : ((l b) ...) b -> ((l b) ...)
  [(change-head ((l_1 b_1) ... (l b)) b_0) ((l_1 b_1) ... (l b_0))]
  [(change-head () _) ()])

(define-metafunction TypeSystem
  fetch-cb : pastc -> b
  [(fetch-cb (ce ... (cl cb))) cb]
  [(fetch-cb ()) #t])

(define-metafunction TypeSystem
  merge-pastc : pastc pastc -> pastc
  [(merge-pastc () ()) ()]
  [(merge-pastc (ce ... (cl_1 cb_1)) (ce ... (cl_1 cb_2))) (ce ... (cl_1 cb_3))
                                                      (where cb_3 ,(and (term cb_1) (term cb_2)))
                                                      ])

(define-metafunction TypeSystem
  find-le : pastl ll -> le or ()
  [(find-le (le_1 ... (ll lb) le_2 ...) ll) (ll lb)]
  [(find-le pastl ll) ()])

(define-metafunction TypeSystem
  change-pastl : pastl -> pastl
  [(change-pastl ((ll_1 lb_1) le ...)) ((ll_1 #t) le_1 ...)
                                    (where (le_1 ...) (change-pastl (le ...)))
                                    ]
  [(change-pastl ()) ()])

(define-judgment-form TypeSystem
  #:mode (ts I I I I O I O)
  #:contract (ts ilist pc i pastc pastc pastl pastl)

  [
   (where pc_next (sum pc 1))
   (where i_1 (fetch-i ilist pc_next))

   (where pastc_1 (change-head pastc #t))
   (where pastl_1 (change-pastl pastl))

   (ts ilist pc_next i_1 pastc_1 pastc_2 pastl_1 pastl_2)
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

   (ts ilist pc_1 i_1 (ce ...) pastc_1 (le ... (ll lb)) pastl_1) ;; goto labelled

   (where lb_1 (fetch-cb pastl_1))
   (where lb_2 ,(term lb))

   (ts ilist pc_2 i_2 (ce ... (pc_0 #f)) pastc_2 (le ... (ll lb_2)) pastl_2) ;; goto next instruction
   -------------------------------------------------------------------- "T-choice-prev-negative"
   (ts ilist pc (choice l) (ce ...) pastc_2 (le ... (ll lb)) pastl_2)
   ]

  ;; [
  ;;  (where pc_1 (sum pc l)) ;; labelled instruction
  ;;  (where i_1 (fetch-i ilist pc_1))

  ;;  (where pc_0 (sum pc_1 -1)) ;; previous from labelled instruction
  ;;  (where i_0 (fetch-i ilist pc_0))

  ;;  (side-condition (is-fail i_0))

  ;;  (where pc_2 (sum pc 1)) ;; next instruction
  ;;  (where i_2 (fetch-i ilist pc_2))

  ;;  (where (le ... (ll lb)) pastl)

  ;;  (ts ilist pc_2 i_2 pastc pastc_2 (le ... (ll #t)) pastl_2) ;; goto next instruction
  ;;  -------------------------------------------------------------------- "T-choice-prev-fail"
  ;;  (ts ilist pc (choice l) pastc pastc_2 pastl pastl_2)
  ;;  ]

  [
   (where pc_1 (sum pc l)) ;; labelled instruction
   (where i_1 (fetch-i ilist pc_1))

   (where pc_0 (sum pc_1 -1)) ;; previous from labelled instruction
   (where i_0 (fetch-i ilist pc_0))
   (side-condition ,(not (or (term (is-negative-commit i_0))
                             ;; (term (is-fail i_0))
                             )))

   (where pc_2 (sum pc 1)) ;; next instruction
   (where i_2 (fetch-i ilist pc_2))

   (ts ilist pc_1 i_1 pastc pastc_1 (le ... (ll lb)) (le_1 ... (ll_1 lb_1))) ;; goto labelled
   (ts ilist pc_2 i_2 pastc pastc_2 (le ... (ll lb)) (le_2 ... (ll_2 lb_2))) ;; goto next

   ;; (where pastc_3 (merge-pastc pastc_1 pastc_2))
   (where lb_3 ,(or (term lb) (and (term lb_1) (term lb_2))))
   ;; fazer combinação do pastc igual ao pastl
   (where cb_1 (fetch-cb pastc_1))
   (where cb_2 (fetch-cb pastc_2))
   (where cb_3 ,(and (term cb_1) (term cb_2)))
   (where cb_4 ,(or (term (fetch-cb pastc)) (term cb_3)))

   (where pastc_3 (change-head pastc_2 cb_4))
   ----------------------------------------------------------------------------- "T-choice"
   (ts ilist pc (choice l) pastc pastc_3 (le ... (ll lb)) (le ... (ll lb_3)))
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
   (side-condition ,(< (term l) 0))

   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))

   (side-condition ,(term (is-fail i_1)))
   (side-condition (fetch-cb pastc))

   (ts ilist pc_1 i_1 pastc pastc_1 pastl pastl_1)
   -------------------------------------------------------------------- "T-commit-fail"
   (ts ilist pc (commit l) pastc pastc_1 pastl pastl_1)
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
   ---------------------------------------------------------------------- "T-jump"
   (ts ilist pc (jump l) pastc pastc pastl pastl)
   ]

  [
   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))

   (where () (find-le (le ...) pc_1))

   (ts ilist pc_1 i_1 pastc pastc_1 (le ... (pc_1 #f)) (le_1 ... (ll_1 lb_1))) ;; goto label

   (where pc_2 (sum pc 1))
   (where i_2 (fetch-i ilist pc_2))
   (where lb_2 (fetch-cb (le ...)))
   (where lb_3 ,(or (term lb_2) (term lb_1)))
   (where pastl_1 (change-head (le ...) lb_3))

   (ts ilist pc_2 i_2 pastc_1 pastc_2 pastl_1 pastl_2) ;; goto next (continue processing
   ---------------------------------------------------------------------------- "T-call"
   (ts ilist pc (call l) pastc pastc_2 (le ...) pastl_2)
   ]

  [
   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))

   (where (ll_1 lb_1) (find-le (le ... (ll lb)) pc_1))

   (side-condition ,(or (term lb) (term lb_1)))
   (where lb_3 ,(or (term lb) (term lb_1)))

   (where pc_2 (sum pc 1))
   (where i_2 (fetch-i ilist pc_2))

   (where pastc_1 (change-head pastc ,(or (term (fetch-cb pastc)) (term lb_3))))

   (ts ilist pc_2 i_2 pastc_1 pastc_2 (le ... (ll lb_3)) pastl_2)
   -------------------------------------------------------------------------- "T-call-passed"
   (ts ilist pc (call l) pastc pastc_2 (le ... (ll lb)) pastl_2)
   ]

  )


(provide (all-defined-out))
