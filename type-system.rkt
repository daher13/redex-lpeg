#lang racket

(require redex)

(require "lang.rkt")
(require "aux.rkt")

(define-extended-language
  TypeSystem
  LPEG
  (pc ::= natural) ;; actual position
  (pastl ::= (pc ...)) ;; past labels (for calls)
  (pastc ::= (pc ...)) ;; past commits
  (t ::= (pastl pastc boolean))
  (xlist ::= (x ...))
  (ptype ::= (x boolean xlist)) ;; peg type
  )

(define-metafunction
  TypeSystem
  includes : (pc ...) pc -> boolean
  [(includes (pc_1 ... pc pc_2 ...) pc) #t]
  [(includes _ _) #f])

(define-judgment-form
  TypeSystem
  #:mode (ts I I I I I O)
  #:contract (ts ilist pc i pastl pastc t)

  [
   (where pc_1 (add pc 1))
   (ts ilist pc_1 (fetch-i ilist pc_1) pastl pastc t)
   --------------------------------------------------- "T-emp"
   (ts ilist pc emp pastl pastc t)
   ]

  [ ;; fix when char matches and goto next
   ------------------------------------------------------ "T-char"
   (ts ilist pc (char ch) pastl pastc (pastl pastc #f))
   ]

  [
   --------------------------------------------- "T-end"
   (ts ilist pc end pastl pastc (pastl pastc #t))
   ]

   [
   --------------------------------------------- "T-fail"
   (ts ilist pc fail pastl pastc (pastl pastc #t))
   ]

  [
   ------------------------------------------- "T-return"
   (ts ilist pc return pastl pastc (pastl pastc #t))
   ]

  [
   (where pc_1 (add pc l))
   (ts ilist pc_1 (fetch-i ilist pc_1) pastl pastc t)
   ----------------------------------------------------- "T-jump"
   (ts ilist pc (jump l) pastl pastc t)
   ]

  [
   (where pc_1 (add pc l)) ;; first option - goto label
   (where pc_2 (add pc 1)) ;; second option - goto next
   (where i_1 (fetch-i ilist pc_1))
   (where i_2 (fetch-i ilist pc_2))
   (ts ilist pc_1 i_1 pastl pastc ((pc_5 ...) (pc_7 ...) boolean_1)) ;; goto label
   (ts ilist pc_2 i_2 pastl pastc ((pc_6 ...) (pc_8 ...) boolean_2)) ;; goto next
   (where boolean_3 ,(or (term boolean_1) (term boolean_2)))
   ---------------------------------------------------------------------------------- "T-choice"
   ;; (ts ilist pc (choice l) pastl pastc ((pc_5 ... pc_6 ...) (pc_7 ... pc_8 ...) boolean_3))
   (ts ilist pc (choice l) pastl pastc ((pc_5 ...) (pc_7 ...) boolean_3))
   ]

  [
   (where pc_1 (add pc l)) ;; first option - goto label
   (where pc_2 (add pc 1)) ;; second option - goto next
   (side-condition ,(not (term (includes (pc_5 ...) pc_1)))) ;; should i do for pc_2 too?
   (where i_1 (fetch-i ilist pc_1))
   (where i_2 (fetch-i ilist pc_2))
   (ts ilist pc_1 i_1 (pc_1 pc_5 ...) pastc (pastl_1 pastc_1 boolean_1)) ;; goto label
   (ts ilist pc_2 i_2 (pc_2 pc_5 ...) pastc (pastl_2 pastc_2 boolean_2)) ;; goto next
   (where boolean_3 ,(and (term boolean_1) (term boolean_2)))
   (where pastl_3 (pc_1 pc_5 ...)) ;; should i add pc_2? (added)
   ----------------------------------------------------------------------------------- "T-call"
   (ts ilist pc (call l) (pc_5 ...) pastc (pastl_1 pastc boolean_3))
   ]

  [
   (where pc_1 (add pc l)) ;; first option - goto label
   (side-condition ,(not (term (includes (pc_5 ...) pc_1))))
   (where i_1 (fetch-i ilist pc_1))
   (ts ilist pc_1 i_1 pastl (pc_1 pc_5 ...) (pastl_1 pastc_1 boolean_1)) ;; goto label
   (where pastc_2 (pc_1 pc_5 ...))
   --------------------------------------------------------------------------------------- "T-commit"
   (ts ilist pc (commit l) pastl (pc_5 ...) (pastl pastc_2 boolean_1))
   ])

(provide (all-defined-out))
