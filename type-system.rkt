#lang racket

(require redex)

(require "./lang.rkt")

(define-extended-language
  LPEGTypes
  LPEG
  (pc ::= natural)
  (nstk ::= (pc ...))
  (pl ::= (pc ...)) ;; past labels
  (t ::= boolean))

(define-metafunction
  LPEGTypes
  add : natural integer -> natural
  [(add natural_1 integer_2) ,(+ (term natural_1) (term integer_2))])

(define-judgment-form
  LPEGTypes
  #:mode (acc-empty I I I I O)
  #:contract (acc-empty ilist pc i nstk t)
  [
   ------------------------------------------------ "T-char"
   (acc-empty ilist pc (char ch) nstk #f)
   ]

  [
  (acc-empty ilist l (fetch-i ilist (add pc l)) nstk t_1)
  ------------------------------------------------------------ "T-jump"
  (acc-empty ilist pc (jump l) nstk t_1)
  ]

  [
   (acc-empty ilist (add pc 1) (fetch-i ilist (add pc 1)) nstk t_1)
   (acc-empty ilist (add pc l) (fetch-i ilist (add pc l)) nstk t_2)
   ---------------------------------------------------------------------- "T-choice"
   (acc-empty ilist pc (choice l) nstk ,(or (term t_1) (term t_2)))
   ]

  [
   (acc-empty ilist (add pc l) (fetch-i ilist (add pc l)) nstk t_1)
   ---------------------------------------------------------------------- "T-commit"
   (acc-empty ilist pc (commit l) nstk t_1)
   ]

  [
   ------------------------------------------------------ "T-end"
   (acc-empty ilist pc end nstk #t)
   ]

  [
   ;; (acc-empty ilist (add pc 1) (fetch-i ilist (add pc 1)) (pc_1 ...) t_1)
   (acc-empty ilist (add pc l) (fetch-i ilist (add pc l)) ((add pc l) pc_1 ...) t_2)
   -------------------------------------------------------------------------------------------- "T-call"
   (acc-empty ilist pc (call l) (pc_1 ...) t_2)
   ]

  [
   (acc-empty ilist pc_1 (fetch-i ilist pc_1) (pc ...) t_1)
   --------------------------------------------------------------- "T-return"
   (acc-empty ilist pc_2 return (pc_1 pc ...) t_1)
   ])

(define-metafunction
  LPEGTypes
  check-pl : pl pc -> boolean
  [(check-pl (pc_1 ... pc pc_2 ...) pc) #t]
  [(check-pl pl pc) #f])

(define-judgment-form
  LPEGTypes
  #:mode (gen-loop I I I I O O)
  #:contract (gen-loop ilist pc i pl pl t)
  [
   (where pc_3 (add pc l))
   --------------------------------------------------------------------------------------------- "T-call-loop"
   (gen-loop ilist pc (call l) (pc_1 ... pc_3 pc_2 ...) (pc_1 ... pc_3 pc_2 ...) #t)
   ]

  [
   (where pc_3 (add pc l))
   (side-condition ,(not (term (check-pl (pc_1 ...) pc_3))))
   (gen-loop ilist pc_3 (fetch-i ilist pc_3) (pc_3 pc_1 ...) pl_2 t_1)
   --------------------------------------------------------------------------------------- "T-call"
   (gen-loop ilist pc (call l) (pc_1 ...) pl_2 t_1)
   ]

  ;; 
  ;;
  ;;  (side-condition (not (pertence (add pc l) em (pc_1 ...)
 ;;   ----------------------------------------------------------------------
 ;;   (gen-loop ilist pc (call l) (pc_1 ...) (((add pc l) pc_1 ...) )

  ;; [
  ;;  ------------------------------------------------------ "T-i"
  ;;  (gen-loop ilist pc i pl #f)
  ;;  ]
  )

(provide (all-defined-out))
