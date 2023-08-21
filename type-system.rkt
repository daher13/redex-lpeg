#lang racket

(require redex)

(require "lang.rkt")
(require "aux.rkt")

(define-extended-language
  LPEGTypes
  LPEG
  (pc ::= natural)
  (stk ::= (pc ...))
  (pl ::= (pc ...)) ;; past labels
  (lp ::= natural nil emp) ;; which instruction enters in loop
  (t ::= boolean emp)
  ) ;; acc empty

(define-metafunction
  LPEGTypes
  check-pl : pl pc -> boolean
  [(check-pl (pc_1 ... pc pc_2 ...) pc) #t]
  [(check-pl pl pc) #f])

(define-judgment-form
  LPEGTypes
  #:mode (acc-empty I I I I O)
  #:contract (acc-empty ilist pc i stk t)
  [
   ------------------------------------------------ "T-char"
   (acc-empty ilist pc (char ch) stk #f)
   ]

  [
  (acc-empty ilist l (fetch-i ilist (add pc l)) stk t_1)
  ------------------------------------------------------------ "T-jump"
  (acc-empty ilist pc (jump l) stk t_1)
  ]

  [
   (acc-empty ilist (add pc 1) (fetch-i ilist (add pc 1)) stk t_1)
   (acc-empty ilist (add pc l) (fetch-i ilist (add pc l)) stk t_2)
   ---------------------------------------------------------------------- "T-choice"
   (acc-empty ilist pc (choice l) stk ,(or (term t_1) (term t_2)))
   ]

  [
   (acc-empty ilist (add pc l) (fetch-i ilist (add pc l)) stk t_1)
   ---------------------------------------------------------------------- "T-commit"
   (acc-empty ilist pc (commit l) stk t_1)
   ]

  [
   ------------------------------------------------------ "T-end"
   (acc-empty ilist pc end stk #t)
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

(define-judgment-form
  LPEGTypes
  #:mode (has-loop I I I I O)
  #:contract (has-loop ilist pc i pl lp)

  (
   (has-loop ilist (add pc 1) (fetch-i ilist (add pc 1)) pl lp_1)
   ------------------------------------------------------------- "T-char"
   (has-loop ilist pc (char ch) pl lp_1)
   )

  (;; this instruction simulates backtrack
   (has-loop ilist (add pc l) (fetch-i ilist (add pc l)) pl lp)
   ------------------------------------------------------------------------------- "T-choice"
   (has-loop ilist pc (choice l) pl lp)
   )

  (
   (has-loop ilist (add pc 1) (fetch-i ilist (add pc 1)) pl lp)
   ------------------------------------------------------------------------------- "T-choice-next"
   (has-loop ilist pc (choice l) pl lp)
   )

  (
   (side-condition ,(not (term (check-pl (pc_1 ... ) (add pc l)))))
   (has-loop ilist (add pc l) (fetch-i ilist (add pc l)) ((add pc l) pc_1 ...) lp)
   ------------------------------------------------------------------------------- "T-call"
   (has-loop ilist pc (call l) (pc_1 ...) lp)
   )

  (;; generates one nil on output
   ;; this instruction simulates same effect of return.
   (side-condition ,(not (term (check-pl (pc_1 ... ) (add pc 1)))))
   (has-loop ilist (add pc 1) (fetch-i ilist (add pc 1)) ((add pc 1) pc_1 ...) lp)
   ------------------------------------------------------------------------------- "T-call-next"
   (has-loop ilist pc (call l) (pc_1 ...) lp)
   )

  (
   (where pc_3 (add pc l)) ;; if you put add function directly on pattern, it doesn't match
   ----------------------------------------------------------------------------------------- "T-call-loop"
   (has-loop ilist pc (call l) (pc_1 ... pc_3 pc_2 ...) pc)
   )

  (
   ------------------------------------------------------ "T-end"
   (has-loop ilist pc end pl nil)
   )

  (
   ------------------------------------------------------ "T-return"
   (has-loop ilist pc end pl nil)
   )

  (
   (where pc_2 (add pc l))
   (side-condition ,(not (term (check-pl (pc_1 ... ) pc_2))))
   (has-loop ilist pc_2 (fetch-i ilist pc_2) (pc_2 pc_1 ...) lp)
   --------------------------------------------------------------------- "T-commit"
   (has-loop ilist pc (commit l) (pc_1 ...) lp)
   )

  (
   (where pc_3 (add pc l)) ;; if you put add function directly on pattern, it doesn't match
   (side-condition ,(< (term l) 0))
   ----------------------------------------------------------------------------------------- "T-commit-loop"
   (has-loop ilist pc (commit l) (pc_1 ... pc_3 pc_2 ...) pc_3)
   )

  (
   (where pc_3 (add pc l)) ;; if you put add function directly on pattern, it doesn't match
   (side-condition ,(judgment-holds (acc-empty ilist pc_3 (fetch-i ilist pc_3) () #t)))
   -------------------------------------------------------------------------------------- "T-commit-loop-acc-empty"
   (has-loop ilist pc (commit l) (pc_1 ... pc_3 pc_2 ...) emp)
   )
  )

(provide (all-defined-out))
