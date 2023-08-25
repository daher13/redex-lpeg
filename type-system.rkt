#lang racket

(require redex)

(require "lang.rkt")
(require "aux.rkt")

(define-metafunction
  TypeSystem
  includes : (pc ...) pc -> boolean
  [(includes (pc_1 ... pc pc_2 ...) pc) #t]
  [(includes _ _) #f])

(define-metafunction
  TypeSystem
  merge-lists : (pc ...) (pc ...) -> (pc ...)
  [(merge-lists (pc_1 ...) (pc_2 ...)) ,(remove-duplicates
                                         (term (pc_1 ... pc_2 ...))
                                         )]
  )

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

  ;; [ ;; fix when char matches and goto next
  ;;  ----------------------------------------------------------------------------- "T-char"
  ;;  (ts ilist pc (char ch) pastl pastc (pastl pastc #f))
  ;;  ]

  [ ;; fixed - goto next instruction
   (where pc_1 (add pc 1))
   (ts ilist pc_1 (fetch-i ilist pc_1) pastl pastc (pastl_1 pastc_1 boolean))
   ----------------------------------------------------------------------------- "T-char"
   (ts ilist pc (char ch) pastl pastc (pastl_1 pastc_1 #f))
   ]


  [
   ----------------------------------------------- "T-end"
   (ts ilist pc end pastl pastc (pastl pastc #t))
   ]

  [
   ------------------------------------------------ "T-fail"
   (ts ilist pc fail pastl pastc (pastl pastc #t))
   ]

  [
   ------------------------------------------------- "T-return"
   (ts ilist pc return pastl pastc (pastl pastc #t))
   ]

  [
   (where pc_1 (add pc l))
   (ts ilist pc_1 (fetch-i ilist pc_1) pastl pastc t)
   ----------------------------------------------------- "T-jump"
   (ts ilist pc (jump l) pastl pastc t)
   ]

  [
   ;; first option - goto label
   (where pc_1 (add pc l))
   (where i_1 (fetch-i ilist pc_1))
   (ts ilist pc_1 i_1 pastl pastc (pastl_1 pastc_1 boolean_1))
   ;; second option - goto next
   (where pc_2 (add pc 1))
   (where i_2 (fetch-i ilist pc_2))
   (ts ilist pc_2 i_2 pastl pastc (pastl_2 pastc_2 boolean_2))
   ;; results
   (where pastl_3 (merge-lists pastl_1 pastl_2))
   (where pastc_3 (merge-lists pastc_1 pastc_2))
   (where boolean_3 ,(or (term boolean_1) (term boolean_2)))
   ------------------------------------------------------------------------------- "T-choice"
   (ts ilist pc (choice l) pastl pastc (pastl_3 pastc_3 boolean_3))
   ]

  [
   (side-condition ,(not (term (includes (pc_5 ...) (add pc l)))))
   ;; first option - goto label
   (where pc_1 (add pc l))
   (where i_1 (fetch-i ilist pc_1))
   (ts ilist pc_1 i_1 (pc_1 pc_5 ...) pastc (pastl_1 pastc_1 boolean_1))
   ;; second option - goto next
   (where pc_2 (add pc 1))
   (where i_2 (fetch-i ilist pc_2))
   (ts ilist pc_2 i_2 (pc_5 ...) pastc (pastl_2 pastc_2 boolean_2)) ;; goto next
   ;; results
   (where pastl_3 (merge-lists pastl_1 pastl_2))
   (where pastc_3 (merge-lists pastc_1 pastc_2))
   (where boolean_3 ,(and (term boolean_1) (term boolean_2)))
   ----------------------------------------------------------------------------------- "T-call"
   (ts ilist pc (call l) (pc_5 ...) pastc (pastl_3 pastc_3 boolean_3))
   ]

  [
   (where pc_1 (add pc l))
   (side-condition ,(not (term (includes (pc_5 ...) pc_1))))
   (where i_1 (fetch-i ilist pc_1))
   (ts ilist pc_1 i_1 pastl (pc_1 pc_5 ...) t) ;; goto label
   --------------------------------------------------------------------------------------- "T-commit"
   (ts ilist pc (commit l) pastl (pc_5 ...) t)
   ]

  [
   (where pc_1 (add pc l))
   --------------------------------------------------------------------------------------- "T-commit-loop"
   (ts ilist pc (commit l) pastl (pc_1 pc_5 ...) (pastl (pc_1 pc_5 ...) #f))
   ]
  )

(provide (all-defined-out))
