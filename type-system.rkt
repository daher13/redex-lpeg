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
  [(merge-lists (pc_1 ...) (pc_2 ...))
   ,(remove-duplicates (term
                        (pc_1 ... pc_2 ...)
                        ))
   ]
  )

(define-metafunction
  TypeSystem
  is-negative-l : i -> boolean
  [(is-negative-l (commit l)) ,(< (term l) 0)]
  [(is-negative-l _) #f])

(define-judgment-form
  TypeSystem
  #:mode (ts I I I I I O)
  #:contract (ts ilist pc i pastl pastc t)

  [
   (where pc_1 (add pc 1))
   (ts ilist pc_1 (fetch-i ilist pc_1) pastl pastc (pastl_1 pastc_1 boolean))
   -------------------------------------------------------------------- "T-emp"
   (ts ilist pc emp pastl pastc (pastl_1 pastc_1 boolean))
   ]

  ;; [ ;; fix when char matches and goto next
  ;;  ----------------------------------------------------------------------------- "T-char"
  ;;  (ts ilist pc (char ch) pastl pastc (pastl pastc #f))
  ;;  ]

  [ ;; fixed - goto next instruction
   ;; (where pc_1 (add pc 1))
   ;; (where i (fetch-i ilist pc_1))
   ;; (ts ilist pc_1 i pastl pastc (pastl_1 pastc_1 boolean))
   ----------------------------------------------------------------------------- "T-char"
   (ts ilist pc (char ch) pastl pastc (pastl pastc #f))
   ]


  [
   ----------------------------------------------- "T-end"
   (ts ilist pc end pastl pastc (pastl pastc #t))
   ]

  [
   ------------------------------------------------ "T-fail"
   (ts ilist pc fail pastl pastc (pastl pastc #f))
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

  ;; [
  ;;  (where pc_1 (add pc l))
  ;;  (side-condition ,(not (term (includes (pc_5 ...) pc_1))))
  ;;  ;; first option - goto label
  ;;  (where pastl_10 ,(if (< (term l) 0) (term (pc_1 pc_5 ...)) (term (pc_5 ...))))
  ;;  (where i_1 (fetch-i ilist pc_1))
  ;;  (ts ilist pc_1 i_1 pastl_10 pastc (pastl_1 pastc_1 boolean_1))
  ;;  ;; second option - goto next
  ;;  (where pc_2 (add pc 1))
  ;;  (where i_2 (fetch-i ilist pc_2))
  ;;  (ts ilist pc_2 i_2 (pc_5 ...) pastc (pastl_2 pastc_2 boolean_2)) ;; goto next
  ;;  ;; results
  ;;  (where pastl_3 (merge-lists pastl_1 pastl_2))
  ;;  (where pastc_3 (merge-lists pastc_1 pastc_2))
  ;;  (where boolean_3 ,(and (term boolean_1) (term boolean_2)))
  ;;  ----------------------------------------------------------------------------------- "T-call"
  ;;  (ts ilist pc (call l) (pc_5 ...) pastc (pastl_3 pastc_3 boolean_3))
  ;;  ]

  [
   (where pc_1 (add pc l))
   (side-condition ,(not (term (includes (pc_5 ...) pc_1)))) ;; for state recursion
   ;; first option - goto label
   (where pastl_10 ,(if (< (term l) 0) (term (pc_1 pc_5 ...)) (term (pc_5 ...))))
   (where i_1 (fetch-i ilist pc_1))
   (ts ilist pc_1 i_1 pastl_10 pastc (pastl_1 pastc_1 boolean_1))
   (side-condition ,(if (term (includes (pc_5 ...) pc_1))
                        (eq? (term boolean_1) #f)
                        #t))
   (where pc_2 (add pc 1))
   (where i_2 (fetch-i ilist pc_2))
   (where boolean_10 (is-negative-l (fetch-i ilist pc_2)))
   (side-condition ,(if (term boolean_10)
                        (eq? (term boolean_1) #f)
                        #t))
   ;; (ts ilist pc_2 i_2 (pc_5 ...) pastc (pastl_2 pastc_2 boolean_2)) ;; goto next
   ----------------------------------------------------------------------------------- "T-call"
   (ts ilist pc (call l) (pc_5 ...) pastc (pastl_1 pastc_1 boolean_1))
   ]

  ;; [
  ;;  (where pc_1 (add pc l))
  ;;  --------------------------------------------------------------------------------------- "T-call-loop"
  ;;  (ts ilist pc (commit l) (pc_1 pc_5 ...) pastc ((pc_1 pc_5 ...) pastc #f))
  ;;  ]

  [
   (where pastc_10 ,(if (< (term l) 0) (term (pc_1 pc_5 ...)) (term (pc_5 ...))))
   (where pc_1 (add pc l))
   (where i_1 (fetch-i ilist pc_1))
   (ts ilist pc_1 i_1 pastl pastc_10 (pastl_1 pastc_1 boolean_1))
   (side-condition ,(if (term (includes (pc_5 ...) pc_1))
                        (eq? (term boolean_1) #f)
                        #t))
   --------------------------------------------------------------------------------------- "T-commit"
   (ts ilist pc (commit l) pastl (pc_5 ...) (pastl_1 pastc_1 boolean_1))
   ]
  )

(provide (all-defined-out))
