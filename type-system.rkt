#lang racket

(require redex)
(require "lpeg.rkt")

(module type-system racket/base)

(define-extended-language TypeSystem LPEG
  (pc ::= natural)
  (c ::= integer) ;; commits
  (b ::= boolean)
  (pastl ::= (l ...))
  (pastc ::= (c ...))
  (t ::= (pastl pastc b))
  )

(define-metafunction TypeSystem
  fetch-i : ilist pc -> i
  [(fetch-i ilist pc) ,(list-ref (term ilist) (term pc))])

(define-metafunction TypeSystem
  sum : integer integer -> integer
  [(sum integer_1 integer_2) ,(+ (term integer_1) (term integer_2))])

(define-judgment-form TypeSystem
  #:mode (ts I I I I I O)
  #:contract (ts ilist pc i pastl pastc t)
  [ ;; fix when char matches and goto next
   ----------------------------------------------------------------------------- "T-char"
   (ts _ pc (char ch) pastl pastc (pastl pastc #f))
   ]

  ;; [
  ;;  (where pc_1 (sum pc 1))
  ;;  (ts ilist pc_1 (fetch-i ilist pc_1) pastl pastc (pastl_1 pastc_1 _))
  ;;  --------------------------------------------------------------------------- "T-char"
  ;;  (ts ilist pc (char _) pastl pastc (pastl_1 pastc_1 #f))
  ;;  ]

  [
   --------------------------------------------------------------------------- "T-return"
   (ts _ pc return pastl pastc (pastl pastc #t))
   ]

  [
   --------------------------------------------------------------------------- "T-end"
   (ts _ pc end pastl pastc (pastl pastc #t))
   ]

  [
   --------------------------------------------------------------------------- "T-fail"
   (ts _ pc fail pastl pastc (pastl pastc #t))
   ]

  [
   (where pc_1 (sum pc l))
   (ts ilist pc_1 (fetch-i ilist pc_1) pastl pastc t)
   --------------------------------------------------------------------------- "T-jump"
   (ts ilist pc (jump l) pastl pastc t)
   ]

  [
   ;; first option - goto label
   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))
   (ts ilist pc_1 i_1 pastl pastc ((l_1 ...) (c_1 ...) b_1))
   ;; second option - goto next
   (where pc_2 (sum pc 1))
   (where i_2 (fetch-i ilist pc_2))
   (ts ilist pc_2 i_2 pastl pastc ((l_2 ...) (c_2 ...) b_2))
   ;; results
   (where pastl_3 (l_1 ... l_2 ...))
   (where pastc_3 (c_1 ... c_2 ...))
   (where b_3 ,(or (term b_1) (term b_2)))
   --------------------------------------------------------------------------- "T-choice"
   (ts ilist pc (choice l) pastl pastc (pastl_3 pastc_3 b_3))
   ]

  [
   (where pc_1 (sum pc l))
   (side-condition ,(not (member (term pc_1) (term pastl))))
   ;; first option - goto label
   (where i_1 (fetch-i ilist pc_1))
   (where pastl_1 ,(cond [(< (term l) 0) (append (term pastl)
                                                 (term (pc_1))
                                                 )]
                         [else (term pastl)]))
   (ts ilist pc_1 i_1 pastl_1 () ((l_1 ...) (c_1 ...) b_1))
   ;; second option - goto next
   ;; if next is onb pastc, ignore
   (where pc_2 (sum pc 1))
   (where i_2 (fetch-i ilist pc_2))
   (ts ilist pc_2 i_2 pastl pastc ((l_2 ...) (c_2 ...) b_2))
   ;; results
   (where pastl_3 (l_1 ... l_2 ...))
   (where pastc_3 (c_1 ... c_2 ...))
   (where b_3 ,(and (term b_1) (term b_2)))
   --------------------------------------------------------------------------- "T-call"
   (ts ilist pc (call l) pastl pastc (pastl_3 pastc_3 b_3))
   ]

  [
   (where pc_1 (sum pc l))
   (side-condition ,(not (member (term pc_1) (term pastc))))
   (where i_1 (fetch-i ilist pc_1))
   (where pastc_1 ,(cond [(< (term l) 0) (append (term pastc)
                                                 (term (pc_1))
                                                 )]
                         [else (term pastc)]))
   (ts ilist pc_1 i_1 pastl pastc_1 (pastl_2 pastc_2 boolean_2))
   ----------------------------------------------------------------------------------------- "T-commit"
   (ts ilist pc (commit l) pastl pastc (pastl_2 pastc_2 boolean_2))
   ]

  [
   (where pc_2 (sum pc l))
   ------------------------------------------------------------------------------------------------- "T-commit-loop"
   (ts ilist pc (commit l) pastl (pc_1 ... pc_2 pc_3 ...) (pastl (pc_1 ... pc_2 pc_3 ...) #t))
   ]
  )

(provide (all-defined-out))
