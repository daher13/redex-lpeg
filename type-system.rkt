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
  sum : integer natural -> natural
  [(sum integer natural) ,(+ (term integer) (term natural))])

(define-judgment-form TypeSystem
  #:mode (ts I I I I I O)

  [
   (where pc_1 (sum pc 1))
   (ts ilist pc_1 (fetch-i ilist pc_1) pastl pastc t)
   --------------------------------------------------------------------------- "T-emp"
   (ts ilist pc emp pastl pastc t)
   ]

  [
   (where pc_1 (sum pc 1))
   (ts ilist pc_1 (fetch-i ilist pc_1) pastl pastc (pastl_1 pastc_1 _))
   --------------------------------------------------------------------------- "T-char"
   (ts ilist pc (char _) pastl pastc (pastl_1 pastc_1 #f))
   ]

  [
   (where t (pastl pastc #f))
   --------------------------------------------------------------------------- "T-return"
   (ts _ pc return pastl pastc t)
   ]

  [
   --------------------------------------------------------------------------- "T-end"
   (ts ilist pc end pastl pastc (pastl pastc #f))
   ]

  [
   (where pc_1 (sum pc l))
   (ts ilist pc_1 (fetch-i ilist pc_1) pastl pastc t)
   --------------------------------------------------------------------------- "T-jump"
   (ts ilist pc (jump l) pastl pastc t)
   ]

  [
   (side-condition ,(>= (term l) 0))
   ;; first option - goto label
   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))
   (ts ilist pc_1 i_1 pastl pastc ((l_1 ...) (c_1 ...) b_1))
   ;; second option - goto next
   (where pc_2 (sum pc 1))
   (where i_2 (fetch-i ilist pc_2))
   (ts ilist pc_2 i_2 pastl pastc ((l_2 ...) (c_2 ...) b_2))
   ;; results
   (where pastl_f (l_1 ... l_2 ...))
   (where pastc_f (c_1 ... c_2 ...))
   (where b_f ,(and (term b_1) (term b_2)))
   --------------------------------------------------------------------------- "T-call"
   (ts ilist pc (choice l) pastl pastc (pastl_f pastc_f b_f))
   ]

  [
   (side-condition ,(>= (term l) 0))
   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))
   (ts ilist pc_1 i_1 pastl pastc t)
   --------------------------------------------------------------------------- "T-commit"
   (ts ilist pc (commit l) pastl pastc t)
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
   (where pastl_f (l_1 ... l_2 ...))
   (where pastc_f (c_1 ... c_2 ...))
   (where b_f ,(or (term b_1) (term b_2)))
   --------------------------------------------------------------------------- "T-choice"
   (ts ilist pc (call l) pastl pastc (pastl_f pastc_f b_f))
   ]
  )

(provide (all-defined-out))

;; (define ilist (term ((char 1) emp end)))
;; (define i (car ilist))
;; (judgment-holds (ts ,ilist 0 ,i () () t) t)

(require "comp-peg-lpeg.rkt")

(define lpeg (term (peg->lpeg ((S (/ B 1)) (B (• (• 2 S) 3))))))
