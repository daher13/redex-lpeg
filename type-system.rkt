#lang racket

(require redex)
(require "lpeg.rkt")

(module type-system racket/base)

(define-extended-language TypeSystem LPEG
  (pc ::= natural)
  (b ::= boolean)
  (c ::= natural)
  (pastl ::= (l ...))
  (pastc ::= (c ...))
  (it ::= (pastl b)) ;; input type
  (ot ::= (pastl b)) ;; output type
  )

(define-metafunction TypeSystem
  fetch-i : ilist pc -> i
  [(fetch-i ilist pc) ,(list-ref (term ilist) (term pc))])

(define-metafunction TypeSystem
  sum : integer integer -> integer
  [(sum integer_1 integer_2) ,(+ (term integer_1) (term integer_2))])

(define-judgment-form TypeSystem
  #:mode (ts I I I I O)
  #:contract (ts ilist pc i it ot) ;; t before and t after

  [
   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))
   (ts ilist pc_1 i_1 (pastl #f) (pastl_1 b_1))
   --------------------------------------------------------------------------- "T-char"
   (ts ilist pc (char ch) (pastl b) (pastl_1 #f))
   ]

  [
   --------------------------------------------------------------------------- "T-return"
   (ts ilist pc return (pastl_1 b_1) (() b_1))
   ]

  [
   --------------------------------------------------------------------------- "T-fail"
   (ts ilist pc fail (pastl b) (pastl #t))
   ]

  [
   --------------------------------------------------------------------------- "T-end"
   (ts ilist pc end it it)
   ]

  [
   (where pc_1 (sum pc 1))
   (where i_1 (fetch-i ilist pc_1))
   (ts ilist pc_1 i_1 (pastl #t) (pastl_1 b_1))
   --------------------------------------------------------------------------- "T-emp"
   (ts ilist pc emp (pastl b) (pastl_1 #t))
   ]

  [
   ;; goto label
   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))
   (ts ilist pc_1 i_1 (pastl b) (pastl_1 b_1))

   ;; goto next
   (where pc_2 (sum pc 1))
   (where i_2 (fetch-i ilist pc_2))
   (ts ilist pc_2 i_2 (pastl b) (pastl_2 b_2))

   ;; results
   (where b_3 ,(or (term b_1) (term b_2)))
   (where (l_1 ...) pastl_1)
   (where (l_2 ...) pastl_2)
   (where pastl_3 (l_1 ... l_2 ...))
   --------------------------------------------------------------------------- "T-choice"
   (ts ilist pc (choice l) (pastl b) (pastl_3 b_3))
   ]

  [
   (side-condition ,(> (term l) 0))

   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 (pastl b) ot)
   --------------------------------------------------------------------------- "T-commit"
   (ts ilist pc (commit l) (pastl b) ot)
   ]

  [
   (side-condition ,(< (term l) 0))
   --------------------------------------------------------------------------- "T-commit-negative"
   (ts ilist pc (commit l) (pastl #f) (pastl #t))
   ]

  [
   ;; first option - goto label
   (side-condition ,(> (term l) 0))
   (where pc_1 (sum pc l))
   (side-condition ,(cond [(member (term pc_1) (term pastl)) (not (term b))]
                          [else #t]))
   (where i_1 (fetch-i ilist pc_1))
   (ts ilist pc_1 i_1 (pastl b) (pastl_1 b_1))

   ;; second option - goto next
   (where pc_2 (sum pc 1))
   (where i_2 (fetch-i ilist pc_2))
   (ts ilist pc_2 i_2 (pastl b_1) (pastl_2 b_2))

   ;; results
   (where b_3 ,(and (term b_1) (term b_2)))
   --------------------------------------------------------------------------- "T-call-positive"
   (ts ilist pc (call l) (pastl b) (pastl_2 b_3))
   ]

  [
   (side-condition ,(< (term l) 0))
   (side-condition ,(not (member (term pc) (term (l_0 ...)))))
   ;; first option - goto label
   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))
   (ts ilist pc_1 i_1 ((pc l_0 ...) b) (pastl_1 b_1))

   ;; second option - goto next
   (where pc_2 (sum pc 1))
   (where i_2 (fetch-i ilist pc_2))
   (ts ilist pc_2 i_2 ((l_0 ...) b_1) (pastl_2  b_2))

   ;; results
   (where b_3 ,(and (term b_1) (term b_2)))
   --------------------------------------------------------------------------- "T-call-negative"
   (ts ilist pc (call l) ((l_0 ...) b) (pastl_2 b_3))
   ]

  [
   (side-condition ,(< (term l) 0))
   (side-condition ,(member (term pc) (term pastl)))
   (where pastl_1 ,(cons (term pc) (term pastl)))
   --------------------------------------------------------------------------- "T-call-passed"
   (ts ilist pc (call l) (pastl #f) (pastl_1 #f))
   ]
  )

(provide (all-defined-out))
