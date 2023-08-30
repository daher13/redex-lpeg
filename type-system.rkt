#lang racket

(require redex)
(require "lpeg.rkt")

(module type-system racket/base)

(define-extended-language TypeSystem LPEG
  (pc ::= natural)
  (b ::= boolean)
  (c ::= natural)
  (pastl ::= (c ...))
  (pastc ::= (l ...))
  (it ::= (pastl pastc b)) ;; input type
  (ot ::= (pastl pastc b)) ;; output type
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
   (ts ilist pc_1 i_1 (pastl pastc #f) ot)
   --------------------------------------------------------------------------- "T-char"
   (ts ilist pc (char ch) (pastl pastc b) ot)
   ]

  [
   --------------------------------------------------------------------------- "T-return"
   (ts ilist pc return it it)
   ]

  [
   --------------------------------------------------------------------------- "T-fail"
   (ts ilist pc fail (pastl pastc b) (pastl pastc #t))
   ]

  [
   --------------------------------------------------------------------------- "T-end"
   (ts ilist pc end it it)
   ]

  [
   ;; goto label
   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))
   (ts ilist pc_1 i_1 (pastl pastc #t) (pastl_1 pastc_1 b_1))

   ;; goto next
   (where pc_2 (sum pc 1))
   (where i_2 (fetch-i ilist pc_2))
   (ts ilist pc_2 i_2 (pastl pastc #t) (pastl_2 pastc_2 b_2))

   ;; results
   (where b_3 ,(or (term b_1) (term b_2)))
   --------------------------------------------------------------------------- "T-choice"
   (ts ilist pc (choice l) (pastl pastc b) (pastl pastc b_3))
   ]

  [
   (side-condition ,(> (term l) 0))

   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))

   (ts ilist pc_1 i_1 (pastl pastc b) ot)
   --------------------------------------------------------------------------- "T-commit-positive"
   (ts ilist pc (commit l) (pastl pastc b) ot)
   ]

  [
   (side-condition ,(< (term l) 0))
   (side-condition ,(cond [(> (length (term (c ...))) 0) (not (eq? (term pc) (car (term (c ...)))))]
                          [else #t]))
   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))
   (where pastc_1 (pc c ...))

   (ts ilist pc_1 i_1 (pastl pastc_1 b) ot)
   --------------------------------------------------------------------------- "T-commit-negative"
   (ts ilist pc (commit l) (pastl (c ...) b) ot)
   ]

  [
   (side-condition ,(not (term b_0)))
   --------------------------------------------------------------------------- "T-commit-passed"
   (ts ilist pc (commit l) (pastl (pc c ...) b_0) (pastl (c ...) #t))
  ]

  [
   ;; first option - goto label
   (side-condition ,(not (member (term pc) (term pastl))))
   (where pc_1 (sum pc l))
   (where i_1 (fetch-i ilist pc_1))
   (ts ilist pc_1 i_1 (pastl pastc b) ot_1)
   --------------------------------------------------------------------------- "T-call"
   (ts ilist pc (call l) (pastl pastc b) ot_1)
   ]
  )

(provide (all-defined-out))

;; (define lpeg (term ((call 2) (jump 3) (char 2) return end)))
;; (judgment-holds (ts ,lpeg 0 ,(car lpeg) (() #t) to) to)
