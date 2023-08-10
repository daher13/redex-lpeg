#lang racket

(require redex)

(require "./lang.rkt")

(define-judgment-form
 LPEG
 #:mode (type-i I I I O)
  [
   (type-i (i_1 i ...) pc i_1 t_1)
   (type-i  (i_1 i ...) (add pc 1) (fetch-i (i_1 i ...) (add pc 1)) t_2)
   (side-condition (< pc (size (term (i_1 i ...)))))
   ----------------------------------------------------------------------- "T-seq"
   (type-i (i_1 i ...) (add pc 2) i_1 ,(or (term t_1) (term t_2)))
   ]

 [
  ---------------------------------"T-char"
  (type-i ilist pc (char ch) #f)
  ]

 [
  (type-i ilist l (fetch-i ilist (add pc l)) t_1)
  -------------------------------------------------------- "T-jump"
  (type-i ilist pc (jump l) t_1)
  ]

 [
  (type-i ilist l (fetch-i ilist (add pc l)) t_1)
  -------------------------------------------------------- "T-commit"
  (type-i ilist pc (commit l) t_1)
  ]

 [
  (type-i ilist (add pc 1) (fetch-i ilist (add pc 1)) t)
  ------------------------------------------------------ "T-choice"
  (type-i ilist pc (choice l) t)
  ]

 [
  ------------------------------------------------------ "T-end"
  (type-i ilist pc end #t)
  ]
 )

(provide type-i)
