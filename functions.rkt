#lang racket

(require redex)
(require "lpeg.rkt")

(define-extended-language TypeSystem LPEG
  (pc ::= natural)
  (b ::= boolean bt bf) ;; bt = blocked true; bf = blocked false
  (cml ::= l) ;; label for commits
  (cmb ::= b) ;; boolean for commits
  (cme ::= (cml cmb)) ;; commit entry
  (pastc ::= (cme ...)) ;; commit stack
  (cll ::= l) ;; label for calcl
  (clb ::= b) ;; boolean for calcl
  (cle ::= (cll clb)) ;; call entry
  (pastl ::= (cle ...))
  )

(define-metafunction TypeSystem
  fetch-i : ilist pc -> i
  [(fetch-i ilist pc) ,(list-ref (term ilist) (term pc))])

(define-metafunction TypeSystem
  sum : integer integer -> integer
  [(sum integer_1 integer_2) ,(+ (term integer_1) (term integer_2))])

(define-metafunction TypeSystem
  is-negative-commit : i -> b
  [(is-negative-commit (commit l)) #t
                                   (side-condition (< (term l) 0))]
  [(is-negative-commit i) #f])

(define-metafunction TypeSystem
  is-fail : i -> b
  [(is-fail fail) #t]
  [(is-fail _) #f])

(define-metafunction TypeSystem
  update-head : ((l b) ...) boolean -> ((l b) ...)
  [(update-head ((l_1 b_1) ... (l b)) boolean) ((l_1 b_1) ... (l boolean))]
  [(update-head () _) ()])

(define-metafunction TypeSystem
  fetch-tail : ((l b) ...) -> b
  [(fetch-tail ((l_1 b_1) ... (l b))) b]
  [(fetch-tail ()) #t])

(define-metafunction TypeSystem
  find-e : ((l b) ...) l -> (l b) or ()
  [(find-e ((l_1 b_1) ... (l b) (l_2 b_2) ...) l) (l b)]
  [(find-e _ _) ()])

(define-metafunction TypeSystem
  update-pastl : pastl -> pastl
  [(update-pastl ((cll_1 clb_1) cle ...)) ((cll_1 #t) cle_1 ...)
                                          (where (cle_1 ...) (update-pastl (cle ...)))]
  [(update-pastl ()) ()])

(define-metafunction TypeSystem
  fetch-pastl-interval : pastl l -> pastl
  [(fetch-pastl-interval (cle ... (l b) cle_1 ...) l) ((l b) cle_1 ...)]
  [(fetch-pastl-interval pastl l) ()])

(define-metafunction TypeSystem
  unify-b : pastl -> b
  [(unify-b ((l b) cle ...)) ,(or (term b) (term b_1))
                             (where b_1 (unify-b (cle ...)))]
  [(unify-b ()) #f])

(provide (all-defined-out))
