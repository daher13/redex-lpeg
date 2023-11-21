#lang racket

(require redex)

(require "../type-system.rkt")

(define-extended-language Z3 TypeSystem
  (b ::= true false boolean)
  (lexeme ::= string)
  (expr ::= (or lexeme lexeme) lexeme b) ;; expression
  (attr ::= (assert (= lexeme expr))) ;; attribution
  (decl ::= (declare-const string Bool))
  (z3 ::= (attr ...) (decl ...))
  )

(define-metafunction TypeSystem
  ilist->eqlist : ilist pc -> eqlist
  [(ilist->eqlist () pc) ()]

  [(ilist->eqlist ((char ch) i ...) pc) ((pc #f) eq ...)
                                        (where (eq ...) (ilist->eqlist (i ...) (sum pc 1)))]

  [(ilist->eqlist ((choice pc_1) i ...) pc) ((pc (+ (sum pc 1) (sum pc pc_1))) eq ...)
                                            (where (eq ...) (ilist->eqlist (i ...) (sum pc 1)))]

  [(ilist->eqlist ((commit l) i ...) pc) ((pc (sum pc l)) eq ...)
                                         (where (eq ...) (ilist->eqlist (i ...) (sum pc 1)))
                                         (side-condition (> (term l) 0))]

  [(ilist->eqlist ((commit l) i ...) pc) ((pc (sum pc l)) ((sum pc -1) #f) eq ...)
                                         (where (eq ...) (ilist->eqlist (i ...) (sum pc 1)))
                                         (side-condition (< (term l) 0))]

  [(ilist->eqlist (return i ...) pc) (eq ...)
                                     (where (eq ...) (ilist->eqlist (i ...) (sum pc 1)))]

  [(ilist->eqlist (fail i ...) pc) ((pc #t) eq ...)
                                   (where (eq ...) (ilist->eqlist (i ...) (sum pc 1)))]

  [(ilist->eqlist (fail i ...) pc) ((pc #t) eq ...)
                                   (where (eq ...) (ilist->eqlist (i ...) (sum pc 1)))])

(define-metafunction Z3
  eq->attr : eq -> attr
  [(eq->attr (pc #t)) (assert (= ,(~a 't_ (term pc)) true))]
  [(eq->attr (pc #f)) (assert (= ,(~a 't_ (term pc)) false))]
  [(eq->attr (pc_1 pc_2)) (assert (= ,(~a 't_ (term pc_1)) ,(~a 't_ (term pc_2))))]
  [(eq->attr (pc (+ pc_1 pc_2))) (assert (= ,(~a 't_ (term pc)) (or ,(~a 't_ (term pc_1)) ,(~a 't_ (term pc_2)))))])

(define-metafunction Z3
  fetch-consts : integer -> z3
  [(fetch-consts -1) ()]
  [(fetch-consts pc) (decl ...(declare-const ,(~a 't_ (term pc)) Bool))
                     (where (decl ...) (fetch-consts (sum pc -1)))
                     ])


(define-metafunction Z3
  eqlist->asserts : eqlist -> z3
  [(eqlist->asserts ()) ()]
  [(eqlist->asserts (eq_1 eq ...)) (attr attr_1 ...)
                              (where attr (eq->attr eq_1))
                              (where (attr_1 ...) (eqlist->asserts (eq ...)))])


(provide (all-defined-out))
