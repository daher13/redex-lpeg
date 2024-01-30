#lang racket

(require redex)
(require "lpeg.rkt")


(define-extended-language Equation LPEG
  (pc ::= natural)
  (x ::= ∧ ∨)
  (eq ::=
      boolean ;; char and fail
      (x eq eq) ;; choice
      (x eq eq)
      (eq 'closed) ;; commits and return
      (* eq) ;; negative commit
      (bind-type pc) ;; return
      (type-of pc)
      ∅
      )
  (i ::= .... (i pc))
  )

(define-metafunction Equation
  fetch-i : ilist pc -> i
  [(fetch-i ilist pc) ,(list-ref (term ilist) (term pc))])

(define-metafunction Equation
  sum : integer integer -> integer
  [(sum integer_1 integer_2) ,(+ (term integer_1) (term integer_2))])

;; (define-metafunction Equation
;;   rpl-eq : eq eq -> eq ;; replace inside equation
;;   [(rpl-eq eq ∅) eq]
;;   [(rpl-eq boolean eq) (∧ eq boolean)]
;;   [(rpl-eq (x (eq_1 'closed) eq_2) eq) (∨ (eq_1 'closed) eq_3) ;; in case of first is closed
;;                                        (where eq_3 (rpl-eq eq_2 eq))]

;;   [(rpl-eq (x eq_1 eq_2) eq) (∨ eq_3 eq_2) ;; in case of first is open
;;                              (where eq_3 (rpl-eq eq_1 eq))]

;;   [(rpl-eq (eq_1 'closed) eq) (∧ (eq_1 'closed) eq)]
;;   )

(define-metafunction Equation
  rpl-eq : eq eq -> eq ;; (equation where-to-replace)
  [(rpl-eq eq ∅) eq]
  [(rpl-eq eq (x (eq_1 'closed) eq_2)) (x (eq_1 'closed) eq_3) ;; in case of first is closed
                                       (where eq_3 (rpl-eq eq eq_2))]

  [(rpl-eq eq (x eq_1 eq_2)) (x eq_3 eq_2) ;; in case of first is open
                             (where eq_3 (rpl-eq eq eq_1))]

  [(rpl-eq eq (eq_1 'closed)) (∧ (eq_1 'closed) eq)]
  [(rpl-eq eq_1 eq) (∧ eq eq_1)]
  )


(define-metafunction Equation
  i->eq : ilist pc eq -> eq
  [(i->eq () pc eq) eq]
  [(i->eq ((char ch) i ...) pc eq) eq_3
                                   (where eq_1 #f)
                                   (where eq_2 (rpl-eq eq_1 eq))
                                   (where pc_1 (sum pc 1))
                                   (where eq_3 (i->eq (i ...) pc_1 eq_2))]

  [(i->eq (fail i ...) pc eq) eq_3
                              (where eq_1 #t)
                              (where eq_2 (rpl-eq eq_1 eq))
                              (where pc_1 (sum pc 1))
                              (where eq_3 (i->eq (i ...) pc_1 eq_2))]

  [(i->eq ((choice l) i ...) pc eq) eq_3
                                    (where eq_1 (∨ ∅ ∅))
                                    (where eq_2 (rpl-eq eq_1 eq))
                                    (where pc_1 (sum pc 1))
                                    (where eq_3 (i->eq (i ...) pc_1 eq_2))]

  [(i->eq ((call l) i ...) pc eq) eq_3
                                  (where pc_2 (sum pc l))
                                  (where eq_1 (type-of pc_2))
                                  (where eq_2 (rpl-eq eq_1 eq))
                                  (where pc_1 (sum pc 1))
                                  (where eq_3 (i->eq (i ...) pc_1 eq_2))]

  ;; [(i->eq ((commit natural) i ...) pc eq) eq_3
  ;;                                         (where pc_2 (sum pc l))
  ;;                                         (where eq_1 (type-of pc_2))
  ;;                                         (where eq_2 (rpl-eq eq_1 eq))
  ;;                                         (where pc_1 (sum pc 1))
  ;;                                         (where eq_3 (i->eq (i ...) pc_1 eq_2))]
  ;; [(i->eq (commit natural) eq) (eq 'closed)]
  ;; [(i->eq (commit integer) eq) ((* eq) 'closed)]
  ;; [(i->eq return pc) (bind-type pc)]
  ;; [(i->eq (choice l)) (∨ ∅ ∅)]
  )

(provide (all-defined-out))
