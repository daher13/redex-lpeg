#lang racket

(require redex)

(require "lang.rkt")

(define-metafunction
  LPEG
  ecompile-e : e -> ilist
  [(ecompile-e terminal) ((char terminal))]
  [(ecompile-e x) ((opencall x))]
  [(ecompile-e (@ e_1 e_2)) ,(append (term (ecompile-e e_1)) (term (ecompile-e e_2)))]
  [(ecompile-e (+ e_1 e_2)) ,(append (term ((choice ,(+ (length (term ilist_1)) 2))))
                                       (term ilist_1)
                                       (term ((commit ,(+ (length (term ilist_2)) 1))))
                                       (term ilist_2))
                              (where ilist_1 (ecompile-e e_1))
                              (where ilist_2 (ecompile-e e_2))
                              ]
  [(ecompile-e (* e)) ,(append (term ((choice ,(+ (length (term ilist)) 2))))
                                 (term ilist)
                                 (term ((commit ,(* (+ (length (term ilist)) 1) -1)))))
                        (where ilist (ecompile-e e))]
  [(ecompile-e (! e)) ,(append (term ((choice ,(+ (length (term ilist)) 3))))
                                 (term ilist)
                                 (term ((commit 1)))
                                 (term (fail)))
                        (where ilist (ecompile-e e))])

(define-metafunction
  LPEG
  ecompile-prod : prod -> b
  [(ecompile-prod (x e)) (x ,(append (term (ecompile-e e))
                                     (term (return))
                                     ))])

(define-metafunction
  LPEG
  ecompile-g : g -> blist
  [(ecompile-g ()) ()]
  [(ecompile-g ((x_1 e_1) prod_2 ...)) ,(append
                                            (term ((ecompile-prod (x_1 e_1))))
                                            (term (ecompile-g (prod_2 ...))))])

(define-metafunction
  LPEG
  count-i : blist -> natural
  ((count-i ()) 0)
  ((count-i ((x_1 ilist_1) (x ilist) ...)) ,(+ (length (term ilist_1))
                                              (term (count-i ((x ilist) ...))))))

(define-metafunction
  LPEG
  add-begin-end : blist -> blist
  [(add-begin-end blist) ,(append
                           (term ((BEGIN ((opencall x_0)
                                          (jump ,(+ (term (count-i blist)) 1))))))
                           (term blist)
                           (term ((END (end)))))
                         (where (x_0 ilist) ,(list-ref (term blist) 0))
                         ])

(define-metafunction
  LPEG
  find-block-index : blist x -> natural
  [(find-block-index (b_1 ... (x_1 ilist_1) b_2 ...) x_1) (count-i (b_1 ...))])

(define-metafunction
  LPEG
  replace-opencall : blist ilist natural -> ilist ;; natural = instr index
  [(replace-opencall blist ((opencall x) i ...) natural)
   ,(append
     (term ((call ,(- (term (find-block-index blist x)) (term natural)))))
     (term (replace-opencall blist (i ...) ,(+ (term natural) 1))))]
  [(replace-opencall blist (i_1 i ...) natural)
   ,(append
     (term (i_1))
     (term (replace-opencall blist (i ...) ,(+ (term natural) 1))))]
  [(replace-opencall blist () natural) ()])

(define-metafunction
  LPEG
  extract-ilist : blist -> ilist
  [(extract-ilist ((x ilist))) ilist]
  [(extract-ilist ((x ilist) b_1 ...)) ,(append (term ilist)
                                                (term (extract-ilist (b_1 ...))))])

(define-metafunction
  LPEG
  ecompile : any ->  ilist
  [(ecompile g) ilist_1
                (where blist (ecompile-g g))
                (where blist_1 (add-begin-end blist))
                (where ilist (extract-ilist blist_1))
                (where ilist_1 (replace-opencall blist_1 ilist 0))
                ]
  [(ecompile e) ,(append (term (ecompile-e e)) (term (end)))])

(provide ecompile)

;; (define g (term (
            ;; (S (+ 1 2))
            ;; (P (* 2)))))

;; (term (ecompile ,g))

;; (define e (term (+ 1 1)))
;; (term (ecompile ,e))
