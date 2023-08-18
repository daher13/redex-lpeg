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
                                     (term (return))))
                                     ])

(define-metafunction
  LPEG
  ecompile-grammar : grammar -> blist
  [(ecompile-grammar (prod)) ((ecompile-prod prod) (END (end)))]
  [(ecompile-grammar ((x_1 e_1) prod_2 ...)) ,(append
                                            (term ((START ((opencall x_1) (jump END)))))
                                            (term ((ecompile-prod (x_1 e_1))))
                                            (term (ecompile-grammar (prod_2 ...))))])

(define-metafunction
  LPEG
  replace-opencall : blist ilist natural -> ilist ;; natural = instr index
  [(replace-opencall blist ((opencall l) i ...) natural)
   ,(append
     (term ((call ,(- (term (find-block-index blist l)) (term natural)))))
     (term (replace-opencall blist (i ...) ,(+ (term natural) 1))))]

  [(replace-opencall blist ((jump END) i ...) natural)
   ,(append
     (term ((jump ,(- (term (find-block-index blist END)) (term natural)))))
     (term (replace-opencall blist (i ...) ,(+ (term natural) 1))))]

  [(replace-opencall blist (i_1 i ...) natural)
   ,(append
     (term (i_1))
     (term (replace-opencall blist (i ...) ,(+ (term natural) 1))))]
  [(replace-opencall blist () natural) ()])

(define-metafunction
  LPEG
  extract-i : blist -> ilist
  [(extract-i ((l ilist))) ilist]
  [(extract-i ((l ilist) b_1 ...)) ,(append (term ilist)
                                          (term (extract-i (b_1 ...))))])

(define-metafunction
  LPEG
  ecompile : any -> ilist
  [(ecompile grammar) (replace-opencall blist ilist 0)
                      (where blist (ecompile-grammar grammar))
                      (where ilist (extract-i blist))
                      ]
  [(ecompile e) ,(append (term (ecompile-e e)) (term (end)))])

(provide ecompile)
