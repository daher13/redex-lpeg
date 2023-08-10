#lang racket

(require redex)

(require "lang.rkt")

(define-metafunction
  LPEG
  ecompile-aux : e -> ilist
  [(ecompile-aux terminal) ((char terminal))]
  [(ecompile-aux (@ e_1 e_2)) ,(append (term (ecompile-aux e_1)) (term (ecompile-aux e_2)))]
  [(ecompile-aux (+ e_1 e_2)) ,(append (term ((choice ,(+ (length (term ilist_1)) 2))))
                                       (term ilist_1)
                                       (term ((commit ,(+ (length (term ilist_2)) 1))))
                                       (term ilist_2))
                              (where ilist_1 (ecompile-aux e_1))
                              (where ilist_2 (ecompile-aux e_2))
                              ]
  [(ecompile-aux (* e)) ,(append (term ((choice ,(+ (length (term ilist)) 2))))
                                 (term ilist)
                                 (term ((commit ,(* (+ (length (term ilist)) 1) -1)))))
                        (where ilist (ecompile-aux e))]
  [(ecompile-aux (! e)) ,(append (term ((choice ,(+ (length (term ilist)) 3))))
                                 (term ilist)
                                 (term ((commit 1)))
                                 (term (fail)))
                        (where ilist (ecompile-aux e))])

(define-metafunction LPEG
		     ecompile : e -> ilist
		     [(ecompile e) ,(append (term (ecompile-aux e)) (term (end)))]
		     )

(provide ecompile)

