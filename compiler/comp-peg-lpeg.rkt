#lang racket

(require redex)

(require "../peg.rkt")
(require "../lpeg.rkt")

(define-union-language PEG+LPEG PEG LPEG)

(define-extended-language Comp PEG+LPEG
  (i ::= ....
     (opencall x)
     )
  (b ::= (x ilist)) ;; block
  (blist ::= (b ...))
  (pc ::= natural)
  (bi ::= (x natural)) ;; block index
  (bilist ::= (bi ...))
  )

(define-metafunction Comp
  ;; compile peg expression
  e->ilist : e -> ilist
  [(e->ilist terminal) ((char terminal))]
  [(e->ilist ϵ) ()]
  [(e->ilist (/ e_1 e_2)) ((choice l_1)
                         i_1 ...
                         (commit l_2)
                         i_2 ...)
                        (where (i_1 ...) (e->ilist e_1))
                        (where (i_2 ...) (e->ilist e_2))
                        (where l_1 ,(+ (length (term (i_1 ...))) 2))
                        (where l_2 ,(+ (length (term (i_2 ...))) 1))]

  [(e->ilist (• e_1 e_2)) (i_1 ... i_2 ...)
                        (where (i_1 ...) (e->ilist e_1))
                        (where (i_2 ...) (e->ilist e_2))]

  [(e->ilist x) ((opencall x))]

  [(e->ilist (* e)) ((choice ,(+ (length (term (i ...))) 2))
                   i ...
                   (commit l))
                  (where (i ...) (e->ilist e))
                  (where l ,(- (+ (length (term (i ...))) 1)))]

  [(e->ilist (! e)) ((choice l)
                   i ...
                   (commit 1)
                   fail)
                  (where (i ...) (e->ilist e))
                  (where l ,(+ (length (term (i ...))) 3))]
  )

(define-metafunction Comp
  ;; compile peg production
  prod->b : prod -> b
  [(prod->b (x e)) (x (i ... return))
                     (where (i ...) (e->ilist e))
                     ])

(define-metafunction Comp
  ;; compile peg grammar
  g->blist : g -> blist
  [(g->blist ()) ()]
  [(g->blist ((x_1 e_1) (x e) ...))
   blist
   (where (b ...) (g->blist ((x e) ...)))
   (where blist ((prod->b (x_1 e_1)) b ...))
   ])

(define-metafunction Comp
  ;; add initial and final instructions
  add-edges : ilist -> ilist
  [(add-edges (i ...)) (
                        ;; (call 2)
                        ;; (jump ,(+ (length (term (i ...))) 1)) ;; jump to the end
                        i ...
                        end)])

(define-metafunction Comp
  ;; fetch block instructions
  fetch-ilist : blist x -> ilist
  [(fetch-ilist (b_1 ... (x ilist) b_2 ...) x) ilist]
  [(fetch-ilist (b ...) x) ()])

(define-metafunction Comp
  ;; mount blist only with accessible states
  mount-blist : (x ...) blist -> blist
  [(mount-blist () blist) ()]
  [(mount-blist (x x_1 ...) blist) (b b_1 ...)
                             (where ilist (fetch-ilist blist x))
                             (where b (x ilist))
                             (where (b_1 ...) (mount-blist (x_1 ...) blist))])

(define-metafunction Comp
  ;; fetch block index considering ilist
  fetch-b-index : blist x -> natural
  [(fetch-b-index ((x ilist) b ...) x) 0]
  [(fetch-b-index ((x_1 ilist_1) b ...) x) ,(+ (length (term ilist_1)) (term natural))
                                         (where natural (fetch-b-index (b ...) x))])

(define-metafunction Comp
  ;; fetch a list with called states (for reduction)
  fetch-opcall : ilist -> (x ...)
  [(fetch-opcall ()) ()]
  [(fetch-opcall ((opencall x) i ...)) (x
                                        x_1 ...)
                                       (where (x_1 ...) (fetch-opcall (i ...)))]
  [(fetch-opcall (i_1 i_2 ...)) (fetch-opcall (i_2 ...))])

(define-metafunction Comp
  ;; fetch accessible states (for reduction)
  fetch-reach : blist (x ...) (x ...) -> (x ...) ;; blist x_to_check x_checked x_end
  [(fetch-reach blist () (x ...)) (s10)] ;; add end state
  [(fetch-reach blist (x_1 x ...) (x_5 ... x_1 x_6 ...))
   (fetch-reach blist (x ...) (x_5 ... x_1 x_6 ...))]
  [(fetch-reach blist (x x_1 ...) (x_5 ...)) (x x_3 ...)
                                (where ilist (fetch-ilist blist x))
                                (where (x_2 ...) (fetch-opcall ilist))
                                (where (x_3 ...) (fetch-reach blist (x_2 ... x_1 ...) (x_5 ... x)))
                                ])

(define-metafunction Comp
  ;; remove unreachable states
  reduce-blist : blist -> blist
  [(reduce-blist blist)
   blist_1
   (where ((x_1 ilist_1) b ...) blist)
   (where (x ...) (fetch-reach blist (x_1) ())) ;; fetch reachable states
   (where blist_1 (mount-blist (x ...) ((x_1 ilist_1) b ...))) ;; remount with reachable states
   ])

(define-metafunction Comp
  extract-ilist : blist -> ilist
  [(extract-ilist ((x ilist))) ilist]
  [(extract-ilist ((x ilist) b_1 ...)) (i_1 ... i_2 ...)
                                       (where (i_1 ...) ilist)
                                       (where (i_2 ...) (extract-ilist (b_1 ...)))])

(define-metafunction Comp
  ;; replace opencall and openjump instructions
  rpl-opcall : ilist blist natural -> ilist
  [(rpl-opcall () blist natural) ()]
  [(rpl-opcall ((opencall x) i ...) blist natural) (i_2 i_1 ...) ;; for opencall instruction
                                                   (where l ,(- (term (fetch-b-index blist x)) (term natural)))
                                                   (where i_2 (call l))
                                                   (where (i_1 ...) (rpl-opcall (i ...) blist ,(+ (term natural) 1)))]

  [(rpl-opcall (i_1 i ...) blist natural) (i_1 i_2 ...)
                                          (where (i_2 ...) (rpl-opcall (i ...) blist ,(+ (term natural) 1)))])

(define-metafunction Comp
  ;; fetch blocks index list
  fetch-bilist : blist blist -> (bi ...) ;; blist remaining_blist
  [(fetch-bilist blist ()) ()]
  [(fetch-bilist blist ((x_1 ilist_1) b ...))
   (
    (x_1 (fetch-b-index blist x_1))
    bi_2 ...
    )
   (where (bi_2 ...) (fetch-bilist blist (b ...)))
   ])

(define-metafunction Comp
  peg->lpeg : g -> (ilist bilist)
  [(peg->lpeg g) (ilist_2 bilist)
   (where blist_1 (g->blist g)) ;; compile peg grammar to lpeg block-list
   ;; (where blist_2 (reduce-blist blist_1)) ;; reduce block
   (where bilist (fetch-bilist blist_1 blist_1)) ;; change to blist_2 if reduction
   (where ilist_1 (extract-ilist blist_1)) ;; extract lpeg instructions
   (where ilist_2 (rpl-opcall ilist_1 blist_1 0)) ;; replace opencall and openjump
   ;; (where ilist_3 (add-edges ilist_2)) ;; add initial and final states
   ]
  )


;; (provide peg->lpeg)

(provide (all-defined-out))
