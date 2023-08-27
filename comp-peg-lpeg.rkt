#lang racket

(require redex)

(require "peg.rkt")
(require "lpeg.rkt")

(define-union-language PEG+LPEG PEG LPEG)

(define-extended-language Comp PEG+LPEG
  (i ::= ....
     (opencall x)
     (openjump x)
     )
  (b ::= (x ilist)) ;; block
  (blist ::= (b ...))
  (pc ::= natural)
  )

(define-metafunction Comp
  ;; compile peg expression
  comp-e : e -> ilist
  [(comp-e terminal) ((char terminal))]
  [(comp-e ϵ) ()]
  [(comp-e (/ e_1 e_2)) ((choice l_1)
                         i_1 ...
                         (commit l_2)
                         i_2 ...)
                        (where (i_1 ...) (comp-e e_1))
                        (where (i_2 ...) (comp-e e_2))
                        (where l_1 ,(+ (length (term (i_1 ...))) 2))
                        (where l_2 ,(+ (length (term (i_2 ...))) 1))]

  [(comp-e (• e_1 e_2)) (i_1 ... i_2 ...)
                        (where (i_1 ...) (comp-e e_1))
                        (where (i_2 ...) (comp-e e_2))]

  [(comp-e x) ((opencall x))]

  [(comp-e (* e)) ((choice ,(+ (length (term (i ...))) 3))
                   i ...
                   (commit l))
                  (where (i ...) (comp-e e))
                  (where l ,(- (+ (length (term (i ...))) 1)))]

  [(comp-e (! e)) ((choice l)
                   i ...
                   (commit 1)
                   fail)
                  (where (i ...) (comp-e e))
                  (where l ,(+ (length (term (i ...))) 3))]
  )

(define-metafunction Comp
  ;; compile peg production
  comp-prod : prod -> b
  [(comp-prod (s0 e)) (s0 (i ... (openjump s10)))
                     (where (i ...) (comp-e e))
                     ]
  [(comp-prod (x e)) (x (i ... return))
                     (where (i ...) (comp-e e))
                     ])

(define-metafunction Comp
  ;; compile peg grammar
  comp-g : g -> blist
  [(comp-g ()) ((s10 (end)))]
  [(comp-g ((x_1 e_1) (x e) ...)) ((comp-prod (x_1 e_1))
                                   b ...)
                                  (where (b ...) (comp-g ((x e) ...)))])

(define-metafunction Comp
  ;; fetch block instructions
  fetch-ilist : blist x -> ilist
  [(fetch-ilist (b_1 ... (x ilist) b_2 ...) x) ilist]
  [(fetch-ilist (b ...) x) ()])

(define-metafunction Comp
  ;; fetch a list with called states
  fetch-opcall : ilist -> (x ...)
  [(fetch-opcall ()) ()]
  [(fetch-opcall ((opencall x) i ...)) (x
                                        x_1 ...)
                                       (where (x_1 ...) (fetch-opcall (i ...)))]
  [(fetch-opcall (i_1 i_2 ...)) (fetch-opcall (i_2 ...))])

(define-metafunction Comp
  ;; fetch a list of accessible states starting from one state (ex s0)
  passed-s : blist x (x ...) -> (x ...)
  [(passed-s blist x (x_1 ... x x_2 ...)) (x_1 ... x x_2 ...)] ;; already checked
  [(passed-s blist x (x_1 ...)) (x_1  ... x) ;; no opencall
                                (where ilist (fetch-ilist blist x))
                                (where () (fetch-opcall ilist))]

  [(passed-s blist x (x_1 ...)) (x_5 ...)
                                (where ilist (fetch-ilist blist x))
                                (where (x_2) (fetch-opcall ilist))
                                (where (x_5 ...) (passed-s blist x_2 (x_1 ... x)))]

  [(passed-s blist x (x_1 ...)) (x_6 ...)
                                (where ilist (fetch-ilist blist x))
                                (where (x_2 x_3) (fetch-opcall ilist))
                                (where (x_5 ...) (passed-s blist x_2 (x_1 ... x)))
                                (where (x_6 ...) (passed-s blist x_3 (x_5 ...)))])

(define-metafunction Comp
  ;; mount lpeg removing inaccesssible states
  rm-inacc : (x ...) blist -> blist
  [(rm-inacc () blist) ((s10 (end)))]
  [(rm-inacc (x x_1 ...) blist) (b b_1 ...)
                             (where ilist (fetch-ilist blist x))
                             (where b (x ilist))
                             (where (b_1 ...) (rm-inacc (x_1 ...) blist))])

(define-metafunction Comp
  ;; fetch block index considering ilist
  fetch-b-index : blist x -> natural
  [(fetch-b-index ((x ilist) b ...) x) 0]
  [(fetch-b-index ((x_1 ilist_1) b ...) x) ,(+ (length (term ilist_1)) (term natural))
                                         (where natural (fetch-b-index (b ...) x))])

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

  [(rpl-opcall ((openjump x) i ...) blist natural) (i_2 i_1 ...) ;; for opencall instruction
                                                   (where l ,(- (term (fetch-b-index blist x)) (term natural)))
                                                   (where i_2 (jump l))
                                                   (where (i_1 ...) (rpl-opcall (i ...) blist ,(+ (term natural) 1)))]

  [(rpl-opcall (i_1 i ...) blist natural) (i_1 i_2 ...)
                                          (where (i_2 ...) (rpl-opcall (i ...) blist ,(+ (term natural) 1)))])


(define-metafunction Comp
  peg->lpeg : g -> ilist
  [(peg->lpeg g) ilist_2
   (where blist_1 (comp-g g)) ;; compile peg grammar to lpeg block-list
   (where (x ...) (passed-s blist_1 s0 ())) ;; fetch accessible states
   (where blist_2 (rm-inacc (x ...) blist_1)) ;; remove inaccessible states
   (where ilist_1 (extract-ilist blist_2)) ;; extract lpeg instructions
   (where ilist_2 (rpl-opcall ilist_1 blist_2 0)) ;; replace opencall and openjump
  ])

(provide peg->lpeg)
