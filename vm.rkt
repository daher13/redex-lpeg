#lang racket

(require redex)

(define-language PEG
  (terminal ::= natural)
  (e ::=
     terminal
     x
     (@ e e)
     (+ e e)
     (* e)
     (! e))
  (prod ::= (x e))
  (grammar ::= (prod ...))
  (x ::= variable-not-otherwise-mentioned))

(define-extended-language LPEG PEG
  (ch ::= natural)
  (n ::= natural)
  (l ::= integer)
  (i ::=
     (char ch)
     (jump l)
     (choice l)
     (call l)
     return
     (commit l)
     capture
     (any n)
     fail
     failure
     end)
  (ilist ::= (i ...))
  (ip ::= natural)
  (s ::= (ch ...))
  (sp ::= natural) ;; subject_position
  (c ::= (sp ip)) ;; capture ;; subj_position index_of_instruction
  (stke ::= n (n n c)) ;; strack entry ;; (first_option backtrack_option capture)
  (stk ::= (stke ...))
  (pc ::= natural)
  (t ::= true false)
  (state ::= (ilist i ip s sp stk c)))

(define ->e
  (reduction-relation
   LPEG
   #:domain state
   (--> (ilist (char ch_1) ip (ch_2 ch_3 ...) sp stk c)
        (ilist (fetch-next ilist ip) (add ip 1) (ch_3 ...) (add sp 1) stk c)
        (side-condition (eq? (term ch_1) (term ch_2)))
        "char-match")

   (--> (ilist (char ch_1) ip (ch_2 ch ...) sp stk c)
        (ilist fail ip (ch_2 ch ...) sp stk c)
        (side-condition (not (eq? (term ch_1) (term ch_2))))
        "char-not-match")

   (--> (ilist (char ch_1) ip () sp stk c)
        (ilist fail ip () sp stk c)
        "char-empty")

   (--> (ilist (jump l_1) ip s sp stk c)
        (ilist (fetch-i ilist (add ip l_1)) (add ip l_1) s sp stk c)
        "jump")

   (--> (ilist (choice l_1) ip s sp (stke ...) c)
        (ilist (fetch-next ilist ip) (add ip 1) s sp ((ip (add ip l_1) c) stke ...) c)
        "choice")

   (--> (ilist (call l_1) ip s sp (stke ...) c)
        (ilist (fetch-i ilist (add ip l_1)) (add ip l_1) s sp ((add ip 1) stke ...) c)
        "call")

   (--> (ilist return ip s sp (n stke ...) c)
        (ilist (fetch-i ilist n) n s sp (stke ...) c)
        "return")

   (--> (ilist (commit l) ip s sp (stke_1 stke_2 ...)  c)
        (ilist (fetch-i ilist (add ip l)) (add ip l) s sp (stke_2 ...) c)
        "commit")

   (--> (ilist capture ip s sp stk (sp_1 ip_1))
        (ilist (fetch-next ilist ip) (add ip 1) s sp stk (sp ip))
        "capture")

   (--> (ilist fail ip s sp (n_1 stke ...) c)
        (ilist fail ip s sp  (stke ...) c)
        "fail-pop")

   (--> (ilist fail ip s sp ((n_1 n_2 (sp_1 ip_1)) stke ...) c)
        (ilist (fetch-i ilist n_2) n_2 s sp_1 (stke ...) (sp_1 ip_1))
        "fail-backtrack")

   ;; (--> (ilist fail ip s sp () c)
   ;;      (ilist failure ip s sp  () c)
   ;;      "failure")

   ;; (--> (ilist (any n) ip s sp stk c)
   ;;      (ilist (fetch-i ilist (add ip 1)) (add ip 1) s sp stk c)
   ;;      (side-condition (>= (- (length (term s)) (term sp)) (term n)))
   ;;      "any")

   ;; (--> (ilist (any n) ip s sp stk c)
   ;;      (ilist fail ip s sp stk c)
   ;;      (side-condition (not (>= (- (length (term s)) (term sp)) (term n))))
   ;;      "any-fail")
   ))

(define-metafunction LPEG
  fetch-i : ilist l -> i
  [(fetch-i ilist l) ,(list-ref (term ilist) (term l))])

(define-metafunction LPEG
  fetch-next : ilist n -> i
  [(fetch-next ilist n_1) ,(list-ref (term ilist) (add1 (term n_1)))])

(define-metafunction LPEG
  add : natural integer -> natural
  [(add n integer_1) ,(+ (term n) (term integer_1))])

(define-metafunction LPEG
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
				 (term ((commit -2))))
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

(define-judgment-form LPEG
  #:mode (types I I I O)
  [
   ------------------------"T-char"
   (types ilist pc (char ch) false)
   ]

  [
   (types ilist pc (fetch-i ilist (add pc l)) t)
   ----------------------------------------------- "T-jump"
   (types ilist pc (jump l) t)
   ]

  [
   (types ilist pc (fetch-i ilist (add pc 1)) t)
   ----------------------------------------------- "T-choice"
   (types ilist pc (choice l) t)
   ]



  ;; [
  ;;  (types ilist pc i_1 t_1)
  ;;  (types ilist (add pc 1) i_2 t_2)
  ;;  (where t_3 ,(if (and (term t_1) (term t_2)) (term true) (term false)))
  ;;  ---------------------------------------------------------------------- "T-cat"
  ;;  (types ilist pc (i_1 i_2) t_3)
  ;;  ]

  )


(define ilist (term (
                     (jump 1)
                     (jump 1)
                     (char 10)
                     )))

;; (term (fetch-i ,ilist 2))

(define i (term (fetch-i ,ilist 0)))

(judgment-holds (types ,ilist 0 ,i true))

;; (define e '(* (+ (! 2) 3)))
;; (define s '(4 2 2 2 3 4))

;; (define ilist (term (ecompile ,e)))
;; (define i (term (fetch-i ,ilist 0)))
;; (define state
;;   (term (
;; 	 ,ilist
;; 	 ,i
;; 	 0 ;; ip
;; 	 ,s
;; 	 0 ;; sp
;; 	 () ;; stk
;; 	 (0 0) ;; c
;; 	 )))

;; (traces ->e state)
