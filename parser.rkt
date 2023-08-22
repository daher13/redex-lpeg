#lang racket

(require redex)
(require "lang.rkt")
(require "aux.rkt")

(define-extended-language
  LPEGProc
  LPEG
  (n ::= natural)
  (ilist ::= (i ...))
  (ip ::= natural)
  (s ::= (ch ...))
  (c ::= (ch (ip i)))
  (clist ::= (c ...))
  (stke ::= n (n s clist))
  (stk ::= (stke ...))
  (pl :: (l ... )) ;; past-labels
  (state ::= (ilist pl i ip s stk clist)))

(define-metafunction
  LPEGProc
  fetch-next : ilist n -> i
  [(fetch-next ilist n_1) ,(list-ref (term ilist) (add1 (term n_1)))])

(define-metafunction
  LPEGProc
  check-pl : pl l -> boolean
  [(check-pl () l_1) #f]
  [(check-pl (l_1 l_2 ...) l_1) #t]
  [(check-pl (l_1 l_2 ...) l_3) (check-pl (l_2 ...) l_3)]
  )

(define
 ->e
 (reduction-relation
  LPEGProc
  #:domain state
  (--> (ilist pl (char ch_1) ip (ch_1 ch_2 ...) stk (c ...))
       (ilist pl (fetch-next ilist ip) (add ip 1) (ch_2 ...) stk (c ... (ch_1 (ip (char ch_1)))))
       "char-match")

  (--> (ilist pl (char ch_1) ip (ch_2 ch_3 ...) stk clist)
       (ilist pl fail ip (ch_3 ...) stk clist)
       (side-condition (not (eq? (term ch_1) (term ch_2))))
       "char-not-match")

  (--> (ilist pl emp ip (ch ...) stk clist)
       (ilist pl (fetch-next ilist ip) (add ip 1) (ch ...) stk clist)
       "emp")

  (--> (ilist pl (char ch_1) ip () stk clist)
       (ilist pl fail ip () stk clist)
       "char-empty")

  (--> (ilist pl (jump l_1) ip s stk clist)
       (ilist pl (fetch-i ilist (add ip l_1)) (add ip l_1) s stk clist)
       "jump")

  (--> (ilist pl (choice l_1) ip s (stke ...) clist)
       (ilist pl (fetch-next ilist ip) (add ip 1) s (((add ip l_1) s clist) stke ...) clist)
       "choice")

  (--> (ilist pl return ip s (n stke ...) clist)
       (ilist pl (fetch-i ilist n) n s (stke ...) clist)
       "return")

  (--> (ilist pl capture ip s stk clist)
       (ilist pl (fetch-next ilist ip) (add ip 1) s stk clist)
       "capture")

  (--> (ilist pl fail ip s (n_1 stke ...) clist)
       (ilist pl fail ip s (stke ...) clist)
       "fail-pop")

  (--> (ilist pl fail ip s ((n_1 s_1 clist_1) stke ...) clist)
       (ilist pl (fetch-i ilist n_1) n_1 s_1 (stke ...) clist_1)
       "fail-backtrack")

  (--> (ilist (l ...) (call l_1) ip s (stke ...) clist)
       (ilist (l ... l_1) (fetch-i ilist (add ip l_1)) (add ip l_1) s ((add ip 1) stke ...) clist)
       ;; (side-condition (not (term (check-pl (l ...) l_1))))
       "call")

  (--> (ilist pl (commit l) ip s (stke_1 stke_2 ...) clist)
       (ilist pl (fetch-i ilist (add ip l)) (add ip l) s (stke_2 ...) clist)
       "commit")
  ))

(provide ->e)
