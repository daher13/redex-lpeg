#lang racket

(require redex)
(require "./lang.rkt")

(define
 ->e
 (reduction-relation
  LPEG
  #:domain state
  (--> (ilist pl (char ch_1) ip (ch_1 ch_2 ...) stk (c ...))
       (ilist pl (fetch-next ilist ip) (add ip 1) (ch_2 ...) stk (c ... (ch_1 (ip (char ch_1)))))
       "char-match")

  (--> (ilist pl (char ch_1) ip (ch_2 ch_3 ...) stk clist)
       (ilist pl fail ip (ch_3 ...) stk clist)
       (side-condition (not (eq? (term ch_1) (term ch_2))))
       "char-not-match")

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

  (--> (ilist pl (commit l) ip s (stke_1 stke ...) clist)
       (ilist pl (fetch-i ilist (add ip l)) (add ip l) s (stke ...) clist)
       "commit")

  (--> (ilist pl capture ip s stk clist)
       (ilist pl (fetch-next ilist ip) (add ip 1) s stk clist)
       "capture")

  (--> (ilist pl fail ip s (n_1 stke ...) clist)
       (ilist pl fail ip s (stke ...) clist)
       "fail-pop")

  (--> (ilist pl fail ip s ((n_1 s_1 clist_1) stke ...) clist)
       (ilist pl (fetch-i ilist n_1) n_1 s_1 (stke ...) clist_1)
       "fail-backtrack")

  (--> (ilist pl (call l) ip s stk clist)
       (ilist pl fail ip s stk clist)
       (side-condition (eq? (term (check-pl pl l)) #t))
       "call-fail")

  (--> (ilist (l ...) (call l_1) ip s (stke ...) clist)
       (ilist (l ... l_1) (fetch-i ilist l_1) (add ip l_1) s ((add ip 1) stke ...) clist)
       (side-condition (eq? (term (check-pl (l ...) l_1)) #f))
       "call")))

(provide ->e)
