#lang racket

(require redex)


(define-language
 PEG
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

(define-extended-language
 LPEG PEG
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
 (c ::= (ch (ip i))) ;; capture
 (clist ::= (c ...)) ;; capture_list
 (stke ::= n (n s clist)) ;; stack entry ;; (backtrack_option subject capture_list)
 (stk ::= (stke ...))
 (pc ::= natural)
 (t ::= boolean)
 (pl :: (l ... )) ;; past-labels
 (state ::= (ilist pl i ip s stk clist)))

(define-metafunction
  LPEG
  fetch-i : ilist l -> i
  [(fetch-i ilist l) ,(list-ref (term ilist) (term l))])

(define-metafunction
  LPEG
  fetch-next : ilist n -> i
  [(fetch-next ilist n_1) ,(list-ref (term ilist) (add1 (term n_1)))])

(define-metafunction
  LPEG
  add : natural integer -> natural
  [(add n integer_1) ,(+ (term n) (term integer_1))])


(define-metafunction LPEG
  check-pl : pl l -> boolean
  [(check-pl (l_1 l_2 ...) l_1) #t]
  [(check-pl (l_1 l_2 ...) l_3) (check-pl (l_2 ...) l_3)]
  [(check-pl () l_1) #f])

(provide LPEG fetch-i fetch-next add check-pl)
