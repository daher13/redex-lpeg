#lang racket

(require redex)
(require peg-gen)
(require rackcheck)
(require "aux.rkt")
(require "lang.rkt")
(require "compiler.rkt")
(require "type-system.rkt")

;; (define generated (sample (gen:peg 3 2 1) 10))
;; generated
;; (define ex (car (cddr generated)))
;; ex

;; (define ex (term (∅ (/ ε 2) ())))
;; (define ex (term ((B (• ε 1) (P (/ 2 B) ∅)) (/ P 2) ((P #f (B)) (B #f ())))))
;; (define ex (term ((G (/ 0 ε) (W (• L 0) (L (/ G G) ∅))) (/ W W) ((L #t (G)) (W #f (L G)) (G #t ())))))
;; (define ex (term ((G (• ε 1) ∅) G ())))
(define ex (term ((C (/ 1 1) (R (! U) (U (* C) ∅))) (/ ε C) ((U #t (C)) (R #t (U C)) (C #f ())))))
;; ex

(define compiled (term (ecompile-peggen ,ex)))
(define blist (car compiled))
(define ilist (cadr compiled))
(define type-list (caddr ex))
;; blist
;; ilist
;; type-list

type-list

(define-metafunction
  TypeSystem
  replace-index : blist xlist -> (l ...)
  [(replace-index blist ()) ()]
  [(replace-index blist (x x_1 ...)) ,(append
                                 (term ((find-block-index blist x)))
                                 (term (replace-index blist (x_1 ...)))
                                 )])

(define-metafunction
  TypeSystem
  check-list : pastl pastl -> boolean
  [(check-list (l_1 l_2 ...) (l_3 ... l_1 l_4 ...))
   (check-list (l_2 ...) (l_3 ... l_1 l_4 ...))]
  [(check-list () (l ...)) #t]
  [(check-list _ _) #f]
  )

(define-metafunction
  TypeSystem
  compare-type : blist ptype t -> any ;; blist peggen_type my_type
  [(compare-type blist (x_1 boolean_1 xlist) (pastl_1 _ boolean))
   #t ;; labels
   (where pastl (replace-index blist xlist))
   (where boolean boolean_1)
   (where #t (check-list pastl pastl_1))
   ]
  [(compare-type _ _ _) #f]
  )

;; (define bindex (term (find-block-index ,blist P)))


blist
ilist
(for ([t (in-list type-list)])
;; (for/list ([t type-list])
  ;; (define b (car t))
  (define b 'R)
  (define bindex (term (find-block-index ,blist ,b)))
  (define i (term (fetch-i ,ilist ,bindex)))

   (display (judgment-holds (ts ,ilist ,bindex ,i () () t) t))

  ;; (define iseq (term (compare-type ,blist ,t ,judge)))
  ;; (define result (if iseq #t
      ;; (list b t judge)))
  ;; (displayln result)
  )
