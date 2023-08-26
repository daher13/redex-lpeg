#lang racket

(require redex)
(require peg-gen)
(require rackcheck)
(require racket/format)
(require "aux.rkt")
(require "lang.rkt")
(require "compiler.rkt")
(require "type-system.rkt")

;; (define ex (term ((S B (B ε ∅)) S ()))) ;; recursive loop
;; (define ex (term (   (S (* A)
                        ;; (A C
                        ;; (C (/ (! D) D)
                           ;; (D (! 4)
                        ;; ∅))))
                     ;; S ())))
;; (define ex (term (∅ (/ ε 2) ())))
;; (define ex (term ((B (• ε 1) (P (/ 2 B) ∅)) (/ P 2) ((P #f (B)) (B #f ())))))
;; (define ex (term ((G (/ 0 ε) (W (• L 0) (L (/ G G) ∅))) (/ W W) ((L #t (G)) (W #f (L G)) (G #t ())))))
;; (define ex (term ((G (• ε 1) ∅) G ())))
;; (define ex (term ((S (! ε) (A (/ 0 P) (P (• 0 S) ∅))) (• ε ε) ((P #f ()) (A #f (P)) (S #t ())))))
;; (define ex (term ((G (• 1 ε) (O (/ 1 G) (I (• ε O) ∅))) (• ε O) ((I #f (G O)) (O #f (G)) (G #f ())))))
;; (define ex (term ((C (• H 1) (S (/ 0 C) (H (• ε ε) ∅))) (/ C H) ((H #t ()) (S #f (H C)) (C #f (H))))))
;; (define ex (term ((R A (U ε (A 0 ∅))) U ((A #f ()) (U #t ()) (R #f (A))))))
;; (define ex (term ((V 0 (W 0 ∅)) 0 ((W #f ()) (V #f ())))))
;; (define ex (term ((J (/ F ε) (F (• 0 1) (V (• 1 1) ∅))) (/ V ε) ((V #f ()) (F #f ()) (J #t (F))))))
;; (define ex (term ((N (* 1) (W (• 1 2) (L (* 2) ∅))) (! L) ((L #t ()) (W #f ()) (N #t ())))))
;; ex

;; (define compiled (term (ecompile-peggen ,ex)))
;; (define blist (car compiled))
;; (define ilist (cadr compiled))
;; (define type-list (caddr ex))
;; (term (print-ilist ,ilist 0))

;; (judgment-holds (ts ,ilist 0 ,(term (fetch-i ,ilist ,0)) () () t) t)

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
   (where #t (check-list pastl_1 pastl))
   ]
  [(compare-type _ _ _) #f]
  )


(define generated (sample (gen:peg 3 2 1) 1000))

(define wrong-list '())
(define counter 0)
(define ignored 0)

(for ([ex (in-list generated)])

  (define compiled (term (ecompile-peggen ,ex)))
  (define blist (car compiled))
  (define ilist (cadr compiled))
  (define type-list (caddr ex))

  (for ([t (in-list type-list)])
    (define b (car t))
    (define bindex (term (find-block-index ,blist ,b)))
    (define i (term (fetch-i ,ilist ,bindex)))

    (define judge (judgment-holds (ts ,ilist ,bindex ,i () () t) t))

    (if (or
         (and (list? (term ,(cadr ex))) (index-of (term ,(cadr ex)) b))
         (eq? (term ,(caddr ex)) b))
        (begin
          (set! counter (add1 counter))
          (if (eq? judge '())
              (set! wrong-list (append (list ilist t ex "empty-judge") wrong-list))
              (begin
                (if (term (compare-type ,blist ,t ,(car judge)))
                    (displayln #t)
                    (set! wrong-list (append (list ex (term (print-ilist ,ilist 0)) t (car judge) b) wrong-list))
                    ))
              )
          )
        (set! ignored (add1 ignored))
        )))

wrong-list

(displayln (string-append
            "Wrong: " (~a (length wrong-list))
            "  Processed: " (~a counter)
            "  Ignored: " (~a ignored)))
