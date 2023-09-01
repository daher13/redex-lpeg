#lang racket

(require redex)
(require rackcheck)
(require peg-gen peg-gen/peg-gen-types)

(require "compiler/comp-peggen-peg.rkt")
(require "compiler/comp-peg-lpeg.rkt")
(require "type-system.rkt")
(require "view.rkt")

(define (compare-types lpegt pgt)
  (match (cons pgt lpegt)
    [(cons _ '()) #f]
    [(cons t (list (list _ b))) (eq? (nullable? t) b)]
    [_ (error (string-append (~a pgt) (~a lpegt)))]
    ))

(define (fetch-b-types bilist ilist)
  (map (lambda (b)
         (match (fetch-b-type bilist (car b) ilist)
           ['() (cons (car b) '())]
           [(list (list _ t)) (cons (car b) t)]
           ))
       bilist))

(define (fetch-b-type bilist bname ilist)
  (let* (
         [pc (cadr (assoc bname bilist))]
         [i (list-ref ilist pc)]
         [btype (judgment-holds (ts ,ilist ,pc ,i (() #t) ot) ot)])
    btype))

(define (check-b-type bilist b ilist pgtypes)
  (let* (
         [lpegt (fetch-b-type bilist (car b) ilist)]
         [pgt (assoc (car b) pgtypes)])
    (if pgt
        (compare-types lpegt (cdr pgt))
        #t)))

(define (test-type pgpeg)
  (displayln pgpeg)
  (if (or (null? (caddr pgpeg)) (eq? (car pgpeg) '∅))
      #t
      (let* (
             [pgtypes (caddr pgpeg)]
             [peg (term (peggen->peg ,pgpeg))]
             [lpeg (term (peg->lpeg ,peg))]
             [ilist (car lpeg)]
             [bilist (cadr lpeg)])
        (andmap (lambda (b)
                  (check-b-type bilist b ilist pgtypes))
                bilist))))

(define-property types-match ([peg (gen:peg 3 3 2)]) (test-type peg))

(define error1 (list '(I (• ϵ 0) ∅) '(• I ϵ) (list (cons 'I (TyPEG #f '()) )))) ;; solved

(define error2 (list '(C (• (• 0 ϵ) (! 0)) (N (! (• 0 C)) (D (• (! C) (• 0 ϵ)) ∅))) ;; solved
                     '(• (/ 0 C) (• N D))
                     (list (cons 'D (TyPEG #f '(C)))
                           (cons 'N (TyPEG #t '()))
                           (cons 'C (TyPEG #f '())))))

(define error3 (list '(A (/ R ϵ) (R (/ 2 1) (D (• R A) ∅))) ;; solved
                     '(* D)
                     (list (cons 'D (TyPEG #f '(R)))
                           (cons 'R (TyPEG #f '()))
                           (cons 'A (TyPEG #t '(R))))))

(define error4 (list '(B (/ (/ V ϵ) (* 0)) (V (• (* 0) (• ϵ X)) (X (* (• 0 V)) ∅))) ;; loop?
                     '(/ (/ 0 1) (/ B ϵ))
                     (list (cons 'X (TyPEG #t '()))
                           (cons 'V (TyPEG #t '(X)))
                           (cons 'B (TyPEG #t '(X V))))))

(define error5 (list '(S (/ (• T N) (/ 0 T)) (T (• (/ 0 0) (* S)) (N (/ (• T ϵ) (• 0 ϵ)) ∅)))
                     '(• (• T ϵ) (/ S ϵ))
                     (list (cons 'N (TyPEG #f '(T)))
                           (cons 'T (TyPEG #f '()))
                           (cons 'S (TyPEG #f '(T))))))

(define error6 (list '(B (/ (• ϵ 1) (/ A ϵ)) (D (• (• ϵ ϵ) (• B 1)) (A (• (• 0 B) (/ D D)) ∅)))
                     '(/ (• ϵ 0) (• B D))
                     (list (cons 'A (TyPEG #f '()))
                           (cons 'D (TyPEG #f '(A B)))
                           (cons 'B (TyPEG #t '(A))))))

(define error7 (list '(C (• (• ϵ S) (/ S ϵ)) (X (* (• ϵ C)) (S (• (• 0 X) (/ C C)) ∅)))
                     '(• (/ S 0) (• S ϵ))
                     (list (cons 'S (TyPEG #f '()))
                           (cons 'X (TyPEG #t '(S C)))
                           (cons 'C (TyPEG #f '(S))))))

(define error8 (list '(B (• (• ϵ A) (/ A 0)) (C (* (• ϵ B)) (A (• (• 0 C) (* B)) ∅)))
                     '(• (/ A A) (• B B))
                     (list (cons 'A (TyPEG #f '()))
                           (cons 'C (TyPEG #t '(A B)))
                           (cons 'B (TyPEG #f '(A))))))

(define error9 (list '(P (* (• U 3)) (U (• (! 0) (• 1 2)) (I (• (• ϵ 0) (• ϵ 0)) ∅)))
                     '(/ (/ P ϵ) (* I))
                     (list (cons 'I (TyPEG #f '()))
                           (cons 'U (TyPEG #f '()))
                           (cons 'P (TyPEG #t '(U))))))

(define error10 (list '(L (/ (• ϵ 1) (* 2)) (F (/ (/ 1 0) (/ 1 L)) (D (! (• L 1)) ∅)))
                      '(• (/ 1 1) (/ F ϵ))
                      (list (cons 'D (TyPEG #t '(L)))
                            (cons 'F (TyPEG #t '(L)))
                            (cons 'L (TyPEG #t '())))))

(define error11 (list '(U (• (• ϵ W) (• ϵ W)) (O (/ (/ U U) (* U)) (W (• (• 0 U) (/ O ϵ)) ∅)))
                      '(• (/ W U) (• W O))
                      (list (cons 'W (TyPEG #f '()))
                            (cons 'O (TyPEG #t '(W U)))
                            (cons 'U (TyPEG #f '(W))))))

(define error12 (list '(K (/ (* 2) (! A)) (A (/ (• 2 K) (• 1 1)) ∅))
                      '(• (• K ϵ) (• K 2))
                      (list (cons 'A (TyPEG #f '()))
                            (cons 'K (TyPEG #t '(A))))))

(define error13 (list '(Q (/ (/ I 0) (/ I U)) (U (• (• ϵ I) (• 0 0)) (I (• (• 0 ϵ) (/ U Q)) ∅)))
                      '(* (/ I Q))
                      (list (cons 'I (TyPEG #f '()))
                            (cons 'U (TyPEG #f '(I)))
                            (cons 'Q (TyPEG #f '(I U))))))

(define errorLoop (list '(S (* (* 2)) (B (* 2) ∅))
                        '(* S)
                        ;; 'S
                        '()))

(check-property (make-config #:tests 45000
                             #:deadline (+ (current-inexact-milliseconds) (* 1000 3600)))
                types-match)

;; (define peg (term (peggen->peg ,error3)))
;; (define lpeg (term (peg->lpeg ,peg)))
;; (define ilist (car lpeg))
;; (define bilist (cadr lpeg))
;; (print-list ilist)
;; (fetch-b-types bilist ilist)
