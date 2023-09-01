#lang racket

(require peg-gen peg-gen/peg-gen-types)

(define wf1 (list '(I (• ϵ 0) ∅) '(• I ϵ) (list (cons 'I (TyPEG #f '()) )))) ;; solved

(define wf2 (list '(C (• (• 0 ϵ) (! 0)) (N (! (• 0 C)) (D (• (! C) (• 0 ϵ)) ∅))) ;; solved
                     '(• (/ 0 C) (• N D))
                     (list (cons 'D (TyPEG #f '(C)))
                           (cons 'N (TyPEG #t '()))
                           (cons 'C (TyPEG #f '())))))

(define wf3 (list '(A (/ R ϵ) (R (/ 2 1) (D (• R A) ∅))) ;; solved
                     '(* D)
                     (list (cons 'D (TyPEG #f '(R)))
                           (cons 'R (TyPEG #f '()))
                           (cons 'A (TyPEG #t '(R))))))

(define wf4 (list '(B (/ (/ V ϵ) (* 0)) (V (• (* 0) (• ϵ X)) (X (* (• 0 V)) ∅))) ;; loop?
                     '(/ (/ 0 1) (/ B ϵ))
                     (list (cons 'X (TyPEG #t '()))
                           (cons 'V (TyPEG #t '(X)))
                           (cons 'B (TyPEG #t '(X V))))))

(define wf5 (list '(S (/ (• T N) (/ 0 T)) (T (• (/ 0 0) (* S)) (N (/ (• T ϵ) (• 0 ϵ)) ∅)))
                     '(• (• T ϵ) (/ S ϵ))
                     (list (cons 'N (TyPEG #f '(T)))
                           (cons 'T (TyPEG #f '()))
                           (cons 'S (TyPEG #f '(T))))))

(define wf6 (list '(B (/ (• ϵ 1) (/ A ϵ)) (D (• (• ϵ ϵ) (• B 1)) (A (• (• 0 B) (/ D D)) ∅)))
                     '(/ (• ϵ 0) (• B D))
                     (list (cons 'A (TyPEG #f '()))
                           (cons 'D (TyPEG #f '(A B)))
                           (cons 'B (TyPEG #t '(A))))))

(define wf7 (list '(C (• (• ϵ S) (/ S ϵ)) (X (* (• ϵ C)) (S (• (• 0 X) (/ C C)) ∅)))
                     '(• (/ S 0) (• S ϵ))
                     (list (cons 'S (TyPEG #f '()))
                           (cons 'X (TyPEG #t '(S C)))
                           (cons 'C (TyPEG #f '(S))))))

(define wf8 (list '(B (• (• ϵ A) (/ A 0)) (C (* (• ϵ B)) (A (• (• 0 C) (* B)) ∅)))
                     '(• (/ A A) (• B B))
                     (list (cons 'A (TyPEG #f '()))
                           (cons 'C (TyPEG #t '(A B)))
                           (cons 'B (TyPEG #f '(A))))))

(define wf9 (list '(P (* (• U 3)) (U (• (! 0) (• 1 2)) (I (• (• ϵ 0) (• ϵ 0)) ∅)))
                     '(/ (/ P ϵ) (* I))
                     (list (cons 'I (TyPEG #f '()))
                           (cons 'U (TyPEG #f '()))
                           (cons 'P (TyPEG #t '(U))))))

(define wf10 (list '(L (/ (• ϵ 1) (* 2)) (F (/ (/ 1 0) (/ 1 L)) (D (! (• L 1)) ∅)))
                      '(• (/ 1 1) (/ F ϵ))
                      (list (cons 'D (TyPEG #t '(L)))
                            (cons 'F (TyPEG #t '(L)))
                            (cons 'L (TyPEG #t '())))))

(define wf11 (list '(U (• (• ϵ W) (• ϵ W)) (O (/ (/ U U) (* U)) (W (• (• 0 U) (/ O ϵ)) ∅)))
                      '(• (/ W U) (• W O))
                      (list (cons 'W (TyPEG #f '()))
                            (cons 'O (TyPEG #t '(W U)))
                            (cons 'U (TyPEG #f '(W))))))

(define wf12 (list '(K (/ (* 2) (! A)) (A (/ (• 2 K) (• 1 1)) ∅))
                      '(• (• K ϵ) (• K 2))
                      (list (cons 'A (TyPEG #f '()))
                            (cons 'K (TyPEG #t '(A))))))

(define wf13 (list '(Q (/ (/ I 0) (/ I U)) (U (• (• ϵ I) (• 0 0)) (I (• (• 0 ϵ) (/ U Q)) ∅)))
                      '(* (/ I Q))
                      (list (cons 'I (TyPEG #f '()))
                            (cons 'U (TyPEG #f '(I)))
                            (cons 'Q (TyPEG #f '(I U))))))

(define wf14 (list '(A (• (/ ϵ ϵ) (• ϵ 0)) ∅)
                      '(• (• A A) (• ϵ A))
                      (list (cons 'A (TyPEG #f '())))))

(provide (all-defined-out))
