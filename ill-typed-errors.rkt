#lang racket

(require peg-gen peg-gen/peg-gen-types)

(define ill1 (list '(N (/ (* 1) (* ϵ)) ∅)
                        '(/ (• N ϵ) (• ϵ ϵ))
                        (list (cons 'N 'ill-typed))))

(define ill2 (list '(M (• ϵ N) (N N (A (• 1 (* ϵ)) ∅)))
                        '(• N 0)
                        (list (cons 'A 'ill-typed)
                              (cons 'N 'ill-typed)
                              (cons 'M (TyPEG #t '(N))))))

(define ill3 (list '(E ϵ (W (• 0 (* ϵ)) (D (• 1 (* ϵ)) ∅)))
                        'D
                        (list (cons 'D 'ill-typed)
                              (cons 'W 'ill-typed)
                              (cons 'E (TyPEG #t '())))))

(define ill4 (list '(V (• (/ ϵ ϵ) (/ 1 1)) (J (/ (/ 0 V) (/ 0 V)) ∅))
                        '(/ (• ϵ 1) (• V V))
                        (list (cons 'J (TyPEG #f '(V)))
                              (cons 'V (TyPEG #f '())))))

(define ill5 (list '(I (• 1 0) (X (• 0 (* ϵ)) (S (* I) ∅)))
                        '(• I X)
                        (list (cons 'S (TyPEG #t '(I)))
                              (cons 'X 'ill-typed)
                              (cons 'I (TyPEG #f '())))))

(define ill6 (list '(A (! (• 0 (* ϵ))) ∅)
                        '(• (/ 0 A) (/ 0 0))
                        (list (cons 'A 'ill-typed))))

(provide (all-defined-out))
