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
    [(cons t (list (list _ _ b))) (eq? (nullable? t) b)]
    [_ (error (string-append (~a pgt) (~a lpegt)))]
    ))

(define (fetch-b-type bilist bname ilist)
  (let* (
         [pc (cadr (assoc bname bilist))]
         [i (list-ref ilist pc)]
         [btype (judgment-holds (ts ,ilist ,pc ,i (() () #t) ot) ot)])
    btype
    ))


(define (fetch-b-types bilist ilist)
  (map (lambda (b)
         (match (fetch-b-type bilist (car b) ilist)
           ['() (cons (car b) '())]
           [(list (list _ _ t)) (cons (car b) t)]
           ))
       bilist))

(define (check-b-type bilist b ilist pgtypes)
           (let* (
                  ;; [pc (term (fetch-b-index ,bilist ,(car b)))]
                  ;; [i (list-ref lpeg pc)]
                  ;; [lpegt (judgment-holds (ts ,lpeg ,pc ,i (() () #t) ot) ot)]
                  [lpegt (fetch-b-type bilist (car b) ilist)]
                  [pgt (assoc (car b) pgtypes)])
             (if pgt
             (compare-types lpegt (cdr pgt))
             #t)))

(define (test-type peg)
  (if (or (null? (caddr peg)) (eq? (car peg) '∅))
      #t
      (let* (
             [pgtypes (caddr peg)]
             [compiled-peg (term (peggen->peg ,peg))]
             [lpeg (term (peg->lpeg ,compiled-peg))]
             [ilist (car lpeg)]
             [bilist (cadr lpeg)])
                (andmap (lambda (b)
                  (check-b-type bilist b ilist pgtypes))
                bilist))))

(define-property types-match ([peg (gen:peg 3 3 2)]) (test-type peg))

;; (check-property (make-config #:tests 100) types-match)

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

(define peg (term (peggen->peg ,error1)))
(define lpeg (term (peg->lpeg ,peg)))
(define ilist (car lpeg))
(define bilist (cadr lpeg))
bilist
(fetch-b-types bilist ilist)
(print-list ilist)
