#lang racket

(require redex)
(require rackcheck)
(require peg-gen peg-gen/peg-gen-types)

(require "compiler/comp-peggen-peg.rkt")
(require "compiler/comp-peg-lpeg.rkt")
(require "type-system.rkt")
(require "view.rkt")

;; (define generated (term ((
;;                          (K (* 2) (L K (S L ∅)))
;;                          (* S)
;;                          ()
;;                          ))))
;; (define generated (term ((
                         ;; (K (/ L (* 2)) (L (• 2 (• K 3)) ∅))
                         ;; K
                         ;; ()
                         ;; ))))

;; (define generated (list (list
                    ;; '(E (• (/ (• ϵ 1) (* G)) (! (• ϵ 0))) (L (/ (• (• E 2) (/ 2 1)) (• (• E 2) (• ϵ E))) (G (• (• (/ 2 2) (• 1 E)) (/ (/ 2 L) (! 0))) ∅))) '(• (• (/ L 0) (/ 2 1)) (/ (/ L 2) (• ϵ 2))) (list (cons 'G (TyPEG #f '())) (cons 'L (TyPEG #f '(G E))) (cons 'E (TyPEG #t '(G)))))))

;; example
;; (set! generated (term (((K ϵ ∅)
;;                           ()))))
;; (define generated (term (((S (/ B 2) (B (• 2 (• S 3)) ∅)) S ()))))


(define (compare-types lpegt pgt)
  (match (cons pgt lpegt)
    [(cons _ '()) #f]
    [(cons t (list (list _ _ b))) (eq? (nullable? t) b)]
    [_ (error (string-append (~a pgt) (~a lpegt)))]
    ))

(define (check-b-type blist b lpeg pgtypes)
           (let* (
                  [pc (term (fetch-b-index ,blist ,(car b)))]
                  [i (list-ref lpeg pc)]
                  [lpegt (judgment-holds (ts ,lpeg ,pc ,i (() () #t) ot) ot)]
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
             [compiled-lpeg (term (peg->lpeg ,compiled-peg))]
             [lpeg (car compiled-lpeg)]
             [blist (cadr compiled-lpeg)])
                (andmap (lambda (b)
                  (check-b-type blist b lpeg pgtypes))
                blist))))

(define-property types-match ([peg (gen:peg 3 3 2)]) (test-type peg))

(check-property (make-config #:tests 10) types-match)

(define error1 (list '(I (• ϵ 0) ∅) '(• I ϵ) (list (cons 'I (TyPEG #f '()) ))))
(define error2 (list '(C (• (• 0 ϵ) (! 0)) (N (! (• 0 C)) (D (• (! C) (• 0 ϵ)) ∅)))
                '(• (/ 0 C) (• N D))
                (list (cons 'D (TyPEG #f '(C)))
                      (cons 'N (TyPEG #t '()))
                      (cons 'C (TyPEG #f '())))))
