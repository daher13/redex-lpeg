#lang racket

(require redex)
(require rackcheck)
(require peg-gen)

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

;; (define generated (sample (gen:peg 3 3 3) 100))

(define generated (term (((E (• (/ (• ϵ 1) (* G)) (! (• ϵ 0))) (L (/ (• (• E 2) (/ 2 1)) (• (• E 2) (• ϵ E))) (G (• (• (/ 2 2) (• 1 E)) (/ (/ 2 L) (! 0))) ∅))) (• (• (/ L 0) (/ 2 1)) (/ (/ L 2) (• ϵ 2))) ((G . #(struct:TyPEG #f ())) (L . #(struct:TyPEG #f (G E))) (E . #(struct:TyPEG #t (G))))))))

;; example
;; (set! generated (term (((K ϵ ∅)
;;                           ()))))
;; (define generated (term (((S (/ B 2) (B (• 2 (• S 3)) ∅)) S ()))))

(define pgpeg (term ())) ;; peggen peg
(define pgstart (term ())) ;; peggen start
(define pgtype (term ())) ;; peggen type
(define peg (term ())) ;; extracted peg
(define lpeg (term ())) ;; compiled lpeg
(define lpegt (term ())) ;; lpeg type

(for ([ex (in-list generated)])
  (set! pgpeg (car ex)) ;; peggen peg
  (set! pgstart (cadr ex))
  (set! pgtype (caddr ex))
  (set! peg (term (peggen->peg ,ex)))
  (define compiled (term (peg->lpeg ,peg)))
  (set! lpeg (car compiled))
  (print-list lpeg)
  ;; (define t (term ()))
  (printf "PG = ~a\nPGPEG = ~a\nPGStart = ~a\nPGType = ~a\nPEG = ~a\nLPEG = ~a\n"
          ex pgpeg pgstart pgtype peg lpeg)
  (set! lpegt (judgment-holds (ts ,lpeg 0 ,(car lpeg) (() () #t) ot) ot))
  (printf "Type = ~a\n\n"
          lpegt)
  )
