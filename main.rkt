#lang racket

(require redex)
(require rackcheck)
(require peg-gen)

(require "compiler/comp-peggen-peg.rkt")
(require "compiler/comp-peg-lpeg.rkt")
(require "type-system.rkt")
(require "view.rkt")

(define generated (term ((
                         (K L (L K ∅))
                         K
                         ()
                         ))))

;; (set! generated (sample (gen:peg 3 3 3) 100))

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
  (set! lpeg (term (peg->lpeg ,peg)))
  ;; (define t (term ()))
  (printf "PG = ~a\nPGPEG = ~a\nPGStart = ~a\nPGType = ~a\nPEG = ~a\nLPEG = ~a\n"
          ex pgpeg pgstart pgtype peg lpeg)
  (set! lpegt (judgment-holds (ts ,lpeg 0 ,(car lpeg) (() () #t) ot) ot))
  (printf "PG = ~a\n\n"
          lpegt)
  )
;; ((() () #f) (() () #t) (() (2) #f) (() (2) #t))
