#lang racket

(require redex)
(require peg-gen
         peg-gen/peg-gen-syntax-factory
         )
(require rackcheck)

(require "compiler/comp-peggen-peg.rkt")
(require "compiler/comp-peg-lpeg.rkt")
(require "type-system.rkt")

;; (define generated (sample (gen:peg 3 3 3) 100))

;; example
(define generated (term (((K (! 2) (T K (S K ∅)))
                          (• (* (! 2)) 2)
                          ()))))
;; (define generated (term (((S (/ B 2) (B (• 2 (• S 3)) ∅)) S ()))))

(for ([ex (in-list generated)])
  (define peg (term (peggen->peg ,ex)))
  (define lpeg (term (peg->lpeg ,peg)))
  (define ppgpeg (car ex)) ;; peggen peg
  (define pgstart (cadr ex))
  (define pgtype (caddr ex))
  ;; (define t (term ()))
  (define t (judgment-holds (ts ,lpeg 0 ,(car lpeg) () () t) t))
  (printf "PG = ~a\nPGPEG = ~a\nPGStart = ~a\nPGType = ~a\nPEG = ~a\nLPEG = ~a\nType = ~a\n\n"
          ex ppgpeg pgstart pgtype peg lpeg t)
  )
