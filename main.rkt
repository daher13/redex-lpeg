#lang racket

(require redex)
(require peg-gen
         peg-gen/peg-gen-syntax-factory
         )
(require rackcheck)

(require "comp-peggen-peg.rkt")
(require "comp-peg-lpeg.rkt")

(define generated (sample (gen:peg 3 2 1) 10000))

;; (define generated (list (list '(K 2 (T ϵ (S K ∅))) 'ϵ '())))


(for ([ex (in-list generated)])
  (define peg (term (peggen->peg ,ex)))
  (define lpeg (term (peg->lpeg ,peg)))
  (printf "EX = ~a\nPEG = ~a\nLPEG = ~a\n\n" ex peg lpeg)
  )
