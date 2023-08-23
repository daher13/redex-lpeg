#lang racket

(require redex)
(require peg-gen)
(require rackcheck)
(require "lang.rkt")
(require "compiler.rkt")
(require "type-system.rkt")

;; (define generated (sample (gen:peg 3 2 1) 10))
;; generated
;; (define ex (car (cddr generated)))
(define ex (term (∅ (• 1 2) ())))
ex
(define compiled (term (ecompile-peggen ,ex)))
(define blist (car compiled))
(define ilist (cadr compiled))
blist
ilist

(define i (car ilist))
(judgment-holds (ts ,ilist 0 ,i () () t) t)
