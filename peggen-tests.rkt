#lang racket

(require redex)
(require peg-gen)
(require rackcheck)
(require "lang.rkt")
(require "compiler.rkt")
(require "type-system.rkt")

;; (sample (gen:peg 3 2 1) 1)

;; (define generated '((∅ (• (/ ε 5) (* 5)) ())))

(define output '(;;(∅ ε ())
                    ;;(∅ ε ())
                    (
                     (I (/ ε 0) (G (/ 0 M) (M (• I 0) ∅)))
                     (/ M I)
                     ((M #f (I))
                      (G #f (I M))
                      (I #t ()))
                     )
                    ))

(define generated (car output))

(define expr (car generated))
(define start (car (cdr generated)))
(define g (append (term (S ,start ,expr))))
(define ilist (term (ecompile ,g)))
;; ilist
(define i (car ilist))

ilist
i


(judgment-holds (ts ,ilist 0 ,i () () t) t)
