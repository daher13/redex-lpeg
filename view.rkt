#lang racket

(require redex)

(require "compiler/comp-peggen-peg.rkt")
(require "compiler/comp-peg-lpeg.rkt")
(require "type-system.rkt")

;; (define (fetch-b-types bilist ilist)
;;   (map (lambda (b)
;;          (match (fetch-b-type bilist (car b) ilist)
;;            ['() (cons (car b) '())]
;;            [(list (list _ _ t)) (cons (car b) t)]
;;            ))
;;        bilist))

(define (print-list ilist)
  (for ([pc (length ilist)] [i ilist])
    (printf "~a -> ~a\n" pc i)))

(define (print-eqlist eqlist)
  (for ([pc (length eqlist)] [eq eqlist])
    (printf "~a -> ~a\n" pc (cadr eq))))

;; (define (print-components pgpeg)
;;   (define peg (term (peggen->peg ,pgpeg)))
;;   (define lpeg (term (peg->lpeg ,peg)))
;;   (define ilist (car lpeg))
;;   (define bilist (cadr lpeg))
;;   bilist
;;   (fetch-b-types bilist ilist)
;;   (print-list ilist)
;; )


(provide (all-defined-out))
