#lang racket

(require redex)
(require rackcheck)
(require peg-gen peg-gen/peg-gen-types)

(require "compiler/comp-peggen-peg.rkt")
(require "compiler/comp-peg-lpeg.rkt")
(require "type-system.rkt")
(require "view.rkt")
(require "well-typed-errors.rkt")
(require "ill-typed-errors.rkt")
(require "types.rkt")

(define peg (term (
                   (A B)
                   (B (• A 2))
                   )))

(set! peg (term (peggen->peg ,ill5)))
(define lpeg (term (peg->lpeg ,peg))) ;; compilando peg em lpeg
(define ilist (car lpeg)) ;; obtém a lista de instruções
(define bilist (cadr lpeg)) ;; obtém a lista de blocos

bilist
ilist

(define eqlist (term (find-eqlist ,ilist 0)))
(define eq (car eqlist))

(for ([pc (length eqlist)] [eq eqlist])
  (define judgment-result (judgment-holds (ts ,eqlist ,eq () pastl b) b))
  (printf "~a -> ~a\n" eq judgment-result))
