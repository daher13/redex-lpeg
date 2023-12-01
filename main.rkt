#lang racket

(require redex)
(require rackcheck)
(require peg-gen peg-gen/peg-gen-types)

(require "compiler/comp-peggen-peg.rkt")
(require "compiler/comp-peg-lpeg.rkt")
(require "type-system.rkt")
(require "view.rkt")
;; (require "well-typed-errors.rkt")
(require "ill-typed-errors.rkt")
;; (require "types.rkt")

(define peg (term (
                   (A B)
                   (B C)
                   (C A)
                   )))

;; (set! peg (term (peggen->peg ,ill5)))
(define lpeg (term (peg->lpeg ,peg))) ;; compilando peg em lpeg
(define ilist (car lpeg)) ;; obtém a lista de instruções
(define bilist (cadr lpeg)) ;; obtém a lista de blocos

(print-list ilist)

(define i (car ilist))

(judgment-holds (ts ,ilist 0 ,i #f b) b)
