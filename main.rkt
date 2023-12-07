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
;; (require "types.rkt")


(define fetch-type (lambda (peg)
                     (define lpeg (term (peg->lpeg ,peg))) ;; compilando peg em lpeg
                     (define ilist (car lpeg)) ;; obtém a lista de instruções
                     (print-list ilist)
                     (define pos 0)
                     (define i (list-ref ilist pos))
                     (judgment-holds (ts ,ilist ,pos ,i #f b #f bl () pastl) (b bl))))


;; (fetch-type peg)

(define peg (term (peggen->peg ,(list-ref wflist 4))))
(fetch-type peg)

;; (for/list ([e wflist])
  ;; (define peg (term (peggen->peg ,e)))
  ;; (fetch-type peg))

;; (for/list ([e illlist])
  ;; (define peg (term (peggen->peg ,e)))
  ;; (fetch-type peg))
