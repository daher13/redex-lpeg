#lang racket

(require redex)
(require rackcheck)
(require peg-gen peg-gen/peg-gen-types)

(require "compiler/comp-peggen-peg.rkt")
(require "compiler/comp-peg-lpeg.rkt")
(require "compiler/comp-lpeg-z3.rkt")
(require "type-system.rkt")
(require "view.rkt")
(require "well-typed-errors.rkt")
(require "ill-typed-errors.rkt")
(require "types.rkt")

(define peg (term (
                   (A (* (* 3)))
                   )))

;; (set! peg (term (peggen->peg ,ill5)))
(define lpeg (term (peg->lpeg ,peg))) ;; compilando peg em lpeg
(define ilist (car lpeg)) ;; obtém a lista de instruções
(define bilist (cadr lpeg)) ;; obtém a lista de blocos

(print-list ilist)


(define eqlist (term (ilist->eqlist ,ilist 0)))

(define eq (car eqlist))
(define asserts (term (eqlist->asserts ,eqlist)))

(define consts (term (fetch-consts ,(length eqlist))))

(for ([pc (length consts)] [decl consts])
  (printf "~a\n" decl))

(for ([pc (length asserts)] [attr asserts])
  (printf "~a\n" attr))

(printf "(check-sat)")



;; (redex-match Z3 attr (term (assert (= 2 (or 2 2)))))
;; (print-list ilist)

;; (display "\n")

;; (print-eqlist eqlist)

;; (display "\n")

;; (for ([pc (length eqlist)] [eq eqlist])
  ;; (define judgment-result (judgment-holds (ts ,eqlist ,eq () pastl b) b))
  ;; (printf "~a -> ~a\n" eq judgment-result))
;; (term ,(format "t~a" 1))
