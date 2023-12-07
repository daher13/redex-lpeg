#lang racket

(require redex)
(require rackcheck)
(require peg-gen peg-gen/peg-gen-types)

(require "compiler/comp-peggen-peg.rkt")
(require "compiler/comp-peg-lpeg.rkt")
(require "type-system.rkt")
(require "view.rkt")
;; (require "well-typed-errors.rkt")
;; (require "ill-typed-errors.rkt")
;; (require "types.rkt")


(define fetch-type
  (lambda (peg)
    ;; (print-list ilist)
    (let* ([lpeg (term (peg->lpeg ,peg))]
           [ilist (car lpeg)]
           [pos 0]
           [i (list-ref ilist pos)]
           [type (judgment-holds (ts ,ilist ,pos ,i #f b #f bl () pastl) (b bl))])
      (match type
        ['() (print-list ilist)]
        [_ 'well-formed]
      ))))


(define peg (term (
                  (s0 (/ S S))
                  (S (! 1))
                  (U S)
                  )))

;; (set! peg (term (peggen->peg ,(list-ref wflist 5))))
(fetch-type peg)

;; (define wflist (sample (gen:peg 3 3 2) 100))

;; (for/list ([e wflist])
  ;; (define peg (term (peggen->peg ,e)))
  ;; (fetch-type peg))

;; (define illlist (sample (gen:ill-peg 3 3 2) 100))

;; (for/list ([e illlist])
  ;; (define peg (term (peggen->peg ,e)))
  ;; (fetch-type peg))
