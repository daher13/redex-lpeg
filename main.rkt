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


(define fetch-type-peggen
  (lambda (expr)
    ;; (print-list ilist)
    (let* ([peg (term (peggen->peg ,expr))]
           [lpeg (term (peg->lpeg ,peg))]
           [ilist (car lpeg)]
           [pos 0]
           [i (list-ref ilist pos)]
           [type (judgment-holds (ts ,ilist ,pos ,i #f b #f bl () pastl) (b bl))])
      (match type
        ['() (list peg ilist)]
        [_ 'well-formed]
      ))))

(define fetch-type-peg
  (lambda (peg)
    ;; (print-list ilist)
    (let* ([lpeg (term (peg->lpeg ,peg))]
           [ilist (car lpeg)]
           [pos 0]
           [i (list-ref ilist pos)]
           [type (judgment-holds (ts ,ilist ,pos ,i #f b #f bl () pastl) (b bl))])
      (match type
        ['() (list peg ilist)]
        [_ 'well-formed]
      ))))

(define peg (term (
                   (s0 (* (• I 0)))
                   (I (• L 0))
                   (L (* 0))
                   )))

(fetch-type-peg peg)

;; (define wflist (sample (gen:peg 3 3 2) 300))

;; (for/list ([e wflist])
  ;; (define peg (term (peggen->peg ,e)))
  ;; (fetch-type-peggen e))

;; (define illlist (sample (gen:ill-peg 3 3 2) 100))

;; (for/list ([e illlist])
  ;; (define peg (term (peggen->peg ,e)))
  ;; (fetch-type peg))
