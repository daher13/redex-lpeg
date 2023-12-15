#lang racket

(require redex)
(require rackcheck)
(require peg-gen peg-gen/peg-gen-types)

(require "compiler/comp-peggen-peg.rkt")
(require "compiler/comp-peg-lpeg.rkt")
(require "type-system.rkt")
(require "view.rkt")

(define fetch-type-peg
  (lambda (peg)
    (let* ([lpeg (term (peg->lpeg ,peg))]
           [ilist (car lpeg)]
           [pos 0]
           [i (list-ref ilist pos)]
           [type (judgment-holds (ts ,ilist ,pos ,i () pastc) pastc)])
      (match type
        ['() (list 'ill-typed peg ilist type)]
        [_ (list 'well-typed peg ilist type)]
        ))))


(define (test-well-typed testLength maxVars maxLits maxDepth)
  (filter-map (lambda (e)
                (let* ([peg (term (peggen->peg ,e))]
                       [type (fetch-type-peg peg)])
                  (match type
                    [(list 'well-typed _ _ _) #f]
                    [(list 'ill-typed _ _ _) peg]
                    )))
              (sample (gen:peg maxVars maxLits maxDepth) testLength)))

(define (test-ill-typed testLength maxVars maxLits maxDepth)
  (filter-map (lambda (e)
                (let* ([peg (term (peggen->peg ,e))]
                       [type (fetch-type-peg peg)])
                  (match type
                    [(list 'well-typed _ _ _) peg]
                    [(list 'ill-typed _ _ _) #f]
                    )))
              (sample (gen:ill-peg maxVars maxLits maxDepth) testLength)))


;; for commits (no calls)
(test-well-typed 10000 0 5 3)
(test-ill-typed 10000 0 5 3)

;; for calls
;; (test-well-typed 1000 3 3 2)
;; (test-ill-typed 1000 3 3 2)

;; (define peg (term (
                   ;; (s0 (* (• 0 (! 1))))
                   ;; )))

;; (fetch-type-peg peg)
