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
           [type (judgment-holds (ts ,ilist ,pos ,i () pastc () pastl) (pastc pastl))])
      (match type
        ['() (list 'ill-typed peg ilist type)]
        [_ (list 'well-typed peg ilist type)]
        ))))

(define (test-type e)
  (let* ([peg (term (peggen->peg ,e))]
         [type (fetch-type-peg peg)])
    (match type
      [(list 'well-typed _ _ _) 'well-typed]
      [(list 'ill-typed _ ilist _) 'ill-typed]
      )))

(define (test-ill-typed testLength maxVars maxLits maxDepth)
  (filter-map (lambda (e)
                (let* ([peg (term (peggen->peg ,e))]
                       [type (fetch-type-peg peg)])
                  (match type
                    [(list 'well-typed _ _ _) peg]
                    [(list 'ill-typed _ _ _) #f]
                    )))
              (sample (gen:ill-peg maxVars maxLits maxDepth) testLength)))

;; well typed

(define-property test-well ([pgpeg (gen:peg 3 3 2)]) (equal? 'well-typed (test-type pgpeg)))
(check-property (make-config #:tests 10000
                             #:deadline (+ (current-inexact-milliseconds) (* 1000 3600)))
                test-well)

(define-property test-ill ([pgpeg (gen:ill-peg 0 5 3)]) (equal? 'ill-typed (test-type pgpeg)))
(check-property (make-config #:tests 10000
                             #:deadline (+ (current-inexact-milliseconds) (* 1000 3600)))
                test-ill)
