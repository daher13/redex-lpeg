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
           [type (judgment-holds (ts ,ilist ,pos ,i () cstk) cstk)])
      (match type
        ['() 'ill-typed]
        ;; ['() (list 'ill-typed peg ilist type)]
        [_ 'well-typed]
        ;; [_ (list 'well-typed peg ilist type)]
        ))))

(define fetch-type-peg
  (lambda (peg)
    (let* ([lpeg (term (peg->lpeg ,peg))]
           [ilist (car lpeg)]
           [pos 0]
           [i (list-ref ilist pos)]
           ;; [type (judgment-holds (ts ,ilist ,pos ,i () cstk) cstk)]
           )
      ;; (match type
        ;; ['() (list 'ill-typed ilist type)]
        ;; [_ (list 'well-typed peg ilist type)]
      ;; )
      (print-list ilist)
      )))

(define peg (term (
                  (A B)
                  (B (• 1 A))
                   )))

(fetch-type-peg peg)

;; for commits (no calls)

;; (for/sum ([e (sample (gen:peg 0 5 3) 10000)])
  ;; (let* ([peg (term (peggen->peg ,e))]
         ;; [type (fetch-type-peggen e)])
    ;; (match type
      ;; ['well-typed 1]
      ;; ['ill-typed 0])))

;; (for/sum ([e (sample (gen:ill-peg 0 5 3) 10000)])
  ;; (let* ([peg (term (peggen->peg ,e))]
         ;; [type (fetch-type-peggen e)])
    ;; (match type
      ;; ['well-typed 0]
      ;; ['ill-typed 1])))

;; for calls

;; (for/sum ([e (sample (gen:peg 3 3 2) 10000)])
  ;; (let* ([peg (term (peggen->peg ,e))]
         ;; [type (fetch-type-peggen e)])
    ;; (match type
      ;; ['well-typed 1]
      ;; ['ill-typed 0])))

;; (for/sum ([e (sample (gen:ill-peg 3 3 2) 10000)])
  ;; (let* ([peg (term (peggen->peg ,e))]
         ;; [type (fetch-type-peggen e)])
    ;; (match type
      ;; ['well-typed 0]
      ;; ['ill-typed 1])))
