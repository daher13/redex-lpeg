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
           [type (judgment-holds (ts ,ilist ,pos ,i () cstk (#f ()) lstk) (cstk lstk))])
      (match type
        ;; ['() 'ill-typed]
        ['() (list 'ill-typed peg ilist type)]
        [_ (list 'well-typed peg ilist type)]
        ;; [_ (list 'well-typed peg ilist type)]
        ))))

(define fetch-type-peg
  (lambda (peg)
    (let* ([lpeg (term (peg->lpeg ,peg))]
           [ilist (car lpeg)]
           [pos 0]
           [i (list-ref ilist pos)]
           [type (judgment-holds (ts ,ilist ,pos ,i () cstk (#f ()) lstk) (cstk lstk))]
           )
      (match type
        ['() (list 'ill-typed ilist type)]
        [_ (list 'well-typed peg ilist type)]
      )
      ;; (print-list ilist)
      )))

;; (define peg (term (
;;                    (s0 (/ (• W G) (• 1 G))) (W (• (! 1) (• ϵ 0))) (G (/ (* W) (• ϵ ϵ)))
;;                    )))

;;   (fetch-type-peg peg)


;;  for commits (no calls)

;; (filter-map (lambda (e)
;;               (let* ([peg (term (peggen->peg ,e))]
;;                       [type (fetch-type-peggen e)])
;;                         (match type
;;                           [(list 'well-typed _ _ _) #f]
;;                           [(list 'ill-typed _ _ _) peg]
;;                           )))
;;             (sample (gen:peg 0 5 3) 10000))

;; (filter-map (lambda (e)
;;               (let* ([peg (term (peggen->peg ,e))]
;;                      [type (fetch-type-peggen e)])
;;                 (match type
;;                   [(list 'ill-typed _ _ _) #f]
;;                   [(list 'well-typed _ _ _) peg]
;;                   )))
;;             (sample (gen:ill-peg 0 5 3) 10000))

;; for calls

;; (filter-map (lambda (e)
;;               (let* ([peg (term (peggen->peg ,e))]
;;                       [type (fetch-type-peggen e)])
;;                         (match type
;;                           [(list 'well-typed _ _ _) #f]
;;                           [(list 'ill-typed _ _ _) peg]
;;                           )))
;;             (sample (gen:peg 3 3 2) 100))

(filter-map (lambda (e)
              (let* ([peg (term (peggen->peg ,e))]
                     [type (fetch-type-peggen e)])
                (match type
                  [(list 'ill-typed _ _ _) #f]
                  [(list 'well-typed _ _ _) peg]
                  )))
            (sample (gen:ill-peg 3 3 2) 10000))
