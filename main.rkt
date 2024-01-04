#lang racket

(require redex)
(require rackcheck)
(require peg-gen peg-gen/peg-gen-types)

(require "compiler/comp-peggen-peg.rkt")
(require "compiler/comp-peg-lpeg.rkt")
(require "type-system.rkt")
(require "view.rkt")

(define fetch-peg-type
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
         [type (fetch-peg-type peg)])
    (match type
      [(list 'well-typed _ _ _) 'well-typed]
      [(list 'ill-typed _ ilist _) 'ill-typed]
      )))

;; (define (test-ill-typed testLength maxVars maxLits maxDepth)
;;   (filter-map (lambda (e)
;;                 (let* ([peg (term (peggen->peg ,e))]
;;                        [type (fetch-peg-type peg)])
;;                   (match type
;;                     [(list 'well-typed _ _ _) peg]
;;                     [(list 'ill-typed _ _ _) #f]
;;                     )))
;;               (sample (gen:ill-peg maxVars maxLits maxDepth) testLength)))

(define (fetch-ill-states pgpeg)
  (let* ([states (caddr pgpeg)])
    (filter-map (lambda (state)
                  (match state
                    [(cons st 'ill-typed) st]
                    [_ #f]
                    ))
                states)))

(define (check-ill-typed pgpeg)
  (equal? 0
          (let* ([ill-states (fetch-ill-states pgpeg)])
            (if (> (length ill-states) 0)
                (count (lambda (state)
                         (let* ([pgpeg2 (list (car pgpeg)
                                              state
                                              (list-ref pgpeg 2))])
                           (match (test-type pgpeg2)
                             ['ill-typed #f]
                             [_ #t]
                             )))
                       ill-states)
                (match (test-type pgpeg)
                  ['ill-typed 0]
                  [_ 1]
                  )))))


;; (define-property test-well ([pgpeg (gen:peg 3 3 2)]) (equal? 'well-typed (test-type pgpeg)))
;; (check-property (make-config #:tests 5000
                             ;; #:deadline (+ (current-inexact-milliseconds) (* 1000 3600)))
                ;; test-well)

;; (define-property test-ill ([pgpeg (gen:ill-peg 0 5 5)]) (check-ill-typed pgpeg))
;; (check-property (make-config #:tests 50000
;;                              #:deadline (+ (current-inexact-milliseconds) (* 1000 3600)))
;;                 test-ill)

(let* ([peg (term (
                   ;;                    ;; (s0 (• (* 2) (* 3)))

                   ;; (s0 C)
                   ;; (C X)
                   ;; (X (• A J))
                   ;; (A 0)
                   ;; (J C)

                   (s0 (• B B))
                   (B (* 1))
                   (X ϵ)
                   (W (• B 0))
                   ))]
       [lpeg (cddar (term (peg->lpeg ,peg)))]
       [type (fetch-peg-type peg)]
       )
  (begin
    (printf "~a\n" (car type))
    (print-list lpeg)))
