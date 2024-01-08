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
  ;; (printf "~a\n" e) ;; print expression
  (let* ([peg (term (peggen->peg ,e))]
         [type (fetch-peg-type peg)])
    (match type
      [(list 'well-typed _ _ _) 'well-typed]
      [(list 'ill-typed _ ilist _) 'ill-typed]
      )))

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

(define (property-test vars lits depth size)
  (define-property test-well ([pgpeg (gen:peg vars lits depth)]) (equal? 'well-typed (test-type pgpeg)))
  (check-property (make-config #:tests size
                               #:deadline (+ (current-inexact-milliseconds) (* 1000 3600)))
                  test-well)
  (define-property test-ill ([pgpeg (gen:ill-peg vars lits depth)]) (check-ill-typed pgpeg))
  (check-property (make-config #:tests size
                               #:deadline (+ (current-inexact-milliseconds) (* 1000 3600)))
                  test-ill)
  )

;; (property-test 3 3 2 10000)
(property-test 4 4 3 5000)
;; (property-test 5 5 4 1000)

;; (let* ([peg (term (
;;                    ;; (s0 (• (* 2) (* 3)))

;;                    ;; (C X)
;;                    ;; (X (• A J))
;;                    ;; (A 0)
;;                    ;; (J C)

;;                    ;; (s0 (/ 0 K))
;;                    ;; (K P)
;;                    ;; (P (• K 0))
;;                    ;; (I (* ϵ))

;;                    (L (* C))
;;                    (C (* (• (• 1 0) (/ 3 3))))
;;                    ;; (s0 (• (! (• 1 3)) (/ (/ 2 0) (• L ϵ))))
;;                    ;; (E (* (/ (• 4 C) (• 1 L))))
;;                    ;; (M (/ (• (* 1) (/ 1 4)) (/ (/ 0 E) (* 0))))

;;                    ))]
;;        [lpeg (cddar (term (peg->lpeg ,peg)))]
;;        [type (fetch-peg-type peg)]
;;        )
;;   (begin
;;     (printf "~a\n" (car type))
;;     (print-list lpeg)))
