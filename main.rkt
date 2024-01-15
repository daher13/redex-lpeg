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
        [_ (list 'well-typed peg type ilist)]
        ))))

(define (test-type e)
  (printf "~a\n" e) ;; print expression
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
                (count (lambda (state) ;; has ill states (tests each ill state)
                         (let* ([pgpeg2 (list (car pgpeg)
                                              state
                                              (list-ref pgpeg 2))])
                           (match (test-type pgpeg2)
                             ['ill-typed #f]
                             [_ #t]
                             )))
                       ill-states)
                (match (test-type pgpeg) ;; hasn't ill states (tests entire expr)
                  ['ill-typed 0]
                  [_ 1]
                  )))))

(define (property-test vars lits depth size category)
  (define-property test-well ([pgpeg (gen:peg vars lits depth)]) (equal? 'well-typed (test-type pgpeg)))
  (define-property test-ill ([pgpeg (gen:ill-peg vars lits depth)]) (check-ill-typed pgpeg))

  (match category
    ['well (check-property (make-config #:tests size
                                        #:deadline (+ (current-inexact-milliseconds) (* 1000 3600)))
                           test-well)]
    ['ill (check-property (make-config #:tests size
                                       #:deadline (+ (current-inexact-milliseconds) (* 1000 3600)))
                          test-ill)]))

;; (property-test 3 3 2 5000 'well)
;; (property-test 4 4 3 5000 'well)
;; (property-test 4 4 3 5000 'ill)
;; (property-test 4 4 3 5000)
;; (property-test 5 5 4 100)
;; (property-test 5 5 5 10000 'ill)

(let* ([peg (term (
                   ;; (s0 (• (* 2) (* 3)))

                   ;; (C X)
                   ;; (X (• A J))
                   ;; (A 0)
                   ;; (J C)

                   ;; (s0 (/ 0 K))
                   ;; (K P)
                   ;; (P (• K 0))
                   ;; (I (* ϵ))

                   ;; (L (* C))
                   ;; (C (* (• (• 1 0) (/ 3 3))))

                   ;; (s0 (• (! (• 1 3)) (/ (/ 2 0) (• L ϵ))))
                   ;; (E (* (/ (• 4 C) (• 1 L))))
                   ;; (M (/ (• (* 1) (/ 1 4)) (/ (/ 0 E) (* 0))))

                   ;; (P (! (• 1 (* P))))

                   ;;  (H (* A))
                   ;;  (A (• D 0))
                   ;;  (D (! (• 0 H))) ;; não9 está deixando o 0 consumir

                   ;; (P (! 0))

                   ;; (A (• 1 (! I)))
                   ;; (I (• A 1))

                   ;; (s0 (* X))
                   ;; (X (! 0))
                   ;; (P (* (/ 0 1)))
                   ;; (B (! (• 0 1)))

                   ;; (s0 (• (• C ϵ) (• C 0)))
                   ;; (X (! (• 0 N)))
                   ;; (N (• (/ C ϵ) (/ 0 C)))
                   ;; (C (! (• 0 N)))

                   ;; (P A)
                   ;; (A B)
                   ;; (B (• 1 C))
                   ;; (C P)

                   ;; (P (• (! (• 1 P)) 2))

                   ;; (P (• 1 A))
                   ;; (A B)
                   ;; (B (• (! 2) P))

                   ;; (P (• 1 (! (* P)))) ;; (P (#t #f)) --> must return true
                   ;; (P (• 1 (* (! P)))) ;; (P (#t #f)) --> must return false

                   ;; (P (* (• 1 (* P))))



                   (s0
                    (•
                     (/ (* (• (/ ϵ ϵ) (• ϵ 0))) (/ (* (/ B E)) (• (/ ϵ ϵ) (/ ϵ ϵ))))
                     (/ (! (/ (• 0 (* ϵ)) (• 0 (* ϵ)))) (* (/ (• 0 ϵ) (/ L V))))))
                   (B
                    (/
                     (• (/ (/ (• E 0) (/ E H)) (• (/ ϵ ϵ) (• ϵ ϵ))) (/ (/ (• ϵ V) (/ 0 0)) (• (• ϵ ϵ) (• E ϵ))))
                     (• (• (! (• H 0)) (* (• H H))) (• (/ (• ϵ ϵ) (! E)) (• (• H ϵ) (! 0))))))
                   (E
                    (•
                     (• (/ (* (• ϵ V)) (* (• ϵ V))) (• (! (• 0 0)) (/ (• ϵ V) (• V ϵ))))
                     (• (/ (• (• ϵ V) (* V)) (• (• ϵ ϵ) (• V V))) (/ (* (/ V 0)) (! (• ϵ 0))))))
                   (V
                    (•
                     (/ (• (! (/ 0 0)) (/ (• ϵ ϵ) (* 0))) (/ (/ (/ ϵ ϵ) (/ ϵ ϵ)) (/ (• 0 0) (! 0))))
                     (• (• (/ (! 0) (/ 0 ϵ)) (• (/ 0 ϵ) (• ϵ 0))) (/ (• (• ϵ 0) (! 0)) (• (* 0) (• 0 ϵ))))))
                   (H
                    (/
                     (• (• (• (/ V ϵ) (* E)) (• (/ ϵ ϵ) (• ϵ E))) (! (• (/ 0 V) (/ 0 ϵ))))
                     (• (/ (• (• V ϵ) (/ E E)) (! (/ V E))) (/ (/ (• ϵ V) (/ 0 V)) (• (• 0 ϵ) (• ϵ V))))))
                   (L
                    (/
                     (• (• (/ (• 0 H) (• H ϵ)) (! (• V ϵ))) (• (/ (/ H B) (/ ϵ ϵ)) (/ (/ B E) (/ V 0))))
                     (• (! (/ (• ϵ 0) (• ϵ V))) (• (/ (* B) (/ ϵ ϵ)) (• (• ϵ E) (/ B V))))))
                   ))]
       [lpeg (cddar (term (peg->lpeg ,peg)))]
       [type (fetch-peg-type peg)]
       )

  (begin
    (printf "~a\n" (car type))
    (print-list lpeg)))
