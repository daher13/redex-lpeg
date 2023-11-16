#lang racket

(require redex)
(require rackcheck)
(require peg-gen peg-gen/peg-gen-types)

(require "compiler/comp-peggen-peg.rkt")
(require "compiler/comp-peg-lpeg.rkt")
(require "type-system.rkt")
(require "view.rkt")
(require "well-typed-errors.rkt")
(require "ill-typed-errors.rkt")

;; (check-property (make-config #:tests 45000
                             ;; #:deadline (+ (current-inexact-milliseconds) (* 1000 3600)))
                ;; types-match)

;; (check-property (make-config #:tests 100
;;                              #:deadline (+ (current-inexact-milliseconds) (* 1000 3600)))
;;                 ill-types-match)


;; (test-type illerror1)

;; (define peg (list (list 's0 '(• 2 (* 3)))))
;; (define peg (term (
;;                    (A (/ (* (! 2)) (* (* 2))))
;;                    )))
;; (define peg (term (
                   ;; (A B)
                   ;; (B A)
                   ;; )))

(define (compare-types pgt lpegt)
  ;; verify if two blocks matches
  (match (cons pgt lpegt)
    [(cons ill-typed '()) #t]
    [(cons t (list (list _ b))) (eq? (nullable? t) b)]
    [_ (error (string-append (~a pgt) (~a lpegt)))]
    ))

(define (fetch-b-type bilist bname ilist)
  ;; fetch the type of a specified block
  (let* (
         [pc (cadr (assoc bname bilist))]
         [i (list-ref ilist pc)]
         [btype (judgment-holds (ts ,ilist ,pc ,i pastl b) b)])
    btype))

(define (fetch-lpeg-types bilist ilist)
  ;; fetch a list of all lpeg block types
  (map (lambda (b)
         (match (fetch-b-type bilist (car b) ilist)
           ['() (cons (car b) '())]
           [(list (list _ t)) (cons (car b) t)]
           ))
       bilist))

(define (check-b-type bilist b ilist pgtypes)
  ;; check is a type of a lpeg block matches peggen block type
  (let* (
         [lpegt (fetch-b-type bilist (car b) ilist)]
         [pgt (assoc (car b) pgtypes)])
    (if pgt
        (compare-types (cdr pgt) lpegt)
        #t)))

(define (test-type pgpeg)
  ;; get a peggen peg, compile to lpeg and test types
  (displayln pgpeg)
      (let* (
             [pgtypes (caddr pgpeg)]
             [peg (term (peggen->peg ,pgpeg))]
             [lpeg (term (peg->lpeg ,peg))]
             [ilist (car lpeg)]
             [bilist (cadr lpeg)])
        (andmap (lambda (b)
                  (check-b-type bilist b ilist pgtypes))
                bilist)))

(define-property types-match ([pgpeg (gen:peg 3 3 2)]) (test-type pgpeg))
(define-property ill-types-match ([pgpeg (gen:ill-peg 3 3 2)]) (test-type pgpeg))

(provide (all-defined-out))
