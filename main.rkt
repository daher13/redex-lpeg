#lang racket

(require redex)

(require "./parser.rkt")
(require "./compiler.rkt")
(require "./type-system.rkt")

;; (define e '(* (+ 3 (! 2)))) ;; generates loop on process
;; (define e '(! (+ (+ 2 3) (+ 3 4))))
;; (define s '(7 5))
;; compile
;; (define ilist (term (ecompile ,e)))
;; ilist
;; type system
;; (define ilist (term ((char 1) (char 2) end)))
;; (define i (term (fetch-i ,ilist 0)))
;; (judgment-holds (type-i ,ilist 0 ,i t) t)
;; parser
;; (define state
  ;; (term (
         ;; ,ilist
         ;; () ;; pl
         ;; ,i
         ;; 0 ;; ip
         ;; ,s
         ;; () ;; stk
         ;; () ;; clist
         ;; )))
;; (traceS ->e state)

(define g '((S D) (D 3)))
;; (define g '(! 4))
(define ilist (term (ecompile ,g)))
(define s '(1 2))
(define i (car ilist))
(define state
  (term (
         ,ilist
         () ;; pl
         ,i
         0 ;; ip
         ,s
         () ;; stk
         () ;; clist
         )))
;; (traces ->e state)

ilist
i

(judgment-holds (gen-loop ,ilist 0 ,i () pl t) (pl t))

;; (judgment-holds (gen-loop (
                           
                           ;; ) 0 (call 0) (1 0 2) pl) pl)
