#lang racket

(require redex)

(require "./lang.rkt")
(require "./processor.rkt")
(require "./compiler.rkt")
(require "./type-system.rkt")

;; (define e '(* (+ 3 (! 2)))) ;; generates loop on process
(define e '(+ 2 3))
(define s '(3 3))

;; compile
(define ilist (term (ecompile ,e)))
ilist

;; type system
;; (define ilist (term ((char 1) (char 2) end)))
(define i (term (fetch-i ,ilist 0)))
(judgment-holds (type-i ,ilist 0 ,i t) t)

;; processor
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
(traces ->e state)
