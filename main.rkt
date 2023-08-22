#lang racket

(require redex)

(require "parser.rkt")
(require "compiler.rkt")
(require "type-system.rkt")

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

(define g '((S D) (D E) (E (! 4))))
;; (define g '(+ (* 2) (* (+ 1 (* 10))))) ;; loop and acc empty, but well formed
;; (define g '(* 2))
;; (define g '(* (! 3))) ;; loop, acc empty, not well formed
;; (define g '(+ (* 2) (* (! 3))))
;; (define g '(+ 1 2))
(define ilist (term (ecompile ,g)))
;; (define ilist (term ((commit 2) (char 1) (commit -2) end)))
;; well formed = all loops goto end instruction
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

(judgment-holds (ts ,ilist 0 ,i () () t) t)
;; (judgment-holds (wf ,ilist b) b)
