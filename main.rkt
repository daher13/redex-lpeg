#lang racket

(require redex)

(require "./lang.rkt")
(require "./processor.rkt")
(require "./compiler.rkt")
(require "./type-system.rkt")

;; (define e '(* (+ (! 2) 3))) ;; didn't generate loop... why?
(define e '(* (+ 2 3)))
(define s '(2 2 2 3 3 3))

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

;; (term
;;  (redex-match LPEG (((choice 6)
;;                      (choice 3)
;;                      (char 2)
;;                      (commits)
;;                      (char 3)
;;                      (commit -5)
;;                      end)
;;                     ()
;;                     fail
;;                     4
;;                     ()
;;                     0
;;                     ((6 (0 0)))) (ilist pl fail ip s sp ((n_1 (sp_1 ip_1)) stke ...))))
