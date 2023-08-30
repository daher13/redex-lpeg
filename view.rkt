#lang racket

(define (print-list ilist)
  (for ([pc (length ilist)] [i ilist])
    (printf "~a -> ~a\n" pc i)))

(provide (all-defined-out))
