#lang racket

(require redex)
(require rackcheck)
(require peg-gen peg-gen/peg-gen-types)

(require "compiler/comp-peggen-peg.rkt")
(require "compiler/comp-peg-lpeg.rkt")
(require "ts-equations.rkt")
(require "view.rkt")

(let* ([ilist (term (
                     (choice 3)
                     ;; (char 1)
                     ;; (commit 2)
                     ;; (char 2)
                     end
                     ))]
       [pos 0]
       [i (list-ref ilist pos)]
       [type (judgment-holds (ts 0 state ,ilist ,pos ,i #f eq) eq)])
  type)
