; (* (* 3))
(declare-const t0 Bool)
(declare-const t1 Bool)
(declare-const t2 Bool)
(declare-const t3 Bool)
(declare-const t4 Bool)
(declare-const t5 Bool)

(assert (= t0 (or t1 t5)))
(assert (= t1 (or t2 t4)))
(assert (= t2 false))
(assert (= t3 t1))
(assert (= t3 false))
(assert (= t4 t0))
(assert (= t5 true))

(check-sat)
(get-model)
