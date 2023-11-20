; (* 3)
(declare-const t0 Bool)
(declare-const t1 Bool)
(declare-const t2 Bool)
(declare-const t3 Bool)

(assert (= t0 (or t1 t3)))
(assert (= t1 false))
(assert (= t1 false))
(assert (= t2 t0))
(assert (= t3 true))

(check-sat)
(get-model)
