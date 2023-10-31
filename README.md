A estrutura da gramática de PEG está definida no arquivo peg.rkt.
Uma produção é dada pelo nome de um não terminal seguido de uma expressão. Ex.:
(S (* 2))
Uma gramática é o conjunto de produções:
(
    (S (* 2))
    (B (/ 2 (/ 3 4))
)

Uma PEG é compilada para uma LPEG utilizando a função peg->lpeg
(define peg (list (list 's0 '(• 2 (* 3)))))
(define peg (term ((s0 (• 2 (* 3))))))

A compilação da lpeg retorna o endereço de cada bloco e a lista de instruções geradas
