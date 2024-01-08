#lang racket
(require examples)
(examples
 (check-equal? (conta empty 0) 0)
 (check-equal? (conta (list 1 2 3 4) 2) 1)
 (check-equal? (conta (list 1 1 1 1 1 1) 1) 6)
 (check-equal? (conta (list 1 2 3 4 5 9 9 9 9) 9) 4))

(define (conta lst0 n1)
  (define (iter lst n acc-soma)
    ;; Aqui, o acumulador é a soma dos elementos já repetidos anteriores ao primeiro de lst
    (cond
      [(empty? lst) acc-soma]
      [else
       (if(= n (first lst))
         (iter (rest lst) n (add1 acc-soma))
          (iter (rest lst) n acc-soma))]))
  (iter lst0 n1 0))