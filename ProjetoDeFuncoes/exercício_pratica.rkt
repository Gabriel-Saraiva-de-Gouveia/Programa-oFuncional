#lang racket
(define (rotaciona-a-direita nome n)
  (if(> n (string-length nome))
      "Tamanho incompativel"
      (string-append (substring nome
                            (-(string-length nome) n)
                            (string-length nome))
                 (substring nome 0 (-(string-length nome)n))
                 )))
