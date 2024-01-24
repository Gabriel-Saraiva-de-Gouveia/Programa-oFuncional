#lang racket
;; Trabalho 02 - Programação Funcional: 
;; Eduardo Rufino  RA: 129379
;; Gabriel Saraiva RA: 129145
(require examples)
;; Função auxiliar fornecida pelo professor:

;; Alternativa é um dos valores: 1, 2, 4, 8, 16.
;; Somatório é um número natural entre 0 e 31.

;; Somatório -> Lista(Alternativa)
;; Calcula a lista de alternativas que somadas gera o somatório s.
(examples
 (check-equal? (somatorio->alternativas 0) empty)
 (check-equal? (somatorio->alternativas 1) (list 1))
 (check-equal? (somatorio->alternativas 21) (list 1 4 16))
 (check-equal? (somatorio->alternativas 10) (list 2 8))
 (check-equal? (somatorio->alternativas 31) (list 1 2 4 8 16)))

(define (somatorio->alternativas s)
  ; Decompõe num com divisões sucessivas por 2.
  ; Se num é ímpar, o úlimo dígito binário é 1, então a alternativa está presente.
  (define (iter num alternativa)
    (cond
      [(zero? num) empty]
      [(odd? num) ; último dígito binário é 1
       (cons alternativa (iter (quotient num 2) (* 2 alternativa)))]
      [else (iter (quotient num 2) (* 2 alternativa))]))
  (iter s 1))

;; Objetivo geral: calcular o desempenho de cada candidato que não foi desclassificado.
;; Para isso, devemos projetar uma função que receba uma lista de provas e um gabarito,
;; e devolva uma lista com o desempenho de cada candidato.

;; O desempenho de um cadidado possui 2 campos, um código de candidato e o resultado de seu desempenho.
;; Assim, podemos representar o desempenho por uma estrutura:

;; O desempenho de um determinado candidato é representado pelos valores:
;;  codigo -> Numero : código do candidato
;;  nota -> NumeroRealPositivo: pontos finais do candidato

;; Cada prova possui um código de candidato, uma nota da redação,
;; e uma lista com a resposta das questões da prova.

;; Prova pode ser representa por uma estrutura:

(struct prova (codigo redacao respostas))
;; Representa uma prova, onde:
;;  codigo -> Numero: código do candidato
;;  redacao -> NumeroInteiroPositivo: nota da redação de um candidato (de 0 a 120)
;;  respostas -> Lista: resposta das questões da prova

;; E o gabarito é uma lista com a resposta correta das questões da prova

;; Calcular o desempenho de cada candidato
;; Listas(prova) Lista(gabarito)-> Listas(desempenho)

(examples
 (check-equal? (determina-desempenho empty empty) empty)
 (check-equal? (determina-desempenho empty (list 10 22 18 10 07)) empty)
 
 (check-equal? (determina-desempenho (list(prova 3441 0 (list 12 14 08 09 16))
                                          (prova 7889 107.0 (list 10 12 07 10 16))
                                          (prova 1212 93.0 (list 02 16 20 01 02))
                                          (prova 6755 56.0 (list 10 28 23 11 16)))
                                     (list 10 28 23 11 16))
               (list (list 7889 131.5)
                     (list 1212 103.0)
                     (list 6755 86.0)))
 (check-equal? (determina-desempenho (list(prova 5521 0 (list 02 04 12 13 30))
                                          (prova 4423 112.0 (list 02 06 13 15 31))
                                          (prova 9921 117.0 (list 02 04 09 13 30))
                                          (prova 0766 120.0 (list 02 06 13 15 31)))
                                     (list 02 06 13 15 31))
               (list (list 4423 142.0)
                     (list 9921 139.3)
                     (list 0766 150.0)))
 (check-equal? (determina-desempenho (list(prova 8845 111.0 (list 30 28 20 16 02 04))
                                          (prova 1010 97.5 (list 27 30 23 04 04 01)))
                                     (list 31 31 21 13 04 06))
               (list (list 8845 126.4)
                     (list 1010 117.1))))

(define (determina-desempenho provas gabarito)
  (define (calcular-nota prova)
    (define pontos (conta-pontos (prova-respostas prova) gabarito))
    (if (> (prova-redacao prova) 0)
        (+ (prova-redacao prova) pontos)
        pontos))
    
  (define (filtrar-provas provas) ;; apenas as provas dos candidatos que não foram desclassificados
    (filter (lambda (prova) (> (prova-redacao prova) 0)) provas))
    
  (map (λ (prova) (list (prova-codigo prova) (calcular-nota prova)))
       (filtrar-provas provas)))

;; Verificação: Parece que ocorre um problema na representação dos exemplos -> no caso das lista
;; Revisao: Criamos uma função chamada filtrar-provas, segue a especificação da mesma:
;; Retorna uma lista com apenas os cadidatos que não foram desclassificados,
;; ou seja, não tiraram 0 na redação
;; ListaDeCandidatos -> ListaDeCandidatos
;; Exemplos:
;; (8441 0 (respostas))(8104 120 (respostas))) -> (8104 120 (respostas))
;; (4432 109 (respostas))(2316 0 (respostas))(1559 87 (respostas)) -> (4432 109 (respostas))(1559 87 (respostas))

;; Contabilizar quantos pontos um candidato fez a partir de suas respostas e do gabarito
;; Lista(respostas) -> Numeros
(examples
 (check-equal? (conta-pontos empty empty) 0)
 (check-equal? (conta-pontos empty (list 08 16 10 12)) 0)
 (check-equal? (conta-pontos (list 10 12 07 10 16)(list 10 28 23 11 16)) 24 1/2)
 (check-equal? (conta-pontos (list 02 16 20 01 02)(list 10 28 23 11 16)) 10)
 (check-equal? (conta-pontos (list 10 28 23 11 16)(list 10 28 23 11 16)) 30)
 (check-equal? (conta-pontos (list 30 28 20 16 02 04)(list 31 31 21 13 04 06)) 15 2/5))

(define (conta-pontos respostas gabarito)
  (cond
    [(empty? respostas) 0]
    [else
     (+(* (valor-alternativas (first gabarito))
          (compara-gabarito (first respostas) (first gabarito)))
       (conta-pontos (rest respostas)(rest gabarito)))]))

;; Verificação: ok
;; Revisao: ok

;; Calcula quanto cada alternativa vale
;; Numero -> Numero
(examples
 (check-equal? (valor-alternativas 10) 3)
 (check-equal? (valor-alternativas 23) 1 1/2) 
 (check-equal? (valor-alternativas 01) 6)
 (check-equal? (valor-alternativas 11) 2)
 (check-equal? (valor-alternativas 31) 1 1/5))

(define (valor-alternativas alternativa)
  (define valores (somatorio->alternativas alternativa))
  (/ 6 (length valores)))

;; Verificação: ok!
;; Revisao: ok!

;; Contabilizar quantas alternativas corretas a alternativa que o candidato marcou possui
;; Numero(resposta) Numero(Gabarito) -> Numero
(examples
 (check-equal? (compara-gabarito 10 10) 2)
 (check-equal? (compara-gabarito 08 10) 1)
 (check-equal? (compara-gabarito 22 23) 3)
 (check-equal? (compara-gabarito 18 19) 2)
 (check-equal? (compara-gabarito 30 31) 4)
 (check-equal? (compara-gabarito 28 31) 3))

(define (compara-gabarito resposta gabarito)
  (define alt-respostas (somatorio->alternativas resposta))
  (define alt-gabarito (somatorio->alternativas gabarito))
  (define (conta-validas lista1 lista2)
    (cond
      [(empty? lista1) 0]
      [else
       (if (member (first lista1) lista2)
            (+ 1 (conta-validas(rest lista1) lista2))
            0)]))
  (conta-validas alt-respostas alt-gabarito))

;; Verificação: ok!
;; Revisao: utilizamos a funcao member, importada do racket, para verificar se o valor que estamos
;; comparando da primeira lista existe na segunda lista. Criamos algumas definicoes:
;; -> alt-repostas: lista com as alternativas da alternativa de resposta
;; -> alt-gabarito: lista com as alternativas da alternativa de gabarito
