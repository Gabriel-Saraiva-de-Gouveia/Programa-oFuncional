#lang racket
;; Gabriel Saraiva de Gouveia RA: 129145
;; Lucas Vitorino Amaral Souza RA: 130236
;; Trabalho 01 - Programação Funcional: Sistema de controle de infrações

;; 1) Função 01:
;; Análise:
;; - Determinar a velocidade que deve ser considerada a partir da velocidade medidade por um radar.
;; Considerando que,
;; se a velocidade medida for até 107Km/h, a velocidade considerada será a
;; velocidade medida menos 7Km/h.
;; Caso a velocidade medida seja maior que 107Km/h, considerar que a velocidade será
;; 7% a menos que a velocidade medida

;; Tipos de dados:
;; a função terá com entrada um valor inteiro positivo maior que 0, que representa a velocidade medida
;; e retornará um valor ponto flutuante positivo que representa a velocidade a ser considerada

;; Especificação:
;; Velocidade-medida -> Velocidade-final
;;  - Assinatura: (define (determina-velocidade velocidade-medida)) -> velocidade-final
;;  - Propósito: calcular e determinar a velocidade a ser considerada a partir da velocidade medida por um radar

;;  - Exemplos:
(require examples)
(examples
 (check-equal? (determina-velocidade 57) 50)
 (check-equal? (determina-velocidade 84) 77)
 (check-equal? (determina-velocidade 107) 100)
 (check-equal? (determina-velocidade 112) 104.16)
 (check-equal? (determina-velocidade 123) 114.39))

;; Implementação:
(define (determina-velocidade velocidade-medida)
  (if (<= velocidade-medida 107) ;;
      (- velocidade-medida 7);;
      (- velocidade-medida
         (* velocidade-medida 0.07))))

;; Verificação: Todos os exemplos dados resultam em sucesso
;; Revisão: ok!

;; 2) Função 02:
;; Análise:
;; determinar a partir da velocidade medida por um radar e do limite
;; de velocidade da via, qual o tipo de infração por excesso de velocidade, se alguma, foi cometida por um motorista.
;; Se o limite for excedido em até 20%, a infração é dada como média.
;; Se o limite for excedido entre 20% e 50% a infração é grave.
;; Se o limite for excedido acima de 50% a infração é gravíssima.

;; Tipos de Dados:
;; a função terá como entrada um valor ponto flutuante, que representa a velocidade medida por um radar,
;; calculada pela funcao "determina-velocidade", e um valor discreto, que representa o limite de velocidade da via.
;; A função retornará o tipo de infração cometida por um motorista. Infração pode ser um dos valores de strings:
;;   - "leve"
;;   - "média"
;;   - "grave"
;;   - "gravíssima"

;; Especificação:
;; Velocidade-medida Limite-via -> Infracao
;;  - Assinatura: (define (determina-infracao velocidade-medida limite-via)) -> tipo-infracao
;;  - Propósito: determinar a partir da velocidade de um veículo e o limite da via o tipo de infração cometida.

;;  - Exemplos:
(examples
 (check-equal? (determina-infracao 53.0 40.0) "media") ;; 15%
 (check-equal? (determina-infracao 55.0 40.0) "media") ;; 20%
 (check-equal? (determina-infracao 61.0 40.0) "grave") ;; 35%
 (check-equal? (determina-infracao 67.0 40.0) "grave") ;; 50%
 (check-equal? (determina-infracao 71.0 40.0) "gravissima") ;; 60%
 (check-equal? (determina-infracao 79.0 40.0) "gravissima") ;; 80%

 (check-equal? (determina-infracao 76.0 60.0) "media") ;; 15%
 (check-equal? (determina-infracao 79.0 60.0) "media") ;; 20%
 (check-equal? (determina-infracao 88.0 60.0) "grave") ;; 35%
 (check-equal? (determina-infracao 97.0 60.0) "grave") ;; 50%
 (check-equal? (determina-infracao 103.0 60.0) "gravissima") ;; 60%
 (check-equal? (determina-infracao 115.56 60.0) "gravissima") ;; 80%

 (check-equal? (determina-infracao 99.0 80.0) "media") ;; 15%
 (check-equal? (determina-infracao 103.0 80.0) "media") ;; 20%
 (check-equal? (determina-infracao 115.56 80.0) "grave") ;; 35%
 (check-equal? (determina-infracao 128.4 80.0) "grave") ;; 50%
 (check-equal? (determina-infracao 136.96 80.0) "gravissima") ;; 60%
 (check-equal? (determina-infracao 154.08 80.0) "gravissima") ;; 80%
 )

;; Implementação:

(define (determina-infracao velocidade-medida limite-via)
  (define velocidade-final (determina-velocidade velocidade-medida))
  (define vinte-por-cento-via (+ limite-via(* limite-via 0.20))) ;; 20% a mais do limite da via
  (define cinquenta-por-cento-via (+ limite-via(* limite-via 0.50))) ;; 50% a mais do limite da via
  (cond
    [(<= velocidade-final vinte-por-cento-via)
     "media"]
    [(and
      (> velocidade-final vinte-por-cento-via)
      (<= velocidade-final cinquenta-por-cento-via))
     "grave"]
    [else
     "gravissima"]))

;; Verificação: Todos os exemplos dados resultam em sucesso
;; Revisão: modificações -> criação de algumas definições.

;; 3) Função 03:
;; Análise:
;; determinar se a CNH de um motorista deve ser suspensa por ultrapassar o limite de pontos, que é dado por:
;; 40 pontos, se não cometeram nenhuma infração grave ou gravíssima
;; 30 pontos, se cometeram uma infração grave ou gravíssima
;; 20 pontos para os demais.

;; Cada uma das classificações de infrações está associada com uma quantidade de pontos:
;; 3 pontos para leve
;; 4 pontos para média
;; 5 pontos para grave
;; 7 pontos para gravíssima

;; Tipos de dados:
;; a função terá como entrada a quantidade de infracoes de um determinado motorista, esses infracoes serão representadas
;; por um tipo de dado composto, chamaremos de cnh-infracoes. E como saída,
;; a função retornará se a CNH devera ser suspensa ou não,
;; representaremos "suspenso ou não" por uma string.
;; Assim, representaremos cnh-infracoes como uma estrutura:

(struct cnh-infracoes (leve media grave gravissima) #:transparent) 

;; CNH-infracoes representa a quantidade de infracoes de um motorista:
;;   leve: Número - número de infrações considerados leves
;;   media: Número - número de infrações considerados médias
;;   grave: Número - número de infrações considerados graves
;;   gravissima: Número - número de infrações considerados gravíssimas

;; Especificação:
;; CNH-pontos -> String ("suspensa" ou "não suspensa")

;;  - Assinatura: (define (cnh-suspensa infracoes)) -> String
;;  - Propósito: determinar se a CNH de um motorista deverá ser suspensa ou não.
;;  - Exemplos:
(examples
 (check-equal? (cnh-suspensa (cnh-infracoes 6 5 0 0)) "não suspensa") ; 38 pontos
 (check-equal? (cnh-suspensa (cnh-infracoes 5 4 0 0)) "não suspensa") ; 31 pontos
 (check-equal? (cnh-suspensa (cnh-infracoes 7 6 0 0)) "suspensa") ; 45 pontos
 (check-equal? (cnh-suspensa (cnh-infracoes 4 3 1 0)) "não suspensa") ; 29 pontos   
 (check-equal? (cnh-suspensa (cnh-infracoes 5 3 1 0)) "suspensa") ; 32 pontos
 (check-equal? (cnh-suspensa (cnh-infracoes 3 3 0 1)) "não suspensa") ; 28 pontos
 (check-equal? (cnh-suspensa (cnh-infracoes 4 4 0 1)) "suspensa") ; 35 pontos
 (check-equal? (cnh-suspensa (cnh-infracoes 0 2 1 1)) "não suspensa") ; 20 pontos
 (check-equal? (cnh-suspensa (cnh-infracoes 1 2 0 2)) "suspensa") ; 25 pontos
 (check-equal? (cnh-suspensa (cnh-infracoes 0 0 2 3)) "suspensa") ; 31 pontos
 )

;; Implementação:
(define (cnh-suspensa infracoes)
  (define contabiliza-pontos
    (+
    (* 3 (cnh-infracoes-leve infracoes))
    (* 4 (cnh-infracoes-media infracoes))
    (* 5 (cnh-infracoes-grave infracoes))
    (* 7 (cnh-infracoes-gravissima infracoes))))
  (cond
    [(and (= (cnh-infracoes-grave infracoes) 0)
          (= (cnh-infracoes-gravissima infracoes) 0))
     (if(<= contabiliza-pontos 40)
        "não suspensa" "suspensa")] ;; nenhuma grave nem gravíssima
    
    [(or
      (and (= (cnh-infracoes-grave infracoes) 1)(= (cnh-infracoes-gravissima infracoes) 0))
      (and (= (cnh-infracoes-gravissima infracoes) 1)(= (cnh-infracoes-grave infracoes) 0)))
     (if(<= contabiliza-pontos 30)
        "não suspensa" "suspensa")] ;; uma grave ou gravíssima
    
    [else
     (if (<= contabiliza-pontos 20)
         "não suspensa" "suspensa")] ;; demais casos
    ))

;; Verificação: primeira condição, nenhuma grave nem gravíssima, funciona para todos os exemplos.
;; A segunda condição está retornando "suspensa" para os casos em que deve ser "não suspensa".
;; Mesmo erro anterior para a última condição. -> Concertados: todos os exemplos passaram!

;; Revisão: ok!
