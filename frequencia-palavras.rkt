#lang racket

(require examples)

;; +--------------------------------------------------------------------------------------------------------------------------------------+
;; |  Trabalho 1 da disciplina de Paradigma de Programação Lógica e Funcional(PPLF) com Prof: Dr. Marco Aurélio Lopes Barbosa             |
;; |  Luiz Flávio Pereira ra91706		                                                                                          |
;; +--------------------------------------------------------------------------------------------------------------------------------------+

;; -------------------------------------------------------- Estruturas e Variáveis --------------------------------------------------------
;; representando uma lista(List)
(define-struct lista (primeiro restante) #:transparent)

;; representando o vazio(Empty)
(define-struct vazio () #:transparent)

;; representando a frequencia das palavras
(define-struct frequencia-palavra (palavra quantidade) #:transparent)

;; representando variáveis
(define zero 0)
(define contador 1)
(define msg-erro "Não é possivel imprimir posições menores ou iguais a 0 de uma Lista(frequencia-palavra) não vazia!")

;; represetando listas que serão usadas nos 'check-equal?' para nos auxiliar nos testes
(define uma-lista-vazia (vazio))
(define lista-palavra (lista "TrabalhoPPLF" (lista "EhDificil" (lista "Demais" (vazio)))))
(define lista-palavra-repetida (lista "TrabalhoPPLF" (lista "EhDificil" (lista "EhDificil" (lista "Demais" (lista "Demais" (lista "Demais" (vazio))))))))
(define lista-frequencia-palavra-crescente (lista (frequencia-palavra "TrabalhoPPLF" 1) (lista (frequencia-palavra "EhDificil" 2) (lista (frequencia-palavra "Demais" 3) (vazio)))))
(define lista-frequencia-palavra-decrescente (lista (frequencia-palavra "Demais" 3) (lista (frequencia-palavra "EhDificil" 2) (lista (frequencia-palavra "TrabalhoPPLF" 1) (vazio)))))

;; --------------------------------------------------------------- Funções ----------------------------------------------------------------

;; String, Lista(String) -> Lista(String)
;; função que descarta/ignora palavras repetidas da lista fornecida
(examples
 (check-equal? (apaga-palavra "TrabalhoPPLF" uma-lista-vazia) uma-lista-vazia)
 (check-equal? (apaga-palavra "TrabalhoPPLF" lista-palavra) (lista "EhDificil" (lista "Demais" (vazio))))
 (check-equal? (apaga-palavra "EhMoleza" lista-palavra) lista-palavra))

#;
(define (apaga-palavra palavra lst)
  (cond [(vazio? lst) ...]
        [else
         (if (palavra=? palavra (lista-primeiro lst))
             ...
             ...)]))

(define (apaga-palavra palavra lst)
  (cond [(vazio? lst) (vazio)]
        [else
         (if (string=? palavra (lista-primeiro lst))
             (apaga-palavra palavra (lista-restante lst))
             (lista (lista-primeiro lst) (apaga-palavra palavra (lista-restante lst))))]))

;; String, Lista(String) -> Número
;; função que conta palavras repetidas da lista fornecida
(examples
 (check-equal? (conta-palavra-repetida "EhSuperFacil" uma-lista-vazia) 0)
 (check-equal? (conta-palavra-repetida "EhMoleza" lista-palavra) 0)
 (check-equal? (conta-palavra-repetida "EhDificil" lista-palavra-repetida) 2))

#;
(define (conta-palavra-repetida palavra lst)
  (cond
    [(vazio? lst) ...]
    [(palavra=? palavra (lista-primeiro lst)) ...]
    [else ...]))

(define (conta-palavra-repetida palavra lst)
  (cond
    [(vazio? lst) zero]
    [(string=? palavra (lista-primeiro lst)) (add1 (conta-palavra-repetida palavra (lista-restante lst)))]
    [else (conta-palavra-repetida palavra (lista-restante lst))]))

;; Lista(frequencia-palavra) -> Lista(frequencia-palavra)(essa lista será gerada pela função auxiliar insere-inicio-lista na verdade)
;; função que realizará a ordenação do frequencia-palavra através da chamada de outra função e suas recursividades
(examples
 (check-equal? (ordena-lista-decrescente uma-lista-vazia) uma-lista-vazia)
 (check-equal? (ordena-lista-decrescente lista-frequencia-palavra-crescente) lista-frequencia-palavra-decrescente))

#;
(define (ordena-lista-decrescente lst)
  (cond
    [(vazio? lst) ...]
    [else ...]))

(define (ordena-lista-decrescente lst)
  (cond
    [(vazio? lst) (vazio)]
    [else (insere-inicio-lista (lista-primeiro lst) (ordena-lista-decrescente (lista-restante lst)))]))

;; frequencia-palavra, Lista(frequencia-palavra) -> Lista(frequencia-palavra)
;; função auxiliar à ordena-lista-decrescente, tem como obrigação inserir no início da Lista(frequencia-palavra) a palavra que for tendo a maior frequencia
(examples
 (check-equal? (insere-inicio-lista (frequencia-palavra "Luiz" 1) uma-lista-vazia) (lista (frequencia-palavra "Luiz" 1) (vazio)))
 (check-equal? (insere-inicio-lista (frequencia-palavra "EhDificil" 1) (lista (frequencia-palavra "TrabalhoPPLF" 2) (vazio)))
                              (lista (frequencia-palavra "TrabalhoPPLF" 2) (lista (frequencia-palavra "EhDificil" 1) (vazio)))))

#;
(define (insere-inicio-lista primeiro lst)
  (cond
    [(vazio? lst) ...]
    [(> (frequencia-palavra-quantidade primeiro) (frequencia-palavra-quantidade (lista-primeiro lst))) ...]
    [else ...]))

(define (insere-inicio-lista primeiro lst)
  (cond
    [(vazio? lst) (lista primeiro (vazio))]
    [(> (frequencia-palavra-quantidade primeiro) (frequencia-palavra-quantidade (lista-primeiro lst))) (lista primeiro lst)]
    [else (lista (lista-primeiro lst) (insere-inicio-lista primeiro (lista-restante lst)))]))

;; Número, Número, Lista(frequencia-palavra) -> Lista(frequencia-palavra) ou Error
;; função que apresentará as palavras mais frequentes de acordo com a quantidade de posições informada em sua chamada
;; Obs: essa função sempre esperará a Lista(frequencia-palavra) já ordenada decrescentemente
;; Obs: se POSICOES maior que o tamanho da Lista(frequencia-palavra) fornecida, então a Lista(frequencia-palavra) toda será apresentada
(examples
 (check-equal? (palavra-mais-frequente 1 01 lista-frequencia-palavra-decrescente) (lista (frequencia-palavra "Demais" 3) (vazio)))
 (check-equal? (palavra-mais-frequente 1 03 lista-frequencia-palavra-decrescente) lista-frequencia-palavra-decrescente)
 (check-equal? (palavra-mais-frequente 1 99 lista-frequencia-palavra-decrescente) lista-frequencia-palavra-decrescente)

 ;;testando quando há o uso do 'error'
 (check-exn exn:fail? (thunk (palavra-mais-frequente 1 00 lista-frequencia-palavra-decrescente) msg-erro))
 (check-exn exn:fail? (thunk (palavra-mais-frequente 1 -1 lista-frequencia-palavra-decrescente) msg-erro)))

#;
(define (palavra-mais-frequente contador posicoes lst)
  (cond
    [(vazio? lst) ...]
    [(or (zero? posicoes) (negative? posicoes)) ...]
    [(= contador posicoes) ...]
    [else ...]))

(define (palavra-mais-frequente contador posicoes lst)
  (cond
    [(vazio? lst) (vazio)]
    [(or (zero? posicoes) (negative? posicoes)) (error msg-erro)]
    [(= contador posicoes) (lista (lista-primeiro lst) (vazio))]
    [else (lista (lista-primeiro lst) (palavra-mais-frequente (add1 contador) posicoes (lista-restante lst)))]))

;; Lista(String), Número -> Lista(frequencia-palavra) ou Error
;; A função conta a frequencia de ocorrências de uma determinada palavra, ordena a Lista(frequencia-palavra) decrescentemente e
;; finaliza sua execução apresentando o resultado de acordo com o POSICOES informado.
(examples
 (check-equal? (calcula-frequencia uma-lista-vazia 01) uma-lista-vazia)
 (check-equal? (calcula-frequencia lista-palavra-repetida 03) lista-frequencia-palavra-decrescente)
 (check-equal? (calcula-frequencia lista-palavra-repetida 99) lista-frequencia-palavra-decrescente)

 ;;testando quando há o uso do 'error'
 (check-exn exn:fail? (thunk (calcula-frequencia lista-palavra 00) msg-erro))
 (check-exn exn:fail? (thunk (calcula-frequencia lista-palavra -1) msg-erro)))

#;
(define (calcula-frequencia lst posicoes)
  (cond
    [(vazio? lst) ...]
    [else 
     (let* ([nova-lista-sem-repeticao ...]
            [quantidade-repetido ...]
            [monta-frequencia-palavra ...]
            [monta-lista-frequencia-palavra ...]
            [ordena-lista-frequencia-palavra ...])
            (...))]))

(define (calcula-frequencia lst posicoes)
  (cond
    [(vazio? lst) (vazio)]
    [else (let* ([nova-lista-sem-repeticao (apaga-palavra (lista-primeiro lst) lst)]
                 [quantidade-repetido (conta-palavra-repetida (lista-primeiro lst) lst)]
                 [monta-frequencia-palavra (frequencia-palavra (lista-primeiro lst) quantidade-repetido)]
                 [monta-lista-frequencia-palavra (lista monta-frequencia-palavra (calcula-frequencia nova-lista-sem-repeticao posicoes))]
                 [ordena-lista-frequencia-palavra (ordena-lista-decrescente monta-lista-frequencia-palavra)])
                 (palavra-mais-frequente contador posicoes ordena-lista-frequencia-palavra))]))

;; +--------------------------------------------------------------------------------------------------------------------------------------+
;; |  A modelagem das estruturas de list e empty foram baseados no conteúdo visto em aula, além do trecho de remover algum dado.          |
;; |  Link: https://malbarbo.pro.br/arquivos/2020/5200/05-autorreferencia-exemplos.rkt                                                    |
;; |                                                                                                                                      |
;; |  Para a ordenação da lista decrescentemente, me basiei na função de ordenação do link abaixo.                                        |
;; |  Link: https://malbarbo.pro.br/arquivos/2020/5200/05-autorreferencia-resolvidos.rkt                                                  |
;; +--------------------------------------------------------------------------------------------------------------------------------------+
;; |  Obs: na descrição do trabalho diz pra "projeto completo da função" vista em aula, preferi deixar link para não ficar muito poluído. |
;; +--------------------------------------------------------------------------------------------------------------------------------------+