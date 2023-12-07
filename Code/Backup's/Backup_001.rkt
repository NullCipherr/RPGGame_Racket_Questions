#lang racket

; Estrutura de Dados
(struct player (name level experience))
(struct challenge (id name concept complete experience))


; --------------------------------------------------------
; Seção : Personagem
; --------------------------------------------------------

; Função para avançar o level do personagem
(define (level-up player)
  (+ (player-level player) 1))

(define (increment-experience challenge player)
  (display "Parabéns!! Você recebeu ") 
  (display (+ (player-experience player) (challenge-experience challenge)))
  (display " de experiência."))

(define (gain-experience challenge player)
  (cond
    [(= (challenge-complete challenge) 1) (increment-experience challenge player)]
    [else (displayln "Level não foi finalizado !!! " )]))




; --------------------------------------------------------
; Seção : Desafio
; --------------------------------------------------------

(define (initialize-challenge challenge) ; Função para iniciar um novo desafio
  (displayln (string-append "Iniciando o desafio: " (challenge-name challenge)))
  (displayln (string-append "Conceito: " (challenge-concept challenge)))
  (displayln "Boa sorte!"))





(define (complete-challenge challenge); Função para marcar um desafio como completado
  (displayln (string-append "Desafio completado: " (challenge-name challenge))))





(define (write-code code)
  ; Função para permitir que o jogador escreva código
  (displayln (string-append "Código inserido: " code)))




; --------------------------------------------------------
; Seção : Jogo
; --------------------------------------------------------

; Função para avançar para a próxima fase
(define (next-challenge challenge player)
  (display "Parabéns, você está indo para a fase ")
  (displayln (challenge-id challenge)))





; Função para inicializar o jogo
(define (initialize-game)
  ; Função para iniciar o jogo e apresentar a narrativa inicial
  (displayln "Bem-vindo a Aventuras na Terra de Racket!")
  (displayln "Você foi escolhido para restaurar a ordem na Terra de Racket.")
  (displayln "Prepare-se para enfrentar desafios mágicos e dominar a arte da programação em Racket."))





; Função de ajuda
(define (print-help)
  (displayln "Bem-vindo ao menu de ajuda.")
  (displayln "Você pode acessar tutoriais, dicas contextuais e documentação integrada aqui."))





; -------------------------------------------------------------------------------------------------------
; Definindo o personagem
(define player-1 (player "ZeroCool" 1 0))
(define challenge-1(challenge 1 "Level 1" "Comparação" 0 50)) 