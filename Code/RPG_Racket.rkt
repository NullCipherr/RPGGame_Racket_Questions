#lang racket

;; ---------------------------------------------------------- ;;
; 0. Canto das idéias
;; ---------------------------------------------------------- ;;

;  Espaço reservado para idéias de implementação 

; 1.1 Player HUD - Nome - Level - Experience - Stage -

;; ---------------------------------------------------------- ;;


; ------------------------------------------------------------ ;; 
; 1. Estrutura de Dados
; ------------------------------------------------------------ ;;

; Estrutura do personagem, contendo nome, Role, nível e experiência.
; name: Este campo armazena o nome do personagem.
; role: Este campo representa a função ou papel do personagem no jogo, como "guerreiro", "mago", etc.
; level: Este campo indica o nível atual do personagem.
; experience: Este campo representa a quantidade de experiência acumulada pelo personagem.
(struct player (id name difficulty level experience advance) #:transparent)

; Estrutura que representa o level(nivel em questão).
; number-level : Este campo armazena o número do nível. Esse campo pode conter valores como 1, 2, 3, ..., n.
; name : Este campo armazena o nome atribuído ao nível. 
; concept : Armazena uma descrição breve ou conceito do que o nível representa. Pode ser uma explicação sobre os desafios que os jogadores enfrentarão ou os conhecimentos que adquirirão ao completar o nível.
; reward-experience : Este campo representa a quantidade de experiência que um jogador ganha ao completar o nível. Pode ser um valor numérico que os jogadores acumulam para avançar em seus próprios níveis ou para desbloquear recursos adicionais.
; answer: Este campo armazena a resposta correta do nível.
; isComplete? : Um campo booleano que indica se o nível foi concluído ou não. Se o valor for verdadeiro, significa que o jogador concluiu com sucesso o nível; se for falso, indica que o nível ainda está em andamento ou não foi iniciado.
(struct level (number-level name concept answer reward-experience isComplete?) #:transparent)


; ===================================================================================================================================================

; (make-player: Cria e retorna um jogador com as informações fornecidas.
;
; Parâmetros:
;   - id: Identificador único do jogador.
;   - name: Nome do jogador.
;   - role: Papel ou função do jogador no jogo (ex: "guerreiro", "mago").
;   - level: Nível atual do jogador.
;   - experience: Quantidade de experiência acumulada pelo jogador.
;   - advance: Avanço ou progresso adicional do jogador.
; 
(define (make-player id name role level experience advance)
  ; Cada elemento da lista é uma cons célula que associa uma chave simbólica ao valor correspondente.
  (player id name role level experience advance))

; Inicializa o contador de criação de personagens
(define character-counter 0)


; --------------------------------------------------------
; Seção : Personagem
; --------------------------------------------------------

; Função para incrementar o valor da experiencia do personagem(LevelXP --> Player)
(define (increment-experience level player)
  ; Função para incrementar a experiência do jogador ao completar um nível
   ; Verificação
      (+ (player-experience player) (level-reward-experience level))
      (notify-increment-experience (level-reward-experience level))
      (begin
        (displayln "O level não foi completado!!")
        (player-experience player)))

; Função para avançar o level do personagem.
(define (level-up player)
  (+ (player-level player) 1)
  (notify-level-up (+ (player-level player) 1)))

;
(define (notify-level-up level)
  (newline)
  (display "Parabéns!! Você avançou para o nível ")(display level))

; Função que incrementa a expriência do personagem.
(define (notify-increment-experience experience)
  (newline)
  (display "Parabéns!! Você recebeu " )
  (display experience)
  (display " de experiência!!"))

; 
(define (gain-experience level player)
  (cond
    [(= (level-isComplete? level) 1) (increment-experience level player)]
    [else (displayln "Level não foi finalizado !!! " )]))


; --------------------------------------------------------
; Seção : Desafio
; --------------------------------------------------------


; Função para marcar um desafio como completado
(define (complete-level level)
  (displayln (string-append "Desafio completado: " (level-name level)))
  (true (level-isComplete?)))




(define (set-difficulty difficulty)
  ;(display (format "A dificuldade foi setada em ~a \n" difficulty))
  (if (equal? difficulty "Fácil")
        "Fácil"
        (if (equal? difficulty "Médio")
            "Médio"
            (if (equal? difficulty "Dificil")
                "Dificil"
                (error "Comando inválido!")))))



; --------------------------------------------------------
; Seção : Jogo
; --------------------------------------------------------


; Função gráfica p/ selecionar a role(JuniorDaCorte, PlenoCondês, KingSenior )
(define (display-select-difficulty name)
  (display "\n ==========================================================================")
  (display (format "\n    Saudações ~a, a seguir selecione o seu nível de dificuldade, ao
    qual corresponderá aos desafios que você deseja enfrentar no futuro!.
  
  (Para saber mais sobre os níveis de dificuldade, dê uma olhada no Guia)" name))
  (newline)
  (display " ==========================================================================\n")
  (displayln "                               1. Fácil                                    ")
  (displayln "                               2. Médio                                    ")
  (displayln "                               3. Dificil                                  ")
  (display " ==========================================================================\n")
  (displayln "                               0. Guide                                    ")
  (display " ==========================================================================\n")
  (newline))


; 
(define (verify-difficulty-number difficulty)
  (if (equal? difficulty "Fácil")
        (set-difficulty difficulty)
        (if (equal? difficulty "Médio")
            (set-difficulty difficulty)
            (if (equal? difficulty "Dificil")
                (set-difficulty difficulty)
                (if (= (string->number difficulty) 1)
                    (verify-difficulty-number "Fácil")
                    (if (= (string->number difficulty) 2)
                        (verify-difficulty-number "Médio")
                        (if (= (string->number difficulty) 3)
                            (verify-difficulty-number "Dificil")
                            (error "Dificuldade inválida!"))))))))

; ------------------------------------------------------------------------------------------------------------------- ;

; Função gráfica do escolher nome.
(define (display-select-name)
  (display "\nBem-vindo(a) à Aba de Seleção de Nome, o ponto de partida para a sua jornada extraordinária!
Neste reino digital, o nome que você escolher será mais do que uma simples etiqueta -
será a personificação da sua jornada e conquistas.")
  (newline))



; Função para escolher um nome.
(define (set-name)
  (display-select-name)
  (display "\n Nome -> ")
  (flush-output)
  ((read-line)))



; Função para avançar para a próxima fase
(define (next-level level player)
  (display "Parabéns, você está indo para a fase ")
  (displayln (level-number-level level)))



; Função para selecionar o personagem na lista de personagem
; int -> int
;
(define (start-game player)
  (newline)
  (display (format " Bem Vindo ~a, \n Digite 1 para começar o desafio, Boa Sorte!! \n\n" (player-name player)))
  (display "-> ")(let ((option (read-line)))
  (cond
    ((= (string->number option) 1) (initialize-game player))
    (else (start-game player)))))



; Função para criar o personagem
; id : Define o 'id' do personagem.
; (set-name) : Define o nome do personagem.
; (set-difficulty) : Define a dificuldade do personagem.
(define (create-player)
  (define id
    (cond
      ((= character-counter 0) 1)
      (else (error "Você já criou o seu personagem!!"))
     ))

  (display-select-name)
  (display "\n Nome -> ")
  (let ((name (read-line)))
  (if (> (string-length name) 15) (error "O nome não pode ser maior do que 15 caracteres!!") 0)

  (display-select-difficulty name)
  (display "\n Difficulty -> ")

  (let ((difficulty_temp (read-line)))
  (let ((difficulty (verify-difficulty-number difficulty_temp)))

  (let ((level 1))
  (let ((experience 0))
  
  
  (define character-1 (make-player id name difficulty level experience 1))
    (draw-player-hud character-1)
    (start-game character-1)
    ))))))

; (struct level (number-level name concept answer reward-experience isComplete?) #:transparent)
(define (save-progress player level)
  (let ((new-id (player-id player)))
  (let ((new-name (player-name player)))
  (let ((new-difficulty (player-difficulty player)))
  (let ((new-level (player-level player)))
  (let ((new-experience (+ (player-experience player) (level-reward-experience level))))
  (let ((new-stage (+ (player-advance player) 1)))
  (define new-character (make-player new-id new-name new-difficulty new-level new-experience new-stage))
    (draw-player-hud new-character)
    (select-level new-character (+ (level-number-level level) 1))
    )))))))


(define (start-level level player)
  (displayln "------------------------------------------------------------------------")
  (displayln (format "Nível ~a: ~a" (level-number-level level) (level-name level)))
  (newline)
  (displayln (level-concept level))
  (displayln (format "Recompensa de Experiência: ~a" (level-reward-experience level)))
  (displayln "------------------------------------------------------------------------")

  (display "Digite a sua resposta: ")
  (flush-output)
  (let ((user-answer (read-line)))
    (if (string=? user-answer (level-answer level))
        (begin
          (correct-answer player level)       
          (save-progress player level)
          (select-level player (+ (level-number-level level) 1)))
        (begin
          (incorrect-answer)
          (select-level player (+ (level-number-level level) 0))))))


(define (correct-answer player level)
  (newline)
  (displayln "Resposta correta! Avançando para o próximo nível...")
  (newline))

(define (incorrect-answer)
  (newline)
  (displayln "Resposta incorreta! Vamos tentar novamente...")
  (newline))
  


; Função que inicializa o level atual do personagem
(define (select-level player advance)
  (cond
    ((= advance 1) (start-level level-1 player))
    ((= advance 2) (start-level level-2 player))
    ((= advance 3) (start-level level-3 player))
    ((= advance 4) (display "\nLevel 4"))
    (else (display "\nVocê concluiu todos os níveis da dificuldade (DIFFICULTY) !!"))))



; Função que inicializa o desafio
(define (initialize-game player)
  (draw-player-hud player)
  (select-level player (player-advance player)))




; Função para inicializar o jogo.
(define (initialize-create)
  ; Função para iniciar o jogo e apresentar a narrativa inicial.
  (if (= character-counter 0)
      (displayln "\nParece que você não criou nenhum personagem ainda, a seguir digite um para criarmos o seu guerreiro!! \n\n 1. Criar personagem \n")
      (displayln "\nBem-vindo de volta guerreiro")
  )

  (display "-> ")
  (let ((x (read-line)))
  (if (= (string->number x) 1)
        (create-player)
        (initialize-create))))





; --> OKAY
; (menu-select) : Função que controla as seleções do menu.
;  - choose : Inteiro de entrada, que seleciona a ação do menu. ex: 1, 2, 3, 4
(define (menu-select)
  (display "-> ")(let ((choose (read-line)))
  (cond
    ((= (string->number choose) 1) (initialize-create))
    ((= (string->number choose) 2) (print-help))
    ((= (string->number choose) 3) (print-credits))
    ((= (string->number choose) 4) (error "Saindo..."))
    (else (menu-game)))))


; 1 --> OKAY
; Função do menu do jogo
(define (menu-game)
  (newline)
  (displayln " =========================================")
  (displayln "       Aventuras na Terra de Racket       ")
  (displayln " =========================================")
  (newline)
  (displayln "             1. Jogar")
  (displayln "             2. Ajuda")
  (displayln "             3. Créditos")
  (displayln "             4. Sair")
  (newline)
  (menu-select))





; Função de ajuda - OKAY
(define (print-help)
  (newline)
  (displayln " =========================================")
  (displayln "         Bem-vindo ao menu de ajuda       ")
  (displayln " =========================================")
  
  (displayln " Você está em uma jornada para aprender
 Racket e se tornar um mestre programador!")
  (newline)
  (displayln "       1.      Explorar Tutoriais")
  (displayln "       2.   Consultar Dicas do Mestre")
  (displayln "       3.    Ler Documentação Mágica")
  (displayln "       4.      Voltar à Aventura")
  (newline)

  (display "-> ")
  (let ((x (read-line)))
    (if (= (string->number x) 4)
        (menu-game)
        (displayln "Comando inválido!"))))


; Função que imprime os créditos do jogo
(define (print-credits)
  (newline)
  (displayln "              =========================================")
  (displayln "                 Seja muito bem vindo aos créditos.    ")
  (displayln "              =========================================")
  (newline)
  (displayln "  Aventuras na terra Racket é um jogo educativo, desenvolvido para a matéria       ")
  (displayln "  de Paradigma de Programação Lógica e Funcional do curso de ciência da computação ")
  (displayln "  fornecido pela Universidade Estadual de Maringá.                                 ")
  (newline)
  (displayln "  A seguir o nome e o RA de cada integrante envolvido no desenvolvimento           ")
  (newline)
  (displayln "  -------------------------------------------------------------------------------- ")
  (newline)
  (displayln "               Andrei Roberto da Costa  ..... RA 107975                            ")
  (newline)
  (displayln "               João Gilberto Casagrande ..... RA 115682                            ")
  (newline)
  (displayln "  -------------------------------------------------------------------------------- ")
  (newline)
  (displayln "    1. Retornar ao menu")
  (newline)

  (display "-> ")
  (let ((x (read-line)))
    (if (= (string->number x) 1)
        (menu-game)
        (print-credits))))





; -------------------------------------------------------------------------------------------------------
; 3. Definindo o personagem
; -------------------------------------------------------------------------------------------------------

(define player-1 (make-player 1 "Andrei" "Junior" 1 0 1))

(define (draw-player-hud player)
  (newline)
  (displayln " ------------------------------------------------------------------ ")
  (displayln " |          Name          Level          XP          Stage        | ")
  (display " ------------------------------------------------------------------ ")
  (newline)
  (display " |         ")
  (display (player-name player))
  (display "           ")
  (display (player-level player))
  (display "             ")
  (display (player-experience player))
  (display "             ")
  (display (player-advance player))
  (display "         |")
  (newline)
  (displayln " ------------------------------------------------------------------")
  (newline))

; (draw-player-hud player-1)


(define level-1 
  (level 1 
         "Início da Jornada" 
         "Você encontra-se em uma vila pacífica, onde os aldeões estão enfrentando um desafio. Uma criatura misteriosa deixou cair uma carta com a equação mágica escrita: '(= x 5)'. Os anciãos da vila acreditam que resolver essa equação trará bênçãos e fortalecerá suas habilidades mágicas. Você aceita o desafio e prepara-se para iniciar sua jornada, enfrentando o primeiro enigma mágico." 
         "(= x 5)" 
         50 
         false))

(define level-2 
  (level 2 
         "A Floresta Encantada" 
         "Após resolver o primeiro enigma, você adentra uma floresta encantada. Aqui, as árvores sussurram enigmas e os riachos guardam segredos mágicos. Um elfo sábio apresenta-lhe o desafio: 'Some 3 ao valor de y'. Decifre esta expressão e prove sua astúcia para avançar na jornada." 
         "(+ y 3)" 
         75 
         false))

(define level-3 
  (level 3 
         "Torre dos Magos" 
         "Você emerge da floresta em direção a uma torre majestosa. Os magos que a habitam testarão suas habilidades com a magia das funções. Eles propõem o desafio: '(define (square n) (* n n))'. Domine esta arte mágica para desbloquear o próximo nível de poder." 
         "(define (square n) (* n n))" 
         100 
         false))

(menu-game)
