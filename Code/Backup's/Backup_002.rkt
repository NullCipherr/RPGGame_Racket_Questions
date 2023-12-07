#lang racket

;; ---------------------------------------------------------- ;;
; 0. Canto das idéias
;; ---------------------------------------------------------- ;;

;  Espaço reservado para idéias de implementação 

; 1.1 Função de dificuldades -> Roles -> Junior, Pleno, Senior, etc -> +experience
; 1.2 Cada nível ter um nome customizado (Jornada pela comparação, Castelo da semantica, ...)
; 1.3 Lista contendo todos os desafios (Penso ser melhor de controlar uma sequencia de niveis)

;; ---------------------------------------------------------- ;;


; ------------------------------------------------------------ ;; 
; 1. Estrutura de Dados
; ------------------------------------------------------------ ;;

; Estrutura do personagem, contendo nome, Role, nível e experiência.
; name: Este campo armazena o nome do personagem.
; role: Este campo representa a função ou papel do personagem no jogo, como "guerreiro", "mago", etc.
; level: Este campo indica o nível atual do personagem.
; experience: Este campo representa a quantidade de experiência acumulada pelo personagem.
(struct player (id name role level experience advance) #:transparent)

; Estrutura que representa o level(nivel em questão).
; number-level : Este campo armazena o número do nível. Esse campo pode conter valores como 1, 2, 3, ..., n.
; name : Este campo armazena o nome atribuído ao nível. 
; concept : Armazena uma descrição breve ou conceito do que o nível representa. Pode ser uma explicação sobre os desafios que os jogadores enfrentarão ou os conhecimentos que adquirirão ao completar o nível.
; reward-experience : Este campo representa a quantidade de experiência que um jogador ganha ao completar o nível. Pode ser um valor numérico que os jogadores acumulam para avançar em seus próprios níveis ou para desbloquear recursos adicionais.
; isComplete? : Um campo booleano que indica se o nível foi concluído ou não. Se o valor for verdadeiro, significa que o jogador concluiu com sucesso o nível; se for falso, indica que o nível ainda está em andamento ou não foi iniciado.
(struct level (number-level name concept reward-experience isComplete?))


(define character-list '()) ; Lista inicial vazia



; --------------------------------------------------------
; Seção : Personagem
; --------------------------------------------------------

; Função para avançar o level do personagem.
(define (level-up player)
  (+ (player-level player) 1)
  (notify-level-up (+ (player-level player) 1)))

;
(define (notify-level-up level)
  (display "Parabéns!! Você avançou para o nível ")(display level))

; Função para incrementar o valor da experiencia do personagem(LevelXP --> Player)
(define (increment-experience level player)
  ; Função para incrementar a experiência do jogador ao completar um nível
  (if (level-isComplete? level) ; Verificação
      (+ (player-experience player) (level-reward-experience level))
      (notify-increment-experience))
      (begin
        (displayln "O level não foi completado!!")
        (player-experience player)))

; Função que incrementa a expriência do personagem.
(define (notify-increment-experience experience)
  (display "Parabéns!! Você recebeu ~a de experiência" experience))

; 
(define (gain-experience level player)
  (cond
    [(= (level-isComplete? level) 1) (increment-experience level player)]
    [else (displayln "Level não foi finalizado !!! " )]))


; --------------------------------------------------------
; Seção : Desafio
; --------------------------------------------------------

(define (initialize-challenge level) ; Função para iniciar um novo desafio
  (displayln (string-append "Iniciando o desafio: " (level-name level)))
  (displayln (string-append "Conceito: " (level-concept level)))
  (displayln "Boa sorte!"))




; Função para marcar um desafio como completado
(define (complete-level level)
  (displayln (string-append "Desafio completado: " (level-name level)))
  (true (level-isComplete?)))




;
(define (write-code code)
  ; Função para permitir que o jogador escreva código
  (displayln (string-append "Código inserido: " code)))



; --------------------------------------------------------
; Seção : Jogo
; --------------------------------------------------------

;
(define (length-character-list character-list)
  (if (null? character-list)
      0
      (+ 1 (length-character-list (cdr character-list)))))

; Função gráfica p/ selecionar a role(JuniorDaCorte, PlenoCondês, KingSenior )
(define (display-select-role)
  (displayln "Saudações ~a, a seguir selecione a sua classe, ao qual corresponde ao nivel de dificuldade que o usuario deseja enfrentar no futuro!.")
  (newline)
  (displayln "---------------------------------------")
  (displayln "1. Junior")
  (displayln "2. Pleno")
  (displayln "3. Senior")
  (displayln "---------------------------------------")
  (newline))

; Função para escolher uma role
(define (set-role player)
  (display-select-role)
  (display "Selecione uma classe -> ")
  (flush-output)
  (player-role player)(read-line))


; Função gráfica do escolher nome.
(define (display-select-name)
  (displayln "Bem-vindo(a) à Aba de Seleção de Nome, o ponto de partida para a sua jornada extraordinária!
Neste reino digital, o nome que você escolher será mais do que uma simples etiqueta -
será a personificação da sua jornada e conquistas.")
  (newline))

; Função para escolher um nome.
(define (set-name player)
  (display-select-name)
  (display "Nome -> ")
  (flush-output)
  (player-name player)(read-line))



; Função para avançar para a próxima fase
(define (next-level level player)
  (display "Parabéns, você está indo para a fase ")
  (displayln (level-number-level level)))


(define (set-player id)
  (make-player id "" "" 1 0 1))


; Função para criar o personagem
(define (create-player id)
  (define player-1 (set-player id))
  (set-name player-1) ; Define o nome do personagem. 
  (set-role player-1) ; Define a role do personagem.


  ;; Adiciona o novo personagem à lista
  (define updated-character-list (append character-list (list player-1)))

  ;; Retorna a lista atualizada
  updated-character-list
  
  (draw-character-menu player-1)) 

  ; (define character-list (list player-1 player-2 )))







; Função para desenhar o menu dos personagens
(define (draw-character-menu player)
  (newline)
  (display "=====================================\n")
  (display " Character Menu\n")
  (display "=====================================\n")
  (display " Character |   Role   | Advance \n")
  (display "-------------------------------------\n")
  (for-each
   (lambda (player)
     (display (format " ~a  | ~a          | ~a\n"
                      (cadr player)
                      (caddr player)
                      (cadddr player))))
   player)
  (display "-------------------------------------\n"))


(define(initialize-game)
  (displayln "Seja muito bem-vindo ao desafio!"))


; Inicializa o contador de criação de personagens
(define characters-counter 0)




; Função para inicializar o jogo.
(define (initialize-create)
  ; Função para iniciar o jogo e apresentar a narrativa inicial.
  (if (= characters-counter 0)
      (displayln "Parece que você não criou nenhum personagem ainda, a seguir digite um para criarmos o seu guerreiro!! \n 1. Criar personagem")
      (displayln "Bem-vindo de volta guerreiro")
  )

  (let ((x (read-line)))
  (if (= (string->number x) 1)
        (create-player (length-character-list character-list))
        (displayln "Comando inválido!"))))






; Função que controla as seleções do menu.
(define (menu-select choose)
  (cond
    ((= (string->number choose) 1) (initialize-create))
    ((= (string->number choose) 2) (print-help))
    ((= (string->number choose) 3) (print-credits))
    ((= (string->number choose) 4) (displayln "Saindo..."))
    (else (menu-game))))

; Funão do menu do jogo
(define (menu-game)
  (displayln "***** Aventuras na Terra de Racket *****")
  (newline)
  (displayln "1. Jogar")
  (displayln "2. Ajuda")
  (displayln "3. Créditos")
  (displayln "4. Sair")
  (newline)
  (display "-> ")(menu-select (read-line)))

; Função de ajuda
(define (print-help)
  (displayln "Bem-vindo ao menu de ajuda.")
  (displayln "Você está em uma jornada para aprender Racket e se tornar um mestre programador!")
  (newline)
  (displayln "1. Explorar Tutoriais")
  (displayln "2. Consultar Dicas do Mestre")
  (displayln "3. Ler Documentação Mágica")
  (displayln "4. Voltar à Aventura")
  (newline)
  (let ((x (read-line)))
    (if (= (string->number x) 4)
        (menu-game)
        (displayln "Comando inválido!"))))

(define (print-credits)
  (displayln "Seja muito bem vindo aos créditos.")
  (displayln "1. Voltar à Aventura")
  (newline)
  (let ((x (read-line)))
    (if (= (string->number x) 1)
        (menu-game)
        (displayln "Comando inválido!"))))





; -------------------------------------------------------------------------------------------------------
; 3. Definindo o personagem
; -------------------------------------------------------------------------------------------------------

(define (make-player id name role level experience advance)
  (list 'player
        (cons 'id id)
        (cons 'name name)
        (cons 'role role)
        (cons 'level level)
        (cons 'experience experience)
        (cons 'advance advance)))


(define player-1 (make-player 1 "Andrei" "Junior" 1 0 1))
(define player-2 (make-player 2 "ZeroCool" "Senior" 1 0 1))
(define player-3 (make-player 3 "NullCipherr" "Junior" 1 0 1))

(define level-1(level 1 "Level 1" "Comparação" 50 true))


(menu-game)

; (get-name player-1)

; (draw-character-menu character-list)

; (level-up player-1)

  
; (increment-experience level-1 player-1)