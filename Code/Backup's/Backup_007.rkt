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
(struct level (number-level name concept answer reward-experience asnwer-correct-message) #:transparent)


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



; --------------------------------------------------------
; Seção : Desafio
; --------------------------------------------------------


(define (set-difficulty difficulty)
  ;(display (format "A dificuldade foi setada em ~a \n" difficulty))
  (if (equal? difficulty "Easy")
        "Easy"
        (if (equal? difficulty "Medium")
            "Medium"
            (if (equal? difficulty "Hard")
                "Hard"
                (error "Comando inválido!")))))



; --------------------------------------------------------
; Seção : Jogo
; --------------------------------------------------------


(define (display-select-difficulty name)
  (define border " ==============================================================================")
  (define empty-space "                                ")

  (newline)
  (displayln border)
  (newline)
  (displayln (format "      Saudações ~a, a seguir selecione o seu nível de dificuldade, ao   " name))
  (displayln "    qual corresponderá aos desafios que você deseja enfrentar no futuro!  ")
  (newline)
  (displayln border)
  (displayln (format "~a 1. Easy" empty-space))
  (displayln (format "~a 2. Medium" empty-space))
  (displayln (format "~a 3. Hard" empty-space))
  (display border)
  (newline))



; 
(define (verify-difficulty-number difficulty)
  (cond [(equal? difficulty "Easy") (set-difficulty difficulty)]
        [(equal? difficulty "Medium") (set-difficulty difficulty)]
        [(equal? difficulty "Hard") (set-difficulty difficulty)]
        [(= (string->number difficulty) 1) (verify-difficulty-number "Easy")]
        [(= (string->number difficulty) 2) (verify-difficulty-number "Medium")]
        [(= (string->number difficulty) 3) (verify-difficulty-number "Hard")]
        [else (error "Dificuldade inválida!")]))


; ------------------------------------------------------------------------------------------------------------------- ;

; Função gráfica do escolher nome.
; Assinatura: display-select-name : -> void
; Propósito: Exibe uma mensagem de boas-vindas e instruções para a seleção de um nome.
(define (display-select-name)
  (newline)
  (write "  Bem-vindo(a) à Aba de Seleção de Nome, o ponto de partida para a sua jornada extraordinária!
 Neste reino digital, o nome que você escolher será mais do que uma simples etiqueta, será a personificação da sua jornada e conquistas.")
  (newline))



; Função para escolher um nome.
; Assinatura: set-name : -> string
; Propósito: Solicita ao usuário que escolha um nome e retorna a entrada do usuário como uma string.
(define (set-name)
  (display-select-name)
  (display "\n Nome -> ")
  (flush-output)
  (read-line))



; Função para selecionar as opções no menu do persnagem.
; Assinatura: start-game : Player -> void
; Propósito: Exibe um menu de opções para o jogador e executa a ação correspondente à escolha do jogador.
(define (start-game player)
  (newline)
  (display (format " Bem Vindo ~a, \n
             1. Começar estágios
             2. Mostrar estágios desbloqueados
             3. Mostrar Achivements \n\n" (player-name player)))
  (display " -> ")
  (let ((option (read-line)))
    (cond
      ((= (string->number option) 1) (initialize-game player))
      (else (start-game player)))))


; Função para criar um novo personagem.
; Assinatura: (create-player) -> void
; Propósito: Cria um novo personagem com 'id', nome, dificuldade, nível, experiência e avanço inicial.
(define (create-player)
  (define id
    (if (= character-counter 0) 1
        (error "Você já criou o seu personagem!!")))

  (display-select-name)
  (display "\n Nome -> ")
  (let ((name (read-line)))

  (display-select-difficulty name)
  (display "\n Difficulty -> ")
  (let* ((difficulty_temp (read-line))
         (difficulty (verify-difficulty-number difficulty_temp))
         (advance 1) ; Independente da dificuldade, o player começará no stage=1, de sua respectiva dificuldade.
         (level 1)
         (experience 0)
         (character-1 (make-player id name difficulty level experience advance)))
    
    (draw-player-hud character-1)
    (start-game character-1))))





; Assinatura: (define (notify-level-up level) 
;     -> void
; Propósito: Exibe uma mensagem de parabéns quando um usuário avança para um novo nível.
; Parâmetros:
;   - level: O nível para o qual o usuário avançou.
(define (notify-level-up level)
  ; border: String que representa uma linha de separação visual para destacar a mensagem.
  (define border " *************************************************** ")
  
  ; Nova linha para melhor formatação da saída.
  (newline)
  
  ; Exibe a linha de separação superior.
  (displayln border)
  
  ; Exibe a mensagem de parabéns com o nível alcançado.
  (displayln (format "  Parabéns!! Você avançou para o nível ~a" level))
  
  ; Exibe a linha de separação inferior.
  (displayln border)
  
  ; Nova linha para melhor formatação da saída.
  (newline))





; Função para verificar se o jogador pode subir de nível com base na experiência acumulada.
; Assinatura: verify-level-up : string number -> boolean
; Propósito: Verifica se o jogador atingiu a quantidade de experiência necessária para subir de nível, com base na dificuldade.
(define (verify-level-up difficulty experience)
  (cond
    [(string=? difficulty "Easy") (>= experience 200)]
    [(string=? difficulty "Medium") (>= experience 400)]
    [(string=? difficulty "Hard") (>= experience 600)]
    ))





; Função para obter a próxima dificuldade com base na dificuldade atual.
; Assinatura: next-difficulty : string -> string
; Propósito: Retorna a próxima dificuldade, considerando que a progressão é de Easy para Medium e de Medium para Hard.
(define (next-difficulty current-difficulty)
  (cond
    [(equal? current-difficulty "Easy") "Medium"]
    [(equal? current-difficulty "Medium") "Hard"]
    [else "Hard"])) ; Não muda a dificuldade além de 'Hard'





; Função para salvar o progresso do jogador após completar um nível.
; Assinatura: (save-progress player level) : Player Level -> void
; Propósito: Atualiza o jogador com base no progresso do nível concluído, incluindo subir de nível, mudar de dificuldade, etc.
(define (save-progress player level)
  (define new-name (player-name player))
  (define new-id (player-id player))
  (define new-experience (+ (player-experience player) (level-reward-experience level)))

  (cond 
    [(verify-level-up (player-difficulty player) new-experience)
     (let* ((reset-experience 0)
            (new-difficulty (next-difficulty (player-difficulty player)))
            (new-level (+ (player-level player) 1))
            (new-stage 1)
            (new-character (make-player new-id new-name new-difficulty new-level reset-experience new-stage)))
       (notify-level-up new-level)
       (draw-player-hud new-character)
       (select-level new-character new-difficulty new-stage))]
    
    [else 
     (let* ((new-difficulty (player-difficulty player))
            (new-level (player-level player))
            (new-stage (+ (player-advance player) 1))
            (new-character (make-player new-id new-name new-difficulty new-level new-experience new-stage)))
       (draw-player-hud new-character)
       (select-level new-character new-difficulty new-stage))]))




; Assinatura: normalize-answer : String -> String
; Propósito: Normaliza uma resposta, removendo espaços em branco e convertendo para minúsculas.
; Parâmetros:
;   - answer: A resposta a ser normalizada.
(define (normalize-answer answer)
  (string-downcase (string-trim answer)))




; Função para iniciar um nível do jogo.
; Assinatura: start-level : Level Player -> void
; Propósito: Exibe informações sobre o nível atual, solicita a resposta do jogador e executa ações com base na resposta.
(define (start-level level player)
  (define border " ------------------------------------------------------------------------")
  
  (displayln border)
  (displayln (format " Nível ~a: ~a" (level-number-level level) (level-name level)))
  (newline)
  (write (level-concept level))
  (newline)
  (newline)
  (displayln (format " Recompensa de Experiência: ~a" (level-reward-experience level)))
  (displayln border)

  (display " Digite a sua resposta: ")
  (flush-output)
  (let ((user-answer (read-line)))
    (if (string=? user-answer (level-answer level))
        (begin
          (newline)
          (write (level-asnwer-correct-message level))
          (next-stage player level)       
          (select-level player (player-difficulty player) (+ (player-advance player) 1)))
        (begin
          (cond
            [(has-uppercase? user-answer) (displayln " \n ATENÇÃO -> A resposta contém letras maiúsculas!! ")]
            [(not(has-matching-parentheses? user-answer))(displayln " \n ATENÇÃO -> A resposta contém erro de abertura e fechamento de parentêses!! ")])
          (incorrect-answer)
          (select-level player (player-difficulty player) (player-advance player))))))




; Função para verificar se cada abertura de parênteses tem um correspondente fechamento na string.
; Assinatura: has-matching-parentheses? : string -> boolean
; Propósito: Retorna verdadeiro se cada abertura de parênteses tiver um correspondente fechamento na string, senão, falso.
(define (has-matching-parentheses? str)
  (define (check-parentheses str count)
    (cond
      [(= count -1) #f]               ; Mais fechamentos do que aberturas
      [(= (string-length str) 0) (= count 0)] ; Contagem final deve ser zero
      [(char=? (string-ref str 0) #\() (check-parentheses (substring str 1) (+ count 1))]
      [(char=? (string-ref str 0) #\)) (check-parentheses (substring str 1) (- count 1))]
      [else (check-parentheses (substring str 1) count)]))
  (check-parentheses str 0))





; Função para avançar para o próximo estágio após uma resposta correta.
; Assinatura: next-stage : Player Level -> void
; Propósito: Exibe mensagens e solicita ação do jogador para continuar para o próximo estágio ou repetir o nível.
(define (next-stage player level)
  (newline)
  (newline)
  (displayln "Digite 1 para continuar")
  (display " -> ")
  (let ((x (read-line)))
    (if (= (string->number x) 1)
        (save-progress player level)
        (next-stage player level))))





; Função para lidar com uma resposta incorreta.
; Assinatura: incorrect-answer : -> void
; Propósito: Exibe mensagens de resposta incorreta e solicita ação do jogador para continuar.
(define (incorrect-answer)
  (newline)
  (displayln " Resposta incorreta! Vamos tentar novamente...")
  (newline)

  (displayln " 1. Tentar Novamente")
  (newline)
  (display " -> ")
  (let ((x (read-line)))
    (if (= (string->number x) 1)
        1
        2
        )))





; Função para verificar se uma string contém letras maiúsculas.
; Assinatura: has-uppercase? : string -> boolean
; Propósito: Retorna verdadeiro se a string contiver pelo menos uma letra maiúscula, senão, falso.
(define (has-uppercase? str)
  (regexp-match? #rx"[A-Z]" str))




; Assinatura: select-level : Player String Int -> void
; Propósito: Função que inicializa o nível atual do personagem com base na dificuldade e estágio fornecidos.
; Parâmetros:
;   - player: Estrutura Player contendo as informações do jogador.
;   - difficulty: String representando a dificuldade do nível (Easy, Medium, Hard).
;   - stage: Inteiro representando o estágio do nível (1, 2, 3).
(define (select-level player difficulty stage)
  (cond
    ((equal? difficulty "Easy")
     (cond
      ((= stage 1) (start-level level-1-easy player))
      ((= stage 2) (start-level level-2-easy player))
      ((= stage 3) (start-level level-3-easy player))))

    ((equal? difficulty "Medium")
     (cond
      ((= stage 1) (start-level level-1-medium player))
      ((= stage 2) (start-level level-2-medium player))
      ((= stage 3) (start-level level-3-medium player))))

    ((equal? difficulty "Hard")
     (cond
      ((= stage 1) (start-level level-1-hard player))
      ((= stage 2) (start-level level-2-hard player))
      ((= stage 3) (start-level level-3-hard player))))))





; Assinatura: initialize-game : Player -> void
; Propósito: Função que inicializa o jogo, desenhando o HUD do jogador e iniciando o primeiro nível.
; Parâmetros:
;   - player: Estrutura Player contendo as informações do jogador.
(define (initialize-game player)
  (draw-player-hud player)
  (select-level player (player-difficulty player) (player-advance player)))





; Assinatura: initialize-create : -> void
; Propósito: Função para inicializar a criação do jogo, permitindo ao jogador criar um novo personagem.
; A função apresenta a narrativa inicial e oferece a opção de criar um novo personagem se nenhum personagem foi criado anteriormente.
(define (initialize-create)
  (newline)
  ; Verifica se é a primeira vez que o jogador está criando um personagem.
  (if (= character-counter 0)
      (write "Parece que você não criou nenhum personagem ainda. A seguir, selecione a opção para criarmos o seu guerreiro!!")
      (displayln "\nBem-vindo de volta, guerreiro.")
  )
  (newline)
  (newline)
  (write " 1. Criar personagem")
  (newline)
  (newline)
  (sleep 0.1)
  (display "-> ")
  (let ((x (read-line)))
    (if (= (string->number x) 1)
        (create-player)
        (initialize-create))))





; Assinatura: menu-select : -> void
; Propósito: Função que controla as seleções do menu, permitindo ao jogador escolher entre as ações disponíveis.
; Parâmetros:
;   - choose: Inteiro de entrada que representa a opção escolhida pelo jogador (ex: 1, 2, 3, 4).
(define (menu-select)
  (display "-> ")
  (let ((choose (read-line)))
    (cond
      ((= (string->number choose) 1) (initialize-create))
      ((= (string->number choose) 2) (print-help))
      ((= (string->number choose) 3) (print-credits))
      ((= (string->number choose) 4) (error "Saindo..."))
      (else (menu-game)))))





; Assinatura: menu-game : -> void
; Propósito: Função do menu principal do jogo, exibindo opções para o jogador escolher.
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





; Assinatura: print-help : -> void
; Propósito: Função que exibe o menu de ajuda, fornecendo opções relacionadas a tutoriais e dicas.
(define (print-help)
  (newline)
  (displayln " =========================================")
  (displayln "         Bem-vindo ao menu de ajuda       ")
  (displayln " =========================================")
  (displayln " Você está em uma jornada para aprender Racket e se tornar um mestre programador!")
  (newline)
  (displayln "       1. Explorar Tutoriais")
  (displayln "       2. Consultar Dicas do Mestre")
  (displayln "       3. Ler Documentação Mágica")
  (displayln "       4. Voltar à Aventura")
  (newline)
  (display "-> ")
  (let ((x (read-line)))
    (if (= (string->number x) 4)
        (menu-game)
        (displayln "Comando inválido!"))))





; Assinatura: print-credits : -> void
; Propósito: Função que imprime os créditos do jogo, exibindo informações sobre o desenvolvimento e os integrantes da equipe.
(define (print-credits)
  (newline)
  (displayln "              =========================================")
  (displayln "                 Seja muito bem-vindo aos créditos.    ")
  (displayln "              =========================================")
  (newline)
  (displayln "  Aventuras na terra Racket é um jogo educativo, desenvolvido para a matéria")
  (displayln "  de Paradigma de Programação Lógica e Funcional do curso de ciência da computação")
  (displayln "  fornecido pela Universidade Estadual de Maringá.")
  (newline)
  (displayln "  A seguir, o nome e o RA de cada integrante envolvido no desenvolvimento")
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

; Assinatura: draw-player-hud : Player -> void
; Propósito: Função que desenha o HUD (Head-Up Display) do jogador, exibindo informações como nome, nível, experiência, estágio e dificuldade.
; Parâmetros:
;   - player: Estrutura Player contendo as informações do jogador.
(define (draw-player-hud player)
  (newline)
  (displayln " ----------------------------------------------------------------------------- ")
  (displayln " |          Name          Level          XP          Stage        Difficulty | ")
  (display   " ----------------------------------------------------------------------------- ")
  (newline)
  (display "          ")
  (display (player-name player))
  (display "            ")
  (display (player-level player))
  (display "             ")
  (display (player-experience player))
  (display "            ")
  (display (player-advance player))
  (display "           ")
  (display (player-difficulty player))
  (display "       ")
  (newline)
  (displayln " -----------------------------------------------------------------------------")
  (newline))




(define player-1 (make-player 1 "Andrei" "Medium" 1 200 1))


(define level-1-easy 
  (level 1 
         " Início da Jornada" 
         " Você encontra-se em uma vila pacífica, onde os aldeões estão enfrentando um desafio. Uma criatura misteriosa deixou cair uma carta com a equação mágica escrita: '(= x 5)'. Os anciãos da vila acreditam que resolver essa equação trará bênçãos e fortalecerá suas habilidades mágicas. Você aceita o desafio e prepara-se para iniciar sua jornada, enfrentando o primeiro enigma mágico." 
         "(= x 5)" 
         50 
         " Ao resolver a equação mágica '(= x 5)', você sente uma onda de energia positiva fluindo através de você. Os anciãos da vila agradecem e reconhecem suas habilidades mágicas aprimoradas. Animado com o sucesso, você segue adiante na jornada, ansioso para enfrentar mais desafios que aprimorarão ainda mais suas habilidades em Racket."))

(define level-2-easy  
  (level 2 
         " A Floresta Encantada" 
         " Após resolver o primeiro enigma, você adentra uma floresta encantada. Aqui, as árvores sussurram enigmas e os riachos guardam segredos mágicos. Um elfo sábio apresenta-lhe o desafio: 'Some 3 ao valor de y'. Decifre esta expressão e prove sua astúcia para avançar na jornada." 
         "(+ y 3)" 
         50 
         " Ao desvendar a expressão mágica '(+ y 3)', as árvores da floresta murmuram em aprovação. O elfo sábio acena com a cabeça, reconhecendo sua habilidade em manipular expressões básicas em Racket. Com a confiança renovada, você segue mais profundamente na floresta, preparado para os desafios que estão por vir."))

(define level-3-easy  
  (level 3 
         " Torre dos Magos" 
         " Você emerge da floresta em direção a uma torre majestosa. Os magos que a habitam testarão suas habilidades com a magia das funções. Eles propõem o desafio: '(define (square n) (* n n))'. Domine esta arte mágica para desbloquear o próximo nível de poder." 
         "(+ x y)" 
         100 
         ""))

(define level-1-medium
  (level 4
         " Despertar da Magia"
         " Em um reino distante, onde a magia adormecida aguarda despertar, você é convocado para resolver o enigma mágico. A mensagem perdida revela: '(if (< x 5) 'Iniciante 'Mestre)'. Os sábios do reino acreditam que desvendar este mistério trará à tona poderes mágicos ocultos. Aceite o desafio e embarque na jornada mágica, enfrentando o primeiro enigma."
         "(if (< x 5) 'Iniciante 'Mestre)"
         100
         ""))

(define level-2-medium
  (level 5
         "Travessia da Sombra Verde"
         "Após desvendar o enigma inicial, você se aventura através da Sombra Verde, uma floresta encantada envolta em mistérios. Árvores sussurram enigmas e riachos guardam segredos. Um guardião élfico apresenta o desafio: '(if (> y 0) 'Luz 'Escuridão)'. Decifre esta expressão e prove sua astúcia para avançar na jornada mágica."
         "(if (> y 0) 'Luz 'Escuridão)"
         100
         ""))

(define level-3-medium  
  (level 6 
         "Torre dos Magos" 
         "Você emerge da floresta em direção a uma torre majestosa. Os magos que a habitam testarão suas habilidades com a magia das funções. Eles propõem o desafio: '(define (square n) (* n n))'. Domine esta arte mágica para desbloquear o próximo nível de poder." 
         "(define (square n) (* n n))" 
         200 
         ""))

(define level-1-hard 
  (level 7 
         "Início da Jornada PIKA 7" 
         "Você encontra-se em uma vila pacífica, onde os aldeões estão enfrentando um desafio. Uma criatura misteriosa deixou cair uma carta com a equação mágica escrita: '(= x 5)'. Os anciãos da vila acreditam que resolver essa equação trará bênçãos e fortalecerá suas habilidades mágicas. Você aceita o desafio e prepara-se para iniciar sua jornada, enfrentando o primeiro enigma mágico." 
         "(= x 5)" 
         150 
         ""))

(define level-2-hard  
  (level 8 
         "A Floresta Encantada PIKA 8" 
         "Após resolver o primeiro enigma, você adentra uma floresta encantada. Aqui, as árvores sussurram enigmas e os riachos guardam segredos mágicos. Um elfo sábio apresenta-lhe o desafio: 'Some 3 ao valor de y'. Decifre esta expressão e prove sua astúcia para avançar na jornada." 
         "(+ y 3)" 
         150 
         ""))

(define level-3-hard  
  (level 9 
         "Torre dos Magos PIKA 9" 
         "Você emerge da floresta em direção a uma torre majestosa. Os magos que a habitam testarão suas habilidades com a magia das funções. Eles propõem o desafio: '(define (square n) (* n n))'. Domine esta arte mágica para desbloquear o próximo nível de poder." 
         "(define (square n) (* n n))" 
         300 
         ""))

; (verify-level player-1)

; (menu-game)


; (write : String -> Void)
; A função write recebe uma string como entrada e não retorna nada (Void).
; A função write imprime uma string na tela, letra por letra, com um atraso de 0.03 segundos entre cada letra, simulando o efeito de digitação de texto em tempo real.

(define (write string)
  ;; Define um tempo de atraso de 0.03 segundos entre cada caractere impresso
  (define time 0)
  ;; Converte a string em uma lista de caracteres
  (define chars (string->list string))
  ;; Define uma função auxiliar 'loop' que recebe uma lista de caracteres
  (define (loop chars)
    ;; Verifica se a lista de caracteres não está vazia
    (unless (empty? chars)
      ;; Imprime o primeiro caractere da lista
      (display (first chars))
      ;; Esvazia o buffer de saída para garantir que o caractere seja exibido imediatamente
      (flush-output)
      ;; Pausa a execução do programa pelo tempo definido
      (sleep time)
      ;; Chama a função 'loop' recursivamente com o restante da lista de caracteres
      (loop (rest chars))))
  ;; Chama a função 'loop' com a lista de caracteres
  (loop chars))

